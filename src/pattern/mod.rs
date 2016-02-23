//! Graph types for pattern representation.
extern crate petgraph;
extern crate regex_syntax;
use self::regex_syntax::{Expr,Repeater};

use std::error::Error;
use std::result::Result;
use std::fmt::{self,Display,Debug};

pub mod transform;

pub type NodeID = petgraph::graph::NodeIndex<petgraph::graph::DefIndex>;
pub type EdgeID = petgraph::graph::EdgeIndex<petgraph::graph::DefIndex>;

/** A node within a grammar's flow graph.  Nodes represent points before and
 * immediately after a token has been consumed; they associate arbitrary
 * actions with parse points or parser "states". */
#[derive(Debug,Copy,Clone)]
pub enum Node<T: Debug + Copy + Clone + PartialOrd<T>> {
    /// First node in a (sub)graph.
    Entry,

    /// Nothing to do; continue the parse.
    Continue,

    /// user-specified action to be taken when a parser reaches the node.
    Action(fn(Edge<T>)),
    
    /// accept the (sub)graph and return from the parse
    Accept
}

impl<T: Debug + Copy + Clone + PartialOrd<T>>
Display for Node<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <Self as Debug>::fmt(self, f)
    }
}
impl<T: Debug + Copy + Clone + PartialOrd<T>>
Display for Edge<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <Self as Debug>::fmt(self, f)
    }
}

/// Non-consuming pattern matchers.
#[derive(Debug,Copy,Clone)]
pub enum Anchor {
    StartOfInput,
    EndOfInput,
    LookAhead(NodeID),
    LookBehind(NodeID)
}

#[derive(Debug,Copy,Clone)]
pub struct ClassRange<T: Debug + Copy + Clone + PartialOrd<T>> {
    pub first: T,
    pub last: T
}

/** Flag type used to differentiate between normal and inverting matches on
 * pattern elements. */
#[derive(Debug,Copy,Clone)]
pub enum Polarity {
    /// Normal match behavior.
    NORMAL,
    /// Invert the match result.
    INVERTED
}

/** Description of a potential transition between parser states.  `Edge`
 * specifies the conditions under which a parser is allowed to transition to
 * the linked state. */

#[derive(Debug,Clone)]
pub struct Edge<T: Debug + Copy + Clone+ PartialOrd<T>> {
    element: Element<T>,
    polarity: Polarity
}

impl<T: Debug + Copy + Clone+ PartialOrd<T>> Edge<T> {
    pub fn new(elt: Element<T>) -> Self {
        Edge{element: elt, polarity: Polarity::NORMAL}
    }
    pub fn new_complement(elt: Element<T>) -> Self {
        Edge{element: elt, polarity: Polarity::INVERTED}
    }
}

#[derive(Debug,Clone)]
/// A matchable pattern element.
pub enum Element<T: Debug + Copy + Clone+ PartialOrd<T>> {
    /// atomic literal value
    Atom(T),

    /// any of a set of elements
    Class(Vec<ClassRange<T>>),

    /** a sequence of atoms.  By directly storing all elements of a sequence
     * (instead of separating them with nodes), we can possibly apply certain
     * optimizations during codegen. */
    Sequence(Vec<T>),

    /// any atom
    Wildcard,

    /** Condition that must match the current input for pattern-matching to
     * continue */
    Anchor(Anchor),

    /** Subgraph reference (instantiation) by reference to that graph's
     * entry node. */
    Subgraph(NodeID),

    /// Arbitrary repetition.
    Closure{pattern: NodeID, min: u32, max: Option<u32>},
}

/** Graph representation of a grammar's DFA.  `Graph` is used to model the
 * DFA's control-flow and allow various transformations to be applied. */
pub type Graph<T> = petgraph::Graph<Node<T>,Edge<T>,petgraph::Directed>;


/// Any grammar type.
pub trait Grammar<T: Debug + Copy + Clone + PartialOrd<T>>
where Self: Sized {
    type ParseError: Error;
    /// Attempt to parse a `str` into an implementation instance.
    fn parse_string<S: AsRef<str>>(s: S) -> Result<Self,Self::ParseError>;
}

#[derive(Debug,Clone)]
pub struct Pattern<T: Debug + Copy + Clone + PartialOrd<T>> {
    pub graph: Graph<T>,
    entry: NodeID
}

impl Pattern<char> {
    /** Create a subgraph describing the given expression, and return its
     * entry and exit node IDs.
     *
     * @return Tuple containing the stand-alone graph's entry- and
     * exit-node IDs. */
    fn subgraph(g: &mut Graph<char>, expr: Expr) -> (NodeID, NodeID) {
        let entry = g.add_node(Node::Entry);
        Pattern::build_recursive(g, entry, None, expr)
    }

    /** Append an edge to a graph after a given node, and (optionally) before
     * a given node.  If the passed `_next` value is `None`, a new `Continue`
     * node will be added.
     *
     * @return ID of the target node for the new edge.
     */
    fn append_edge(g: &mut Graph<char>, prev: NodeID, _next: Option<NodeID>, e: Edge<char>) -> (NodeID, NodeID) { 
        let next = _next.unwrap_or_else(|| g.add_node(Node::Continue));
        g.add_edge(prev, next, e);
        (prev, next)
    }


    /** Given a root-node ID, append a regular-expression's representation to
     * a graph.
     *
     * @return ID of the exit node (final state) of the expression graph after
     * the expression has been added.
     */
    // !!!FIXME!!! ensure that `next` value returned is as documented in all
    // cases -- ensure we aren't returning a `next` that we've used as a `prev`
    // to e.g. a subgraph.
    fn build_recursive(g: &mut Graph<char>, mut prev: NodeID, next: Option<NodeID>, expr: Expr) -> (NodeID, NodeID) {
        let new_next = match expr {
            Expr::Literal{chars, casei} =>
                // FIXME: for case-insensitive matches, we should add some sort
                // of normalization preprocessing action to the node.
                Self::append_edge(g, prev, next, if chars.len() > 1 { Edge::new(Element::Sequence(chars)) } else { Edge::new(Element::Atom(chars[0]))}).1,
            Expr::AnyChar => Self::append_edge(g, prev, next, Edge::new(Element::Wildcard)).1,
            Expr::AnyCharNoNL => Self::append_edge(g, prev, next, Edge::new_complement(Element::Atom('\n'))).1,
            Expr::Class(c) => Self::append_edge(g, prev, next, Edge::new(Element::Class(c.into_iter().map(|cr| ClassRange { first: cr.start, last: cr.end }).collect()))).1,
            Expr::StartLine => { let sg = Pattern::subgraph(g, Expr::parse(r"\A|\n").unwrap()).0;
                                 Self::append_edge(g, prev, next, Edge::new(Element::Anchor(Anchor::LookBehind(sg)))).1 },
            Expr::EndLine => { let sg = Pattern::subgraph(g, Expr::parse(r"\z|\r?\n").unwrap()).0;
                               Self::append_edge(g, prev, next, Edge::new(Element::Anchor(Anchor::LookAhead(sg)))).1 },
            Expr::StartText => Self::append_edge(g, prev, next, Edge::new(Element::Anchor(Anchor::StartOfInput))).1,
            Expr::EndText => Self::append_edge(g, prev, next, Edge::new(Element::Anchor(Anchor::EndOfInput))).1,

            Expr::Group{e, ..} => return Pattern::build_recursive(g, prev, next, *e),
            Expr::Repeat{e, r, greedy} => {
                let (subgraph_entry, _) = Pattern::subgraph(g, *e);
                let (min, max) = match r {
                    Repeater::ZeroOrOne => (0, Some(1)),
                    Repeater::ZeroOrMore => (0, None),
                    Repeater::OneOrMore => (1, None),
                    Repeater::Range{min: mn, max: mx} => (mn, mx)
                };
                Self::append_edge(g, prev, next, Edge::new(Element::Closure{pattern: subgraph_entry, min: min, max: max})).1 },
            Expr::Concat(mut exprs) => {
                if exprs.is_empty() { panic!("Invalid concatenation of nothing"); }
                exprs.reverse();
                while exprs.len() > 1 {
                    let (_, _prev) = Self::build_recursive(g, prev, None, exprs.pop().unwrap());
                    prev = _prev;
                }
                Self::build_recursive(g, prev, next, exprs.pop().unwrap()).1
            },
            Expr::Alternate(exprs) => {
                let _next = next.unwrap_or_else(|| g.add_node(Node::Continue));
                for expr in exprs {
                    Pattern::build_recursive(g, prev, Some(_next), expr);
                }
                _next
            },

            // Why would we have an empty expression?
            Expr::Empty => unreachable!(),

            // FIXME: should we implement word boundaries?
            Expr::WordBoundary => unreachable!(), /*Self::append_edge(g, prev, next, Edge::Anchor(Anchor::And(*/
            Expr::NotWordBoundary => unreachable!(),
        };
        (prev, new_next)
    }
    fn build(expr: Expr) -> Pattern<char> {
        let mut g = Graph::new();
        // Build the graph describing the given expression.
        let (entry, exit) = Pattern::subgraph(&mut g, expr);
        /* Set the final node to "Accept" type.  */
        *g.node_weight_mut(exit).unwrap() = Node::Accept;


        return Pattern{graph: g, entry: entry};
    }
}

impl Grammar<char> for Pattern<char> {
    type ParseError = self::regex_syntax::Error;
    fn parse_string<S: AsRef<str>>(s: S) -> Result<Pattern<char>,Self::ParseError> {
        match Expr::parse(s.as_ref()) {
            Ok(expr) => Ok(Pattern::build(expr)),
            Err(err) => Err(err)
        }
    }
}
