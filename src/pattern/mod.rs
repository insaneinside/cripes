//! Graph-based pattern representations for analysis and transformation of
//! regular languages.
//!
//! Patterns are parameterized by the _kind_ of thing those patterns describe.
//! Regular expressions, for example, are usually patterns of characters,
//! i.e. `Pattern<char>`.
//!
//! By implementing Pattern<T>  builder for your

use std::any::TypeId;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::result::Result;
use std::fmt::{self,Display,Debug};

use dot;
use anymap::any::CloneAny;
use regex_syntax;
use regex_syntax::{Expr,Repeater};

use util::graph::{self,Index};

/// Trait-bounds requirements for atomic values in a pattern.
pub trait Atom: Debug + Copy + Clone + PartialOrd<Self> {}
impl<T> Atom for T where T: Debug + Copy + Clone + PartialOrd<T> {}

/// Primitive type used for node and edge indices in pattern graphs.
type GraphIndex = u32;

/// Wrapped node index.
pub type NodeId = graph::NodeIndex<u32>;
/// Wrapped edge index.
pub type EdgeId = graph::EdgeIndex<u32>;


// ================================================================
// Nodes
/** A node within a pattern's flow graph.

    Points exist immediately before and after a token has been consumed; they
    associate arbitrary actions with parse points or parser "states".
 */
#[derive(Copy,Clone,Debug)] pub struct Point {
    /// Branching depth within the pattern.
    depth: usize,

    /// Next node at the same branching depth.
    next: Option<NodeId>,

    /// Whether or not this parse point represents an accepting state.
    accept: bool
}

impl Point {
    /** Create a new parse-point with the specified branch depth.
     *
     * The caller is responsible for ensuring that the point is placed at the
     * specified depth.
     */
    pub fn new(depth: usize) -> Self {
        Point{depth: depth, next: None, accept: false}
    }

    /// Get the branching depth of this node.
    #[inline(always)]
    pub fn depth(&self) -> usize { self.depth }

    /// Check if the node represents an accepting state.
    #[inline(always)]
    pub fn is_accept(&self) -> bool { self.accept }

    /// Set whether or not the node is an accepting state.
    pub fn set_accept(&mut self, accept: bool) {
        self.accept = accept;
    }
}

impl Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <Self as Debug>::fmt(self, f)
    }
}



// ================================================================
// Edges

/// Non-consuming pattern matchers.
#[derive(Debug,Clone)]
pub enum Anchor {
    /// Beginning of the input buffer.
    StartOfInput,
    /// End of the input buffer.
    EndOfInput,
    /// Match the *FA with entry at the given node, ahead of the
    /// current position.
    LookAhead(NodeId),
    /// Match the *FA with entry at the given node, behind the
    /// current position.
    LookBehind(NodeId)
}

/// A range of atoms.
#[derive(Debug,Copy,Clone)]
pub struct ClassRange<T: Atom> {
    /// First atom in the range.
    pub first: T,
    /// Last atom in the range.
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

// ----------------------------------------------------------------

/** `Edge` specifies the conditions under which a parser is allowed to
 * transition to the linked state.
 */
#[derive(Debug,Clone)]
pub struct Edge<T: Atom> {
    /** Transition that must (not) match the input when `polarity` is
        `Polarity::NORMAL` (`Polarity::INVERTED`). */
    input: Transition<T>,

    /// Polarity of the match.
    polarity: Polarity
}

impl<T: Atom> Edge<T> {
    /// Create an edge for the given input transition with normal polarity.
    pub fn new(input: Transition<T>) -> Self {
        Edge{input: input, polarity: Polarity::NORMAL}
    }
    /// Create an edge for the given input transition with inverted polarity.
    pub fn new_inverted(input: Transition<T>) -> Self {
        Edge{input: input, polarity: Polarity::INVERTED}
    }
}

// We implement Display for Edge to provide a more readable output from Graph.
impl Display for Edge<char> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.input {
            Transition::Atom(ref x) => {
                if x.is_whitespace() || x.is_control() {
                    write!(f, "'{}'", x.escape_default().collect::<String>()) }
                else { write!(f, "'{}'", x) } },
            Transition::Sequence(ref x) => write!(f, "'{}'", String::from_iter(x.iter().cloned())),
            Transition::Wildcard => f.write_str("(any)"),
            Transition::Repeat{min, max} => {
                if let Some(max) = max { write!(f, "({}..{} times)", min, max) }
                else { write!(f, "({}+ times)", min) } },
            _ => <Self as Debug>::fmt(self, f)
        }

    }
}


/// Description of a potential transition between parser states.
#[derive(Debug,Clone)]
pub enum Transition<T: Atom> {
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
    Subgraph(NodeId),

    /** Backwards edge. The `min` (and `max`, if present) value(s) specify the
     * number of times that the edge may be followed, i.e.  */
    Repeat {
        /** Number of times the edge _must_ be followed before any other
            transitions from the source node are allowed.  May be zero, in
            which case other transitions are valid the first time the source
            node is visited. */
        min: u32,
        
        /** Maximum number of times the edge _may_ be followed.  If `None`,
         * there is no limit. */
        max: Option<u32> },
}


impl<'a,T: Atom> dot::GraphWalk<'a,NodeId,EdgeId> for Pattern<T> {
    fn nodes(&self) -> dot::Nodes<'a,NodeId> { self.graph.node_indices().collect() }
    fn edges(&self) -> dot::Edges<'a,EdgeId> { self.graph.edge_indices().collect() }
    fn source(&self, e: &EdgeId) -> NodeId {
        if let Some((id, _)) = self.graph.edge_endpoints(*e) { id }
        else { panic!("Invalid edge ID passed to `source`") }
    }
    fn target(&self, e: &EdgeId) -> NodeId {
        if let Some((_, id)) = self.graph.edge_endpoints(*e) { id }
        else { panic!("Invalid edge ID passed to `source`") }
    }
}

// impl<'a, T: Atom> dot::Labeller<'a,NodeId,EdgeId> for Pattern<T> {
//     fn graph_id(&'a self) -> dot::Id<'a> { dot::Id::new(format!("G{}", self.entry.index())).unwrap() }
//     fn node_id(&'a self, n: &NodeId) -> dot::Id<'a> { dot::Id::new(format!("N{}", n.index())).unwrap() }
//     fn edge_label(&'a self, e: &EdgeId) -> dot::LabelText<'a> {
//         let edge = &self.graph[*e];
//         dot::LabelText::LabelStr(format!("{:?}", edge).into())
//     }
// }
impl<'a> dot::Labeller<'a,NodeId,EdgeId> for Pattern<char> {
    fn graph_id(&'a self) -> dot::Id<'a> { dot::Id::new(format!("G{}", self.entry.index())).unwrap() }
    fn node_id(&'a self, n: &NodeId) -> dot::Id<'a> { dot::Id::new(format!("N{}", n.index())).unwrap() }
    fn edge_label(&'a self, e: &EdgeId) -> dot::LabelText<'a> {
        let edge = &self.graph[*e];
        dot::LabelText::LabelStr(format!("{}", edge).into())
    }
    fn node_label(&'a self, n: &NodeId) -> dot::LabelText<'a> {
        let point = &self.graph[*n];
        dot::LabelText::LabelStr(format!("{} ({})", n.index(), point.depth()).into())
    }
    fn node_shape(&'a self, node: &NodeId) -> Option<dot::LabelText<'a>> {
        let shape: &'static str =
            if self.graph[*node].is_accept() { "doublecircle" }
            else { "circle" };
        Some(dot::LabelText::LabelStr(String::from(shape).into()))
    }
}
// ================================================================
// Patterns

/// Graph-based representation of automata patterns.
#[derive(Debug,Clone)]
pub struct Pattern<T: Atom> {
    /// Graph representation of the control-flow of the pattern's automaton.
    pub graph: graph::Graph<Point,Edge<T>>,

    /// Index of the node that serves as the main entry point for the graph.
    pub entry: NodeId,

    /// Stored node analyses
    node_analyses: HashMap<(TypeId,NodeId),Box<CloneAny>>,

    /// Stored edge analyses
    edge_analyses: HashMap<(TypeId,EdgeId),Box<CloneAny>>
}

impl From<Expr> for Pattern<char> {
    fn from(e: Expr) -> Self { Pattern::build(e) }
}

impl Pattern<char> {
    /** Create a subgraph describing the given expression, and return its
     * entry and exit node IDs.
     *
     * @return Tuple containing the stand-alone graph's entry- and
     * exit-node IDs. */
    fn subgraph(g: &mut graph::Graph<Point,Edge<char>>, depth: usize, expr: Expr) -> (NodeId, NodeId) {
        let entry = g.add_node(Point::new(depth));
        Pattern::build_recursive(g, entry, None, expr)
    }

    /** Append an edge to a graph after a given node, and (optionally) before
     * a given node.  If the passed `_next` value is `None`, a node will
     * be added.
     *
     * @return ID of the target node for the new edge.
     */
    fn append_edge(g: &mut graph::Graph<Point,Edge<char>>, prev: NodeId, _next: Option<NodeId>, e: Edge<char>) -> (NodeId, NodeId) {
        let next = _next.unwrap_or_else(|| { let lvl = g[prev].depth(); g.add_node(Point::new(lvl)) });
        g.add_edge(prev, next, e);
        (prev, next)
    }


    /// Convert a 
    fn convert_class(c: regex_syntax::CharClass) -> Transition<char> {
        // The `unwrap()` below is okay because the way regex_syntax is
        // implemented, class ranges simply _cannot_ be empty.
        let first = c.iter().cloned().nth(0).unwrap();

        if c.len() > 1 || first.start != first.end {
            Transition::Class(c.into_iter().map(|cr| ClassRange { first: cr.start, last: cr.end }).collect())
        } else {
            Transition::Atom(first.start)
        }
    }

    /** Build the canonical NFA subgraph for an expression.
     *
     * @param g Graph object that should hold the expression's subgraph.
     *
     * @param prev Index of the node that serves as the entry point for
     *     the subgraph.
     *
     * @param next When not `None`, specifies index of the exit node (final
     *     state) of the expression graph after the expression's subgraph has
     *     been added.
     *
     * @param expr `regex_syntax` expression to be added to the graph.
     *
     * @return Indices of the entry and exit nodes for the appended subgraph.
     */
    fn build_recursive(g: &mut graph::Graph<Point,Edge<char>>, mut prev: NodeId, next: Option<NodeId>, expr: Expr) -> (NodeId, NodeId) {
        let new_next = match expr {
            Expr::Literal{chars, casei} =>
                // FIXME: for case-insensitive matches, we should add some sort
                // of normalization preprocessing action to the node.
                Self::append_edge(g, prev, next, Edge::new(if chars.len() > 1 { Transition::Sequence(chars) } else { Transition::Atom(chars[0])})).1,
            Expr::AnyChar => Self::append_edge(g, prev, next, Edge::new(Transition::Wildcard)).1,
            Expr::AnyCharNoNL => Self::append_edge(g, prev, next, Edge::new_inverted(Transition::Atom('\n'))).1,
            Expr::Class(c) => {
                let negated = c.clone().negate();
                // Use the smallest possible set of characters.
                Self::append_edge(g, prev, next, if negated.len() < c.len() {Edge::new_inverted(Self::convert_class(negated))} else {Edge::new(Self::convert_class(c))}).1
            },
            Expr::StartLine => unimplemented!(), /*{ let sg = Pattern::subgraph(g, Expr::parse(r"\A|\n").unwrap()).0;
                                 Self::append_edge(g, prev, next, Edge::new(Transition::Anchor(Anchor::LookBehind(sg)))).1 },*/
            Expr::EndLine => unimplemented!(), /*{ let sg = Pattern::subgraph(g, Expr::parse(r"\z|\r?\n").unwrap()).0;
                               Self::append_edge(g, prev, next, Edge::new(Transition::Anchor(Anchor::LookAhead(sg)))).1 },*/
            Expr::StartText => Self::append_edge(g, prev, next, Edge::new(Transition::Anchor(Anchor::StartOfInput))).1,
            Expr::EndText => Self::append_edge(g, prev, next, Edge::new(Transition::Anchor(Anchor::EndOfInput))).1,

            Expr::Group{e, ..} => return Pattern::build_recursive(g, prev, next, *e),

            Expr::Repeat{e, r, greedy} => {
                let (min, mut max) = match r {
                    Repeater::ZeroOrOne => (0, Some(1)),
                    Repeater::ZeroOrMore => (0, None),
                    Repeater::OneOrMore => (1, None),
                    Repeater::Range{min: mn, max: mx} => (mn, mx)
                };
                /* The minimum repetition count, times the subgraph's length,
                   gives the distance between its entry and exit nodes on the
                   main graph.

                     - For a minimum repetition count of zero the entry and
                       exit nodes are one and the same, and no additional edges
                       are required to handle repeated traversal of
                       the subgraph.

                     - For any minimum repetition count ≥ 1, we inline the
                       subgraph's first occurrence.

                       !!FIXME!!: method for specifying min/max repetition
                           count(s) is not yet implemented for the NFA graph;
                           instead we add a backwards edge (epsilon transition)
                           that specifies the number of times it may or should
                           be followed.]
                   */
                if min == 0 {
                    Pattern::build_recursive(g, prev, Some(prev), *e);
                    prev
                } else {
                    // Inline the first occurrence of the subgraph.
                    let (subgraph_entry, subgraph_exit) = Pattern::build_recursive(g, prev, next, *e);

                    /* FIXME: decide how to implement required repetitions.
                       Directly inlining more than one copy of the subgraph
                       could easily lead to excessive bloat during codegen, and
                       provides less information to optimization passes about
                       the intent (pattern repetition) of the transitions.
                     */
                    if let Some(_max) = max { max = Some(_max - 1); }
                    Self::append_edge(g, subgraph_exit, Some(prev), Edge::new(Transition::Repeat{min: min - 1, max: max}));

                    subgraph_exit
                }
            },
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
                let _next = next.unwrap_or_else(|| { let lvl = g[prev].depth(); g.add_node(Point::new(lvl)) });
                for expr in exprs {
                    Pattern::build_recursive(g, prev, Some(_next), expr);
                }
                _next
            },

            // Why would we have an empty expression?
            Expr::Empty => unreachable!(),

            // FIXME: should we implement word boundaries?
            Expr::WordBoundary => unreachable!(), /*Self::append_edge(g, prev, next, Transition::Anchor(Anchor::And(*/
            Expr::NotWordBoundary => unreachable!(),
        };
        (prev, new_next)
    }
    fn build(expr: Expr) -> Pattern<char> {
        let mut g = graph::Graph::new();
        // Build the graph describing the given expression.
        let (entry, exit) = Pattern::subgraph(&mut g, 0, expr);
        // Set the final node as an accepting state.
        g[exit].set_accept(true);


        return Pattern{graph: g, entry: entry,
                       node_analyses: HashMap::new(), edge_analyses: HashMap::new()};
    }

    /// Attempt to parse a regular expression; if successful, return the
    pub fn parse_string<S: AsRef<str>>(s: S) -> Result<Self,regex_syntax::Error> {
        match Expr::parse(s.as_ref()) {
            Ok(expr) => Ok(Pattern::build(expr)),
            Err(err) => Err(err)
        }
    }
}
