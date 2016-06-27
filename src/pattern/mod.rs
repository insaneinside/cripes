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
use std::iter::{FromIterator,IntoIterator};
use std::result::Result;
use std::fmt::{self,Display,Debug};

use dot;
use anymap::any::CloneAny;
use regex_syntax;
use regex_syntax::{Expr,Repeater};

use util::graph::{self,Builder,Graph as Graphlike,Id as GraphID};

pub mod analysis;
use self::analysis::{FlowStructure,NodeAnalysis};

/// Trait-bounds requirements for atomic values in a pattern.
pub trait Atom: Debug + Copy + Clone + PartialOrd<Self> {}
impl<T> Atom for T where T: Debug + Copy + Clone + PartialOrd<T> {}

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
#[derive(Copy,Clone,Debug)]
pub struct Point {
    /// Whether or not this parse point represents an accepting state.
    accept: bool
}

impl Point {
    /** Create a new parse-point.
     */
    pub fn new() -> Self {
        Point{accept: false}
    }

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


impl<'a,T: Atom> dot::GraphWalk<'a,NodeId,EdgeId> for Graph<T> {
    fn nodes(&self) -> dot::Nodes<'a,NodeId> { self.graph.node_ids().collect() }
    fn edges(&self) -> dot::Edges<'a,EdgeId> { self.graph.edge_ids().collect() }
    fn source(&self, e: &EdgeId) -> NodeId {
        self.graph.edge_source(*e)
    }
    fn target(&self, e: &EdgeId) -> NodeId {
        self.graph.edge_target(*e)
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
impl<'a> dot::Labeller<'a,NodeId,EdgeId> for Graph<char> {
    fn graph_id(&'a self) -> dot::Id<'a> { dot::Id::new(format!("G{}", self.entry.index())).unwrap() }
    fn node_id(&'a self, n: &NodeId) -> dot::Id<'a> { dot::Id::new(format!("N{}", n.index())).unwrap() }
    fn edge_label(&'a self, e: &EdgeId) -> dot::LabelText<'a> {
        let edge = &*self.graph[*e];
        dot::LabelText::LabelStr(format!("{}", edge).into())
    }
    fn edge_style(&'a self, e: &EdgeId) -> dot::Style {
        match self.graph.edge(*e).input {
            Transition::Repeat{..} => dot::Style::Dashed,
            _ => dot::Style::Solid
        }
    }
    fn node_label(&'a self, n: &NodeId) -> dot::LabelText<'a> {
        dot::LabelText::LabelStr(format!("{} ({:?})",
                                         n.index(),
                                         <FlowStructure as NodeAnalysis<GraphImpl<char>>>::apply(&self.graph, *n))
                                 .into())
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

/// Graph type used for patterns.
pub type GraphImpl<T> = graph::WeightedGraph<Point,Edge<T>>;


/// Graph-based representation of automata patterns.
#[derive(Debug,Clone)]
pub struct Graph<T: Atom> {
    /// Graph representation of the control-flow of the pattern's automaton.
    pub graph: GraphImpl<T>,

    /// Index of the node that serves as the main entry point for the graph.
    pub entry: NodeId,

    /// Stored node analyses
    node_analyses: HashMap<(TypeId,NodeId),Box<CloneAny>>,

    /// Stored edge analyses
    edge_analyses: HashMap<(TypeId,EdgeId),Box<CloneAny>>
}

impl From<Expr> for Graph<char> {
    fn from(e: Expr) -> Self { Self::build(e) }
}

impl<T: Atom> Graph<T> {
    pub fn rewrite<F>(&mut self, f: F)
        where F: Fn(&mut graph::Rewriter<GraphImpl<T>>) {
        self.graph = {
            let mut r = graph::Rewriter::new(&self.graph);
            f(&mut r);
            r.finish() };
    }
}



impl Graph<char> {
    /// Convert a `regex_syntax` character class to a transition.
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

    fn build(expr: Expr) -> Self {
        let mut g = GraphImpl::new();
        // Build the graph describing the given expression.
        let (entry, exit) = Self::build_subgraph(&mut g, expr);
        // Set the final node as an accepting state.
        (*g[exit]).set_accept(true);


        return Graph{graph: g, entry: entry,
                     node_analyses: HashMap::new(), edge_analyses: HashMap::new()};
    }

    /// Attempt to parse a regular expression; if successful, return the
    pub fn parse_string<S: AsRef<str>>(s: S) -> Result<Self,regex_syntax::Error> {
        match Expr::parse(s.as_ref()) {
            Ok(expr) => Ok(Self::build(expr)),
            Err(err) => Err(err)
        }
    }
}

impl Builder<GraphImpl<char>> for Graph<char> {
    type Input = Expr;

    fn entry_node(g: &mut GraphImpl<char>, _: &Expr) -> NodeId {
        g.add_node(Point::new())
    }
    fn target_node<E>(g: &mut GraphImpl<char>, _: &E) -> NodeId {
        g.add_node(Point::new())
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
    fn build_recursive(g: &mut GraphImpl<char>, mut prev: NodeId, next: Option<NodeId>, expr: Expr) -> (NodeId, NodeId) {
        let new_next = match expr {
            Expr::Literal{chars, casei} => {
                // FIXME: for case-insensitive matches, we should add some sort
                // of normalization preprocessing action to the node.
                if casei { panic!("Case-insensitive matching is not yet supported") }
                Self::append_edge(g, prev, next, Edge::new(if chars.len() > 1 { Transition::Sequence(chars) } else { Transition::Atom(chars[0])})).1
            },
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

            Expr::Group{e, ..} => return Self::build_recursive(g, prev, next, *e),

            Expr::Repeat{e, r, greedy} => {
                if ! greedy { panic!("Non-greedy repetition is not yet supported"); }
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
                    Self::build_recursive(g, prev, Some(prev), *e);
                    prev
                } else {
                    // Inline the first occurrence of the subgraph.
                    let (_, subgraph_exit) = Self::build_recursive(g, prev, next, *e);

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
                let _next = next.unwrap_or_else(|| g.add_node(Point::new()) );
                for expr in exprs {
                    Self::build_recursive(g, prev, Some(_next), expr);
                }
                _next
            },

            // An empty expression is simply an espilon transition to the
            // current node.  If a `next` node is provided, we'll use
            // that instead.
            Expr::Empty => next.unwrap_or_else(|| prev),

            // FIXME: should we implement word boundaries?
            Expr::WordBoundary => unreachable!(), /*Self::append_edge(g, prev, next, Transition::Anchor(Anchor::And(*/
            Expr::NotWordBoundary => unreachable!(),
            _ => unimplemented!()
        };
        (prev, new_next)
    }
}
