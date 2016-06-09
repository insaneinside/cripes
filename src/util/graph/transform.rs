//! Utilities for graph transformations.

use std::default::Default;

use super::interface::Graph;

/// Graph-building interface.
pub trait Builder<G: Graph> {
    /// Type from which the graph is built.
    type Input;

    /// Create the entry node for the given input, returning its ID.
    fn entry_node(g: &mut G, input: &Self::Input) -> <G as Graph>::NodeId;

    /// Create the target node for the given edge data, returning the ID of the
    /// new node.
    fn target_node<E>(g: &mut G, e: &E) -> <G as Graph>::NodeId
        where (<G as Graph>::NodeId, <G as Graph>::NodeId, E): Into<<G as Graph>::Edge>;


    
    /// Recursively 
    fn build_recursive(g: &mut G,
                       prev: <G as Graph>::NodeId,
                       next: Option<<G as Graph>::NodeId>,
                       input: Self::Input) -> (<G as Graph>::NodeId,
                                               <G as Graph>::NodeId);

    /// Append an edge to a graph after a given node, and (optionally) before
    /// a given node.
    ///
    /// If the passed `_next` value is `None`, `target_node` will be used
    /// be added.
    ///
    /// @return ID of the target node for the new edge.
    fn append_edge<E>(g: &mut G,
                      prev: <G as Graph>::NodeId,
                      _next: Option<<G as Graph>::NodeId>,
                      e: E) -> (<G as Graph>::NodeId,
                                <G as Graph>::NodeId)
        where (<G as Graph>::NodeId, <G as Graph>::NodeId, E): Into<<G as Graph>::Edge> {
        let next = _next.unwrap_or_else(|| Self::target_node(g, &e));
        g.add_edge((prev, next, e).into());
        (prev, next)
    }


    /// Create a subgraph describing the given input, and return its
    /// entry and exit node IDs.
    ///
    /// @return Tuple containing the stand-alone graph's entry- and
    /// exit-node IDs.
    fn build_subgraph(g: &mut G, input: Self::Input) -> (<G as Graph>::NodeId,
                                                         <G as Graph>::NodeId) {
        let entry = Self::entry_node(g, &input);
        Self::build_recursive(g, entry, None, input)
    }
    

    /// Convert the given input into a graph.
    ///
    /// Returns the resulting graph and the entry node.
    fn build(input: Self::Input) -> (G, <G as Graph>::NodeId)
        where G: Default {
        let mut g = G::new();
        let (entry, _) = Self::build_subgraph(&mut g, input);
        (g, entry)
    }
}

/*enum Placement {
    Before,
    After
}
struct Insertion<'a,G: Graph> {
    placement: Placement,
    node: <G as Graph>::Node
}*/


/// Graph-rewriting helper.
///
/// Rewriter provides an implementation of Graph in which all `mut` methods act
/// on the output graph, while all non-`mut` methods act on the input.
pub struct Rewriter<'a,G>
    where G: Graph + Default + 'a {
    input: &'a G,
    output: G
}

impl<'a,G> Rewriter<'a,G>
    where G: Graph + Default + 'a {
    /// Create a rewriter for the given input graph.
    pub fn new(g: &'a G) -> Self {
        Rewriter{input: g, output: G::new()}
    }

    /// Finish the 
    pub fn finish(self) -> G {
        self.output
    }
}

impl<'a,G> Graph for Rewriter<'a,G>
    where G: Graph + Default + 'a {
    type NodeId = <G as Graph>::NodeId;
    type EdgeId = <G as Graph>::EdgeId;
    type Node = <G as Graph>::Node;
    type Edge = <G as Graph>::Edge;
    type NodeIdIterator = <G as Graph>::NodeIdIterator;
    type EdgeIdIterator = <G as Graph>::EdgeIdIterator;

    fn add_node<T: Into<Self::Node>>(&mut self, n: T) -> Self::NodeId {
        self.output.add_node(n)
    }

    #[inline(always)]
    fn contains_node(&self, n: Self::NodeId) -> bool {
        self.input.contains_node(n)
    }


    #[inline(always)]
    fn node(&self, n: Self::NodeId) -> &Self::Node {
        self.input.node(n)
    }

    #[inline(always)]
    fn node_mut(&mut self, n: Self::NodeId) -> &mut Self::Node {
        self.output.node_mut(n)
    }

    #[inline(always)]
    fn node_ids(&self) -> Self::NodeIdIterator {
        self.input.node_ids()
    }

    #[inline(always)]
    fn node_count(&self) -> usize {
        self.input.node_count()
    }

    #[inline(always)]
    fn add_edge<T: Into<Self::Edge>>(&mut self, e: T) -> Self::EdgeId {
        self.output.add_edge(e)
    }

    #[inline(always)]
    fn edge(&self, e: Self::EdgeId) -> &Self::Edge {
        self.input.edge(e)
    }

    #[inline(always)]
    fn contains_edge(&self, e: Self::EdgeId) -> bool {
        self.input.contains_edge(e)
    }

    #[inline(always)]
    fn edge_ids(&self) -> Self::EdgeIdIterator {
        self.input.edge_ids()
    }

    #[inline(always)]
    fn edge_count(&self) -> usize {
        self.input.edge_count()
    }
}

