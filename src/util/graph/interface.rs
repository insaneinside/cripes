//! Interfaces (traits) for graph implementations.

use std;
use std::fmt::Debug;
use std::iter::{Chain,Iterator,IntoIterator};

use super::visit::{self,Visit};
use super::iter::Successors;

/// Interface provided by node and edge identifiers.
pub trait Id: Copy + Debug + PartialEq + PartialOrd {
    /// Create a new Id object with the given value.
    fn new(v: usize) -> Self;

    /// Retrieve the underlying integer value of the identifier.
    fn index(&self) -> usize;
}

/// Any directed edge type used by a graph.
pub trait Edge: Debug {
    /// Type used to identify source and destination nodes.
    type NodeId: Id;

    /// Get index of the edge's source node.
    fn source(&self) -> Self::NodeId;

    /// Get index of the edge's target node.
    fn target(&self) -> Self::NodeId;
}

/// Any node type used by a graph.
pub trait Node: Debug {
    /// Type used to identify incoming and outgoing edges.
    type EdgeId: Id;

    /// Get an iterator over all (incoming + outgoing) edges connected to
    /// the node.
    fn edges(&self) -> Chain<std::slice::Iter<Self::EdgeId>, std::slice::Iter<Self::EdgeId>> {
        self.incoming_edges().into_iter().chain(self.outgoing_edges().into_iter())
    }

    /// Get an iterator over incoming edges connected to the node.
    ///
    /// Note that if an edge has the node's index as both its source _and_ its
    /// target, it should appear in the set of values returned by this iterator
    /// **and** that returned by the iterator obtained from `outgoing_edges`.
    fn incoming_edges(&self) -> std::slice::Iter<Self::EdgeId>;

    /// Get the number of incoming edges.
    fn incoming_edge_count(&self) -> usize;

    /// Get an iterator over outgoing edges connected to the node.
    ///
    /// Note that if an edge has the node's index as both its source _and_ its
    /// target, it should appear in the set of values returned by this iterator
    /// **and** that returned by the iterator obtained from `incoming_edges`.
    fn outgoing_edges(&self) -> std::slice::Iter<Self::EdgeId>;

    /// Get the number of outgoing edges.
    fn outgoing_edge_count(&self) -> usize;

    /// Record the existence of an edge that terminates at this node.
    fn add_incoming_edge(&mut self, e: Self::EdgeId);

    /// Record the existence of an edge that originates from this node.
    fn add_outgoing_edge(&mut self, e: Self::EdgeId);
}



/// Common interface for all graph types.
pub trait Graph {
    /// Type used to identify nodes for lookup.
    type NodeId: Id;

    /// Type used to identify edges for lookup.
    type EdgeId: Id;

    /// Concrete node type used by a graph.
    type Node: Node<EdgeId=Self::EdgeId>;

    /// Concrete edge type used by a graph.
    type Edge: Edge<NodeId=Self::NodeId>;

    /// Iterator returned from `node_ids`.
    type NodeIdIterator: Iterator<Item=Self::NodeId>;

    /// Iterator returned from `edge_ids`.
    type EdgeIdIterator: Iterator<Item=Self::EdgeId>;

    // --------------------------------

    /// Create a new, empty instance of this graph type.
    fn new() -> Self
        where Self: Sized + Default {
        Default::default()
    }

    // --------------------------------

    /// Add an explicitly-specified node to the graph, returning its unique
    /// identifier.
    fn add_node<T: Into<Self::Node>>(&mut self, n: T) -> Self::NodeId;

    /// Fetch a reference to a node.
    ///
    /// If the given node ID is invalid, i.e. `self.contains_node(n)` is false,
    /// this call should **panic**.
    fn node(&self, n: Self::NodeId) -> &Self::Node;

    /// Fetch a mutable reference to a node.
    ///
    /// If the given node ID is invalid, i.e. `self.contains_node(n)` is false,
    /// this call should **panic**.
    fn node_mut(&mut self, n: Self::NodeId) -> &mut Self::Node;

    /// Check if a node ID is valid for this graph.
    fn contains_node(&self, n: Self::NodeId) -> bool;

    /// Fetch an iterator over the IDs of all nodes in the graph.
    fn node_ids(&self) -> Self::NodeIdIterator;

    /// Count the number of nodes in the graph.
    fn node_count(&self) -> usize;

    // --------------------------------

    /// Add an explicitly-specified edge to the graph, returning its
    /// unique identifier.
    fn add_edge<T: Into<Self::Edge>>(&mut self, e: T) -> Self::EdgeId;

    /// Fetch a reference to an edge.
    ///
    /// If the given node ID is invalid, i.e. `self.contains_node(n)` is false,
    /// this call should **panic**.
    fn edge(&self, e: Self::EdgeId) -> &Self::Edge;

    /// Check if an edge ID is valid for this graph.
    fn contains_edge(&self, e: Self::EdgeId) -> bool;

    /// Fetch an iterator over the IDs of all edges in the graph.
    fn edge_ids(&self) -> Self::EdgeIdIterator;

    /// Count the number of edges in the graph.
    fn edge_count(&self) -> usize;

    // --------------------------------

    /// Fetch an iterator over the direct successors of a node.
    ///
    /// If the given node ID is invalid, i.e. `self.contains_node(n)` is false,
    /// this call should **panic**.
    fn direct_successors(&self, n: Self::NodeId) -> Successors<Self>
        where Self: Sized {
        Successors::new(self, self.node(n).outgoing_edges())
    }

    /// Get an iterator over incoming edges connected to a node.
    ///
    /// Note that if an edge has a node's index as both its source _and_ its
    /// target, it should appear in the set of values returned by this iterator
    /// _and_ that returned by the iterator obtained from `outgoing_edges` for
    /// the same node.
    ///
    /// If the given node ID is invalid, i.e. `self.contains_node(n)` is false,
    /// this call should **panic**.
    fn incoming_edges(&self, n: Self::NodeId) -> std::slice::Iter<Self::EdgeId> {
        self.node(n).incoming_edges()
    }

    /// Get an iterator over outgoing edges connected to a node.
    ///
    /// Note that if an edge has a node's index as both its source _and_ its
    /// target, it should appear in the set of values returned by this iterator
    /// _and_ that returned by the iterator obtained from `incoming_edges` for
    /// the same node.
    ///
    /// If the given node ID is invalid, i.e. `self.contains_node(n)` is false,
    /// this call should **panic**.
    fn outgoing_edges(&self, n: Self::NodeId) -> std::slice::Iter<Self::EdgeId> {
        self.node(n).outgoing_edges()
    }

    /// Get an iterator over all edges connected to a node.
    ///
    /// Note that if a node is both the source source _and_ the target for an
    /// edge, that edge's index will be returned twice by the iterator.
    ///
    /// If the given node ID is invalid, i.e. `self.contains_node(n)` is false,
    /// this call will **panic**.
    fn edges(&self, n: Self::NodeId) -> Chain<std::slice::Iter<Self::EdgeId>,
                                              std::slice::Iter<Self::EdgeId>> {
        if self.contains_node(n) {
            self.node(n).edges()
        } else {
            panic!("Invalid node ID supplied to Graph::edges()");
        }
    }


    /// Convert all elements in a slice to edges, and add them to the graph.
    ///
    /// Each element of the slice can be any type convertible to an edge using
    /// `std::convert::From`; at the time of writing this means one of two
    /// tuple forms:
    ///
    ///   * `(source_node_index, target_node_index, payload)`, where the edge
    ///     is fully specified as arguments to `add_edge`, or
    ///   * `(source_node_index, target_node_index)`, where the edge's payload
    ///     is created using the `Default` trait.
    ///
    /// ```rust
    /// use cripes::util::graph::*;
    ///
    /// # fn main() {
    /// let mut g = WeightedGraph::<(),(),u8>::new();
    /// let n0 = g.add_node(());
    /// let n1 = g.add_node(());
    /// let n2 = g.add_node(());
    /// let n3 = g.add_node(());
    ///
    /// g.add_edges(&[(n0, n1), (n1, n2), (n1, n3), (n2, n3), (n3, n2)]);
    ///
    /// # }
    /// ```
    ///
    /// If any endpoint refers to a node that does not exist, this call will
    /// **panic**.
    fn add_edges<'a, D>(&mut self, edges: &'a [D]) where D: 'a, Self::Edge: From<&'a D> {
        for e in edges.iter() {
            let edge = Self::Edge::from(e);
            if ! self.contains_node(edge.source()) {
                panic!("Invalid source node ({:?}) specified for edge in argument to Graph::add_edges",
                       edge.source());
            } else if ! self.contains_node(edge.target()) {
                panic!("Invalid target node ({:?}) specified for edge in argument to Graph::add_edges",
                       edge.target());
            } else {
                self.add_edge(edge);
            }
        }
    }

    /// Fetch the IDs of an edge's source and target nodes.
    ///
    /// If the given edge ID is invalid, i.e. `self.contains_edge(e)` is false,
    /// this call should **panic**.
    fn edge_endpoints(&self, e: Self::EdgeId) -> (Self::NodeId, Self::NodeId) {
        let edge = self.edge(e);
        (edge.source(), edge.target())
    }


    /// Fetch the index of the node from which a given edge originates.
    ///
    /// If the given edge ID is invalid, i.e. `self.contains_edge(e)` is false,
    /// this call should **panic**.
    fn edge_source(&self, e: Self::EdgeId) -> Self::NodeId {
        self.edge(e).source()
    }

    /// Fetch the index of the node at which a given edge terminates.
    ///
    /// If the given edge ID is invalid, i.e. `self.contains_edge(e)` is false,
    /// this call should **panic**.
    fn edge_target(&self, e: Self::EdgeId) -> Self::NodeId {
        self.edge(e).target()
    }

    /*/// Iterate over the graph using a self-starting algorithm.
    ///
    /// Self-starting algorithms are those that select initial node(s) on
    /// their own.
    fn iter<A: visit::SelfStartingAlgorithm<Self>>(&self) -> visit::VisitorIter<Self, A>
        where Self: Sized {
        visit::Visitor::new::<Self, A>(self).into_iter(self)
    }*/

    /// Iterate over the graph using a rooted algorithm.
    ///
    /// Rooted algorithms are those that operate using a pre-defined root node
    /// or nodes.
    fn iter<A: visit::RootedAlgorithm<Self>>(&self, entry: Self::NodeId) -> visit::VisitorIter<Self, visit::Visitor<Self, A>>
        where Self: Sized {
        visit::Visitor::new(self, entry.into()).into_iter(self)
    }
}
