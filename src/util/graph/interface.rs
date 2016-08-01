//! Interfaces (traits) for graph implementations.
//!
//! These traits define a flexible interface for working with different graph
//! implementations by liberal application of Rust's trait paradigm.
//!
//!   * [`Graph`](trait.Graph.html) describes the base interface for any
//!     abstract graph type; its methods may be used to query the *structure*
//!     of an instance without regard to the graph's directionality or any
//!     particular node and edge implementations.  Specifically, `Graph` deals
//!     only with the IDs of nodes and edges when querying connectivity.
//!
//!   * [`ConcreteGraph`](trait.ConcreteGraph.html) extends `Graph` with
//!     methods for constructive mutation of the graph by adding nodes and
//!     edges, in addition to granting access to the nodes and
//!     edges themselves.
//!
//!     - [`Node`](trait.Node.html), which bounds `ConcreteGraph`'s associated
//!       type of the same name, provides a method to directly query the IDs of
//!       edges connected to a node.
//!
//!     - [`Edge`](trait.Edge.html), which bounds `ConcreteGraph`'s associated
//!       type of the same name, provides a method to directly query the IDs of
//!       an edge's endpoints.
//!
//!   * [`DirectedGraph`](trait.DirectedGraph.html) extends `Graph` with
//!     methods for querying the IDs of incoming and outgoing edges given
//!     a particular node ID, and the source and destination node IDs given
//!     a particular edge ID, and provides a method for iterating over the ID's
//!     of a particular node's successors.
//!
//!     - [`DirectedNode`](trait.DirectedNode.html) extends `Node` with methods
//!       to retrieve iterators over the IDs of edges that originate and
//!       terminate at a node.
//!
//!     - [`DirectedEdge`](trait.DirectedEdge.html) extends `Edge` with methods
//!       to retrieve the IDs of an edge's source and target nodes.
//!
//!     An implementation of `DirectedGraph` is provided for types that
//!     implement `ConcreteGraph` with `Node` and `Edge` associated types that
//!     fulfill `DirectedNode` and `DirectedEdge`, respectively.
//!
//! When Rust gains support for Higher-Kinded Types, the various iterator types
//! used by this module's traits will become associated types &mdash; but for
//! now **iterator types are hardcoded** to match the actual implementation
//! because of the inability to specify generic lifetime parameters on
//! associated types.
//!

use std;
use std::hash::Hash;
use std::fmt::Debug;
use std::iter::{Chain,Iterator};

use super::visit::{self,Visit};
use super::iter::Successors;

/// Interface provided by node and edge identifiers.
pub trait Id: Copy + Debug + Hash + Eq + PartialEq + PartialOrd + From<usize> {
    /// Create a new Id object with the given value.
    fn new(v: usize) -> Self;

    /// Retrieve the underlying integer value of the identifier.
    fn index(&self) -> usize;
}

// ----------------------------------------------------------------

/// Any edge type used by a graph.
pub trait Edge {
    /// Type used to identify nodes connected by the edge.
    type NodeId: Id;

    /// Get the IDs of the two nodes connected by the edge.
    fn endpoints(&self) -> (Self::NodeId, Self::NodeId);
}

/// Trait implemented by directed edge types.
pub trait DirectedEdge: Edge {
    /// Get the ID of the edge's source node.
    fn source(&self) -> Self::NodeId;

    /// Get the ID of the edge's target node.
    fn target(&self) -> Self::NodeId;
}

// ----------------------------------------------------------------

/// Any node type used by a graph.
pub trait Node {
    /// Type used to identify edges connected to the node.
    type EdgeId: Id;

    // FIXME: lack of HKTs means we have to hard-code the iterator type?!
    ///// Iterator type returned by `edges`
    //type EdgeIdIterator: IntoIterator<Item=Self::EdgeId>;

    /// Get an iterator over the IDs of all edges connected to the node.
    //fn edges(&self) -> Self::EdgeIdIterator;
    fn edges(&self) -> Chain<std::slice::Iter<Self::EdgeId>,std::slice::Iter<Self::EdgeId>>;
}

/// Interface provided by undirected nodes.
pub trait UndirectedNode: Node {
    /// Record the existence of an edge between this node and some other node.
    fn add_edge(&mut self, e: Self::EdgeId);
}

/// Trait implemented by nodes in directed graphs.
pub trait DirectedNode: Node {
    ///// Iterator type returned by either `incoming_edges` or `outgoing_edges`
    //type EdgeIdIterator: IntoIterator<Item=Self::EdgeId>;

    /// Get an iterator over incoming edges connected to the node.
    ///
    /// Note that if an edge has the node's index as both its source _and_ its
    /// target, it should appear in the set of values returned by this iterator
    /// **and** that returned by the iterator obtained from `outgoing_edges`.
    //fn incoming_edges(&self) -> Self::EdgeIdIterator;
    fn incoming_edges(&self) -> std::slice::Iter<Self::EdgeId>;

    /// Get the number of incoming edges.
    fn incoming_edge_count(&self) -> usize;

    /// Get an iterator over outgoing edges connected to the node.
    ///
    /// Note that if an edge has the node's index as both its source _and_ its
    /// target, it should appear in the set of values returned by this iterator
    /// **and** that returned by the iterator obtained from `incoming_edges`.
    //fn outgoing_edges(&self) -> Self::EdgeIdIterator;
    fn outgoing_edges(&self) -> std::slice::Iter<Self::EdgeId>;

    /// Get the number of outgoing edges.
    fn outgoing_edge_count(&self) -> usize;

    /// Record the existence of an edge that terminates at this node.
    fn add_incoming_edge(&mut self, e: Self::EdgeId);

    /// Record the existence of an edge that originates from this node.
    fn add_outgoing_edge(&mut self, e: Self::EdgeId);
}

// ----------------------------------------------------------------

/// Common interface for all graph types.
pub trait Graph {
    /// Type used to identify nodes for lookup.
    type NodeId: Id;

    /// Type used to identify edges for lookup.
    type EdgeId: Id;

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

    /// Check if a node ID is valid for this graph.
    fn contains_node(&self, n: Self::NodeId) -> bool;

    /// Fetch an iterator over the IDs of all nodes in the graph.
    fn node_ids(&self) -> Self::NodeIdIterator;

    /// Count the number of nodes in the graph.
    fn node_count(&self) -> usize;

    // --------------------------------

    /// Check if an edge ID is valid for this graph.
    fn contains_edge(&self, e: Self::EdgeId) -> bool;

    /// Fetch an iterator over the IDs of all edges in the graph.
    fn edge_ids(&self) -> Self::EdgeIdIterator;

    /// Count the number of edges in the graph.
    fn edge_count(&self) -> usize;

    // --------------------------------

    /// Get an iterator over the IDs of all edges connected to a node.
    ///
    /// Note that if a node is both the source source _and_ the target for an
    /// edge, that edge's index will be returned twice by the iterator.
    ///
    /// If the given node ID is invalid, i.e. `self.contains_node(n)` is false,
    /// this call will **panic**.
    //fn edges(&self, n: Self::NodeId) -> <Self::Node as Node>::EdgeIdIter {
    fn edges(&self, n: Self::NodeId) -> Chain<std::slice::Iter<Self::EdgeId>,std::slice::Iter<Self::EdgeId>>
        where Self: ConcreteGraph
    {
        if self.contains_node(n) {
            self.node(n).edges()
        } else {
            panic!("Invalid node ID supplied to Graph::edges()");
        }
    }



    /// Fetch the IDs of an edge's source and target nodes.
    ///
    /// If the given edge ID is invalid, i.e. `self.contains_edge(e)` is false,
    /// this call should **panic**.
    fn edge_endpoints(&self, e: Self::EdgeId) -> (Self::NodeId, Self::NodeId)
        where Self: ConcreteGraph
    {
        self.edge(e).endpoints()
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

/// Graph methods that deal with concrete node and edge types.
pub trait ConcreteGraph: Graph {
    /// Concrete node type used by a graph.
    type Node: Node<EdgeId=Self::EdgeId>;

    /// Concrete edge type used by a graph.
    type Edge: Edge<NodeId=Self::NodeId>;

    /// Get an immutable reference to a specific node.
    fn node(&self, Self::NodeId) -> &Self::Node;

    /// Get a mutable reference to a specific node.
    fn node_mut(&mut self, Self::NodeId) -> &mut Self::Node;

    /// Get an immutable reference to a specific edge.
    fn edge(&self, Self::EdgeId) -> &Self::Edge;

    /// Get a mutable reference to a specific edge.
    fn edge_mut(&mut self, Self::EdgeId) -> &mut Self::Edge;

    /// Add an explicitly-specified node to the graph, returning its unique
    /// identifier.
    ///
    /// @return ID of the newly-added node.
    fn add_node<T: Into<Self::Node>>(&mut self, n: T) -> Self::NodeId;

    /// Add an explicitly-specified edge to the graph, returning its
    /// unique identifier.
    fn add_edge<T: Into<Self::Edge>>(&mut self, e: T) -> Self::EdgeId;

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
            let (a, b) = edge.endpoints();
            if ! self.contains_node(a) {
                panic!("Invalid source node ({:?}) specified for edge in argument to Graph::add_edges", a);
            } else if ! self.contains_node(b) {
                panic!("Invalid target node ({:?}) specified for edge in argument to Graph::add_edges", b);
            } else {
                self.add_edge(edge);
            }
        }
    }

}


/// Interface provided by graphs in which nodes and edges are directed.
pub trait DirectedGraph: Graph {
    /// Fetch the index of the node from which a given edge originates.
    ///
    /// If the given edge ID is invalid, i.e. `self.contains_edge(e)` is false,
    /// this call should **panic**.
    fn edge_source(&self, e: Self::EdgeId) -> Self::NodeId;

    /// Fetch the index of the node at which a given edge terminates.
    ///
    /// If the given edge ID is invalid, i.e. `self.contains_edge(e)` is false,
    /// this call should **panic**.
    fn edge_target(&self, e: Self::EdgeId) -> Self::NodeId;

    /// Fetch an iterator over the direct successors of a node.
    ///
    /// If the given node ID is invalid, i.e. `self.contains_node(n)` is false,
    /// this call should **panic**.
    fn direct_successors(&self, n: Self::NodeId) -> Successors<Self>
        where Self: Sized;

    /// Get an iterator over incoming edges connected to a node.
    ///
    /// Note that if an edge has a node's index as both its source _and_ its
    /// target, it should appear in the set of values returned by this iterator
    /// _and_ that returned by the iterator obtained from `outgoing_edges` for
    /// the same node.
    ///
    /// If the given node ID is invalid, i.e. `self.contains_node(n)` is false,
    /// this call should **panic**.
    fn incoming_edges(&self, n: Self::NodeId) -> std::slice::Iter<Self::EdgeId>;

    /// Get an iterator over outgoing edges connected to a node.
    ///
    /// Note that if an edge has a node's index as both its source _and_ its
    /// target, it should appear in the set of values returned by this iterator
    /// _and_ that returned by the iterator obtained from `incoming_edges` for
    /// the same node.
    ///
    /// If the given node ID is invalid, i.e. `self.contains_node(n)` is false,
    /// this call should **panic**.
    fn outgoing_edges(&self, n: Self::NodeId) -> std::slice::Iter<Self::EdgeId>;
}

impl<T> DirectedGraph for T
    where T: ConcreteGraph,
          T::Node: DirectedNode<EdgeId=T::EdgeId>,
          T::Edge: DirectedEdge<NodeId=T::NodeId>
{
    fn edge_source(&self, e: Self::EdgeId) -> Self::NodeId {
        self.edge(e).source()
    }

    fn edge_target(&self, e: Self::EdgeId) -> Self::NodeId {
        self.edge(e).target()
    }

    fn direct_successors(&self, n: Self::NodeId) -> Successors<Self>
        where Self: Sized {
        Successors::new(self, self.node(n).outgoing_edges())
    }

    fn incoming_edges(&self, n: Self::NodeId) -> std::slice::Iter<Self::EdgeId> {
        self.node(n).incoming_edges()
    }

    fn outgoing_edges(&self, n: Self::NodeId) -> std::slice::Iter<Self::EdgeId> {
        self.node(n).outgoing_edges()
    }

}
