//! Directed graph types.  While these types are indeed generic with respect to
//! application, they're tailored for graph-based pattern analysis.
//!
//! We would like a graph representation that gives us both fast traversal and
//! fast analysis, and whose API allows for efficient *and* ergonomic use.
//! Naturally there is no best fit, but we can achieve a good fit using
//! a variant of the adjacency-list pattern.
//!
//! Our implementation is based on that of the `petgraph` crate.  Here,
//! however, each node contains space-optimized vectors of outgoing and
//! incoming edges.
//!
//!

use smallvec::SmallVec;
use num::{NumCast,ToPrimitive};
use std::ops;
use std::slice;
use std::hash::{Hash,Hasher};
use std::fmt::Debug;
use std::marker::PhantomData;
use std::iter::{Chain,Iterator,DoubleEndedIterator};

pub mod visit;
use self::visit::Visit;

// ================================================================
// Indices: types used to identify nodes and edges.

/// Default integer type used to identify nodes and edges.
pub type DefaultIndexType = u32;


/// Any integer type used as the underlying type for node and edge indices.
pub trait IndexType: Copy + Eq + Hash + Ord + Debug + NumCast + ToPrimitive + 'static {}
impl<T> IndexType for T where T: Copy + Eq + Hash + Ord + Debug + NumCast + ToPrimitive + 'static {}

/// Interface provided by node and edge identifiers.
pub trait Id: Copy + Debug {
    /// Create a new index object with the given value.
    fn new(v: usize) -> Self;

    /// Retrieve the underlying integer value of the identifier.
    fn index(&self) -> usize;
}


macro_rules! impl_index_wrapper_type {
    ($tp: ty, $n: ident, $T: ident) => {
        impl<$T: IndexType> Id for $tp {
            #[inline(always)]
            fn new(v: usize) -> Self { $n(NumCast::from(v).unwrap()) }
            #[inline(always)]
            fn index(&self) -> usize { NumCast::from(self.0).unwrap() }
        }

        impl<$T: IndexType> From<$T> for $tp {
            fn from(v: $T) -> Self {
                Self::new(NumCast::from(v).unwrap())
            }
        }

        impl<$T: IndexType> Hash for $tp {
            #[inline(always)]
            fn hash<H: Hasher>(&self, h: &mut H) {
                <$T as Hash>::hash(&self.0, h);
            }
        }
    }
}

/// Type used to identify and index the edges of a graph.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct EdgeIndex<T: IndexType>(T);
impl_index_wrapper_type!(EdgeIndex<T>,EdgeIndex,T);

/// Type used to identify and index the nodes of a graph.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct NodeIndex<T: IndexType>(T);
impl_index_wrapper_type!(NodeIndex<T>,NodeIndex,T);


/// Iterator over the (node or edge) indices of a graph.
#[derive(Clone)]
pub struct Indices<T: Id>(ops::Range<usize>, PhantomData<T>);
impl<T: Id> Indices<T> {
    /// Create an `Indices` iterator over the given range.
    fn new(r: ops::Range<usize>) -> Self {
        Indices(r, PhantomData)
    }
}
impl<T: Id> Iterator for Indices<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|v| Id::new(v))
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}


// ================================================================
// Nodes and edges

/// Trait bounds for types used as edge and node data payloads.
pub trait Data: Clone + Debug {}
impl<T> Data for T where T: Clone + Debug {}

// ----------------------------------------------------------------
// Edge

/// Any directed edge type used by a graph.
pub trait Edge<NodeId: Id> {
    /// Get index of the edge's source node.
    fn source(&self) -> NodeId;

    /// Get index of the edge's target node.
    fn target(&self) -> NodeId;
}

/// Weighted directed edge.
#[derive(Clone,Debug)]
pub struct WeightedEdge<T: Data, I: Id> {
    source: I,
    target: I,
    data: T
}

impl<T, I> Edge<I> for WeightedEdge<T, I>
    where T: Data, I: Id {
    #[inline(always)]
    fn source(&self) -> I { self.source }
    #[inline(always)]
    fn target(&self) -> I { self.target }
}

impl<T: Data, I: Id> WeightedEdge<T, I> {
    /// Create an edge with the given source & target node indices and
    /// weight data.
    fn new(source: I, target: I, data: T) -> Self {
        WeightedEdge{source: source, target: target, data: data}
    }
}

impl<T: Data, I: Id, I2: Copy> From<(I2, I2)> for WeightedEdge<T, I>
    where T: Default, I: From<I2> {
    fn from(u: (I2, I2)) -> Self {
        Self::new(I::from(u.0), I::from(u.1), Default::default())
    }
}

impl<T: Data, I: Id, I2: Copy> From<(I2, I2, T)> for WeightedEdge<T, I>
    where I: From<I2> {
    fn from(u: (I2, I2, T)) -> Self {
        Self::new(I::from(u.0), I::from(u.1), u.2)
    }
}


impl<'a, T: Data, I: Id, I2: Copy> From<&'a (I2, I2)> for WeightedEdge<T, I>
    where T: Default, I: From<I2> {
    fn from(u: &'a (I2, I2)) -> Self {
        Self::new(I::from(u.0), I::from(u.1), Default::default())
    }
}
impl<'a, T: Data, I: Id, I2: Copy> From<&'a (I2, I2, T)> for WeightedEdge<T, I>
    where T: Clone, I: From<I2> {
    fn from(u: &'a (I2, I2, T)) -> Self {
        Self::new(I::from(u.0), I::from(u.1), u.2.clone())
    }
}


// ----------------------------------------------------------------
// Node

/// Any node type used by a graph.
pub trait Node<EdgeId: Id> {
    /// Get an iterator over incoming edges connected to the node.
    ///
    /// Note that if an edge has the node's index as both its source _and_ its
    /// target, it should appear in the set of values returned by this iterator
    /// **and** that returned by the iterator obtained from `outgoing_edges`.
    fn incoming_edges<'a>(&'a self) -> slice::Iter<'a,EdgeId>;

    /// Get an iterator over outgoing edges connected to the node.
    ///
    /// Note that if an edge has the node's index as both its source _and_ its
    /// target, it should appear in the set of values returned by this iterator
    /// **and** that returned by the iterator obtained from `incoming_edges`.
    fn outgoing_edges<'a>(&'a self) -> slice::Iter<'a,EdgeId>;

    /// Record the existence of an edge that terminates at this node.
    fn add_incoming_edge(&mut self, e: EdgeId);

    /// Record the existence of an edge that originates from this node.
    fn add_outgoing_edge(&mut self, e: EdgeId);
}

///  Weighted node implementation.
#[derive(Clone,Debug)]
pub struct WeightedNode<T: Data, I: Id> {
    incoming_edges: SmallVec<[I; 8]>,
    outgoing_edges: SmallVec<[I; 8]>,
    data: T
}

impl<T: Data, I: Id> WeightedNode<T, I> {
    /// Instantiate a new node with the given data.
    fn new(data: T) -> Self {
        WeightedNode{incoming_edges: SmallVec::new(), outgoing_edges: SmallVec::new(), data: data}
    }

    /// Get an iterator over all (incoming + outgoing) edges connected to
    /// the node.
    fn edges<'a>(&'a self) -> Chain<slice::Iter<'a,I>,slice::Iter<'a,I>> {
        self.incoming_edges().chain(self.outgoing_edges())
    }
}

impl<T, I> Node<I> for WeightedNode<T, I>
        where T: Data, I: Id {
    fn incoming_edges<'a>(&'a self) -> slice::Iter<'a,I> {
        self.incoming_edges.iter()
    }


    fn outgoing_edges<'a>(&'a self) -> slice::Iter<'a,I> {
        self.outgoing_edges.iter()
    }


    fn add_incoming_edge(&mut self, e: I) {
        self.incoming_edges.push(e);
    }

    fn add_outgoing_edge(&mut self, e: I) {
        self.outgoing_edges.push(e);
    }
}

// ================================================================
// Graph

/// Common interface for all graph types.
pub trait Graph {
    /// Type used to identify nodes for lookup.
    type NodeId: Id;

    /// Type used to identify edges for lookup.
    type EdgeId: Id;

    /// Concrete node type used by a graph.
    type Node: Node<Self::EdgeId>;

    /// Concrete edge type used by a graph.
    type Edge: Edge<Self::NodeId>;

    // --------------------------------

    /// Add an explicitly-specified node to the graph, returning its unique
    /// identifier.
    fn add_node(&mut self, n: Self::Node) -> Self::NodeId;

    /// Check if a node ID is valid for this graph.
    fn contains_node(&self, n: Self::NodeId) -> bool;

    /// Count the number of nodes in the graph.
    fn node_count(&self) -> usize;

    /// Fetch an iterator over the direct successors of a node.
    fn direct_successors(&self, n: Self::NodeId) -> Successors<Self>
        where Self: Sized;

    // --------------------------------

    /// Add an explicitly-specified edge to the graph, returning its
    /// unique identifier.
    fn add_edge(&mut self, e: Self::Edge) -> Self::EdgeId;

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
    fn add_edges<'a, D>(&mut self, edges: &'a [D]) where D: 'a, Self::Edge: From<&'a D> {
        for e in edges.iter() {
            self.add_edge(Self::Edge::from(e));
        }
    }

    /// Check if an edge ID is valid for this graph.
    fn contains_edge(&self, e: Self::EdgeId) -> bool;

    /// Fetch the IDs of an edge's source and target nodes.
    ///
    /// @return A tuple containing the requested indices, or `None` if the
    ///     specified edge ID is invalid.
    fn edge_endpoints(&self, e: Self::EdgeId) -> Option<(Self::NodeId, Self::NodeId)>;

    /// Fetch the index of the node from which a given edge originates.
    fn edge_source(&self, e: Self::EdgeId) -> Option<Self::NodeId>;

    /// Fetch the index of the node at which a given edge terminates.
    fn edge_target(&self, e: Self::EdgeId) -> Option<Self::NodeId>;

    /// Count the number of edges in the graph.
    fn edge_count(&self) -> usize;

    /// Iterate over the graph using a rooted algorithm.
    ///
    /// Rooted algorithms are those that operate using a pre-defined root node
    /// or nodes.
    fn iter<A: visit::RootedAlgorithm<Self>>(&self, entry: Self::NodeId) -> visit::VisitorIter<Self, visit::Visitor<Self, A>>
        where Self: Sized {
        visit::Visitor::new(self, entry.into()).into_iter(self)
    }
}

/// Iterator over some node's direct successors in a particular graph.
pub struct Successors<'a, G: Graph>
    where G: 'a, <G as Graph>::EdgeId: 'a {
    graph: &'a G,
    iter: slice::Iter<'a, <G as Graph>::EdgeId>
}
impl<'a,G: 'a + Graph> Iterator for Successors<'a,G> {
    type Item = <G as Graph>::NodeId;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|id| self.graph.edge_target(*id).unwrap())
    }
}

impl<'a,G: 'a + Graph> DoubleEndedIterator for Successors<'a,G> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.next_back().map(|id| self.graph.edge_target(*id).unwrap())
    }
}


/// Directed graph with user-defined node and edge weights.
#[derive(Clone,Debug)]
pub struct WeightedGraph<N: Data, E: Data, Ix: IndexType = DefaultIndexType> {
    nodes: Vec<WeightedNode<N, EdgeIndex<Ix>>>,
    edges: Vec<WeightedEdge<E, NodeIndex<Ix>>>
}

impl<N, E, Ix> Graph for WeightedGraph<N, E, Ix>
    where N: Data, E: Data, Ix: IndexType {
    type NodeId = NodeIndex<Ix>;
    type EdgeId = EdgeIndex<Ix>;
    type Node = WeightedNode<N, Self::EdgeId>;
    type Edge = WeightedEdge<E, Self::NodeId>;

    fn add_node(&mut self, n: Self::Node) -> Self::NodeId {
        self.nodes.push(n);
        NodeIndex::new(self.nodes.len() - 1)
    }

    fn direct_successors(&self, n: Self::NodeId) -> Successors<Self> {
        Successors{graph: self, iter: self.nodes[n.index()].outgoing_edges()}
    }

    fn contains_node(&self, n: Self::NodeId) -> bool {
        n.index() < self.nodes.len()
    }

    fn node_count(&self) -> usize {
        self.nodes.len()
    }

    fn add_edge(&mut self, e: Self::Edge) -> Self::EdgeId {
        let srcidx = e.source();
        let destidx = e.target();

        self.edges.push(e);
        let index = EdgeIndex::new(self.edges.len() - 1);

        // Update cross-referenced edge lists for the connected nodes.
        self.nodes[srcidx.index()].add_outgoing_edge(index);
        self.nodes[destidx.index()].add_incoming_edge(index);

        index
    }

    fn contains_edge(&self, e: Self::EdgeId) -> bool {
        e.index() < self.edges.len()
    }

    fn edge_source(&self, e: EdgeIndex<Ix>) -> Option<NodeIndex<Ix>> {
        if e.index() < self.edges.len() { Some(self.edges[e.index()].source) }
        else { None }
    }

    fn edge_target(&self, e: EdgeIndex<Ix>) -> Option<NodeIndex<Ix>> {
        if e.index() < self.edges.len() { Some(self.edges[e.index()].target) }
        else { None }
    }

    fn edge_endpoints(&self, e: EdgeIndex<Ix>) -> Option<(NodeIndex<Ix>, NodeIndex<Ix>)> {
        if e.index() < self.edges.len() {
            let ref edge = self.edges[e.index()];
            Some((edge.source, edge.target))
        } else {
            None
        }
    }

    fn edge_count(&self) -> usize {
        self.edges.len()
    }
}


impl<N: Data, E: Data, Ix: IndexType> WeightedGraph<N, E, Ix> {
    /// Create a new, empty graph.
    pub fn new() -> Self {
        WeightedGraph{nodes: Vec::new(), edges: Vec::new()}
    }

    /// Add a node to the graph.
    ///
    /// @return Index of the newly-added node.
    pub fn add_node(&mut self, data: N) -> NodeIndex<Ix> {
        <Self as Graph>::add_node(self, <Self as Graph>::Node::new(data))
    }

    /// Fetch an iterator over the indices of all nodes in the graph.
    pub fn node_indices(&self) -> Indices<NodeIndex<Ix>> {
        Indices::new(0..self.nodes.len())
    }

    /// Get an iterator over all edges connected to a node.
    ///
    /// Note that if a node is both the source source _and_ the target for an
    /// edge, that edge's index will be returned twice by the iterator.
    pub fn all_edges<'a>(&'a self, n: NodeIndex<Ix>) -> Chain<slice::Iter<'a,EdgeIndex<Ix>>,slice::Iter<'a,EdgeIndex<Ix>>> where N: 'a {
        self.nodes[n.index()].edges()
    }

    /// Get an iterator over incoming edges connected to a node.
    ///
    /// Note that if an edge has a node's index as both its source _and_ its
    /// target, it should appear in the set of values returned by this iterator
    /// _and_ that returned by the iterator obtained from `outgoing_edges` for
    /// the same node.
    pub fn incoming_edges<'a>(&'a self, n: NodeIndex<Ix>) -> slice::Iter<'a,EdgeIndex<Ix>> where N: 'a {
        self.nodes[n.index()].incoming_edges()
    }


    /// Get an iterator over outgoing edges connected to a node.
    ///
    /// Note that if an edge has a node's index as both its source _and_ its
    /// target, it should appear in the set of values returned by this iterator
    /// _and_ that returned by the iterator obtained from `incoming_edges` for
    /// the same node.
    pub fn outgoing_edges<'a>(&'a self, n: NodeIndex<Ix>) -> slice::Iter<'a,EdgeIndex<Ix>> where N: 'a {
        self.nodes[n.index()].outgoing_edges()
    }

    /// Add an edge to the graph.
    ///
    /// The source and target nodes, as identified by calling `edge.source()`
    /// and `edge.target()` on the edge, will be updated as appropriate.
    ///
    /// **Panics** if either of the source or target node indices are invalid.
    ///
    /// @return Index of the newly-added edge.
    ///
    /// @fixme Is there any way we can make this operation atomic, or at least
    /// less drawn-out?
    pub fn add_edge(&mut self, source: NodeIndex<Ix>, target: NodeIndex<Ix>, data: E) -> EdgeIndex<Ix> {
        panic_unless!(source.index() < self.nodes.len(), "Invalid source-node index provided to `Graph::add_edge`");
        panic_unless!(target.index() < self.nodes.len(), "Invalid target-node index provided to `Graph::add_edge`");

        // Add the edge itself
        self.edges.push(<Self as Graph>::Edge::new(source, target, data));

        // ...and add cross-references to the nodes.
        let edge = EdgeIndex::new(self.edges.len() - 1);
        self.nodes[source.index()].add_outgoing_edge(edge);
        self.nodes[target.index()].add_incoming_edge(edge);

        edge
    }

    /// Fetch an iterator over the indices of all edges in the graph.
    pub fn edge_indices(&self) -> Indices<EdgeIndex<Ix>> {
        Indices::new(0..self.edges.len())
    }
}

// Implement node-data lookup via indexing.
macro_rules! graph_index_impl {
    ($tp: ty, $n: ident, $e: ident, $r: ident, $m: ident) => {
        impl<$n: Data, $e: Data, Ix: IndexType> ops::Index<$tp> for WeightedGraph<N, E, Ix> {
            type Output = $r;
            fn index(&self, ix: $tp) -> &$r {
                &self.$m[ix.index()].data
            }

        }
        impl<$n: Data, $e: Data, Ix: IndexType> ops::IndexMut<$tp> for WeightedGraph<N, E, Ix> {
            fn index_mut(&mut self, ix: $tp) -> &mut $r {
                &mut self.$m[ix.index()].data
            }

        }
    }
}
graph_index_impl!(EdgeIndex<Ix>, N, E, E, edges);
graph_index_impl!(NodeIndex<Ix>, N, E, N, nodes);
