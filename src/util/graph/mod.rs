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
use std::iter::{Chain,Iterator};


pub mod visit;


// ================================================================
// Indices: types used to identify nodes and edges.

/// Default integer type used to identify nodes and edges.
pub type DefaultIndexType = u32;


/// Any integer type used as the underlying type for node and edge indices.
pub trait IndexType: Copy + Eq + Hash + Ord + Debug + NumCast + ToPrimitive + 'static {}
impl<T> IndexType for T where T: Copy + Eq + Hash + Ord + Debug + NumCast + ToPrimitive + 'static {}

/// Interface provided by wrapped node or edge index types.
pub trait Index: Copy + Debug {
    /// Create a new index object with the given value.
    fn new(v: usize) -> Self;
    /// Retrieve the underlying integer value of the index object.
    fn index(&self) -> usize;
}


macro_rules! impl_index_wrapper_type {
    ($tp: ty, $n: ident, $T: ident) => {
        impl<$T: IndexType> Index for $tp {
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
pub struct Indices<T: Index>(ops::Range<usize>, PhantomData<T>);
impl<T: Index> Indices<T> {
    /// Create an `Indices` iterator over the given range.
    fn new(r: ops::Range<usize>) -> Self {
        Indices(r, PhantomData)
    }
}
impl<T: Index> Iterator for Indices<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|v| Index::new(v))
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

/// Internal type used to store edge-related data.
#[derive(Clone,Debug)]
pub struct Edge<T: Data, Ix: IndexType = DefaultIndexType> {
    source: NodeIndex<Ix>,
    target: NodeIndex<Ix>,
    data: T
}

impl<T: Data, Ix: IndexType> Edge<T, Ix> {
    /// Create an edge with the given source & target node indices and
    /// weight data.
    fn new(source: NodeIndex<Ix>, target: NodeIndex<Ix>, data: T) -> Self {
        Edge{source: source, target: target, data: data}
    }
}

impl<T: Data, Ix: IndexType, I: Copy> From<(I, I)> for Edge<T, Ix> 
    where T: Default, NodeIndex<Ix>: From<I> {
    fn from(u: (I, I)) -> Self {
        Self::new(NodeIndex::from(u.0), NodeIndex::from(u.1), Default::default())
    }
}

impl<T: Data, Ix: IndexType, I: Copy> From<(I, I, T)> for Edge<T, Ix>
    where NodeIndex<Ix>: From<I> {
    fn from(u: (I, I, T)) -> Self {
        Self::new(NodeIndex::from(u.0), NodeIndex::from(u.1), u.2)
    }
}


impl<'a, T: Data, Ix: IndexType, I: Copy> From<&'a (I, I)> for Edge<T, Ix> 
    where T: Default, NodeIndex<Ix>: From<I> {
    fn from(u: &'a (I, I)) -> Self {
        Self::new(NodeIndex::from(u.0), NodeIndex::from(u.1), Default::default())
    }
}
impl<'a, T: Data, Ix: IndexType, I: Copy> From<&'a (I, I, T)> for Edge<T, Ix>
    where T: Clone, NodeIndex<Ix>: From<I> {
    fn from(u: &'a (I, I, T)) -> Self {
        Self::new(NodeIndex::from(u.0), NodeIndex::from(u.1), u.2.clone())
    }
}


// ----------------------------------------------------------------
// Node

/// Internal type used to store node-related data.
#[derive(Clone,Debug)]
pub struct Node<T: Data, Ix: IndexType = DefaultIndexType> {
    incoming_edges: SmallVec<[EdgeIndex<Ix>; 8]>,
    outgoing_edges: SmallVec<[EdgeIndex<Ix>; 8]>,
    data: T
}

impl<T: Data, Ix: IndexType> Node<T, Ix> {
    /// Instantiate a new node with the given data.
    fn new(data: T) -> Self {
        Node{incoming_edges: SmallVec::new(), outgoing_edges: SmallVec::new(), data: data}
    }

    /// Get an iterator over incoming edges connected to the node.
    ///
    /// Note that if an edge has the node's index as both its source _and_ its
    /// target, it should appear in the set of values returned by this iterator
    /// **and** that returned by the iterator obtained from `outgoing_edges`.
    fn incoming_edges<'a>(&'a self) -> slice::Iter<'a,EdgeIndex<Ix>> where T: 'a {
        self.incoming_edges.iter()
    }


    /// Get an iterator over outgoing edges connected to the node.
    ///
    /// Note that if an edge has the node's index as both its source _and_ its
    /// target, it should appear in the set of values returned by this iterator
    /// **and** that returned by the iterator obtained from `incoming_edges`.
    fn outgoing_edges<'a>(&'a self) -> slice::Iter<'a,EdgeIndex<Ix>> where T: 'a {
        self.outgoing_edges.iter()
    }

    /// Get an iterator over all (incoming + outgoing) edges connected to
    /// the node.
    fn edges<'a>(&'a self) -> Chain<slice::Iter<'a,EdgeIndex<Ix>>,slice::Iter<'a,EdgeIndex<Ix>>> {
        self.incoming_edges().chain(self.outgoing_edges())
    }


    /// Record the existence of an edge that terminates at this node.
    fn add_incoming_edge(&mut self, e: EdgeIndex<Ix>) {
        self.incoming_edges.push(e);
    }

    /// Record the existence of an edge that originates from this node.
    fn add_outgoing_edge(&mut self, e: EdgeIndex<Ix>) {
        self.outgoing_edges.push(e);
    }
}

// ================================================================
// Visitor support

// pub struct ForwardWalker<IndexType = DefaultIndexType> {
//     current_node: NodeIndex<Ix>
// }
// impl<N: Data, E: Data, Ix: IndexType> ForwardWalker<IndexType> {
//     type Item =


// ================================================================
// Graph

/// Standalone member-types helper for Graph.
pub trait Types {
    /// Concrete node type used by a graph.
    type NodeType;
    /// Concrete edge type used by a graph.
    type EdgeType;
    /// Integer type underlying a graph's node and edge index objects.
    type IndexType;
}    

/// Directed graph with user-defined node and edge weights.
#[derive(Clone,Debug)]
pub struct Graph<N: Data, E: Data, Ix: IndexType = DefaultIndexType> {
    nodes: Vec<Node<N, Ix>>,
    edges: Vec<Edge<E, Ix>>
}
impl<N: Data, E: Data, Ix: IndexType> Types for Graph<N, E, Ix> {
    type NodeType = self::Node<N, Ix>;
    type EdgeType = Edge<E, Ix>;
    type IndexType = Ix;
}

impl<N: Data, E: Data, Ix: IndexType> Graph<N, E, Ix> {
    /// Create a new, empty graph.
    pub fn new() -> Self {
        Graph{nodes: Vec::new(), edges: Vec::new()}
    }

    /// Add a node to the graph.
    ///
    /// @return Index of the newly-added node.
    pub fn add_node(&mut self, data: N) -> NodeIndex<Ix> {
        self.nodes.push(Node::new(data));
        NodeIndex::new(self.nodes.len() - 1)
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
        self.edges.push(Edge::new(source, target, data));

        // ...and add cross-references to the nodes.
        let edge = EdgeIndex::new(self.edges.len() - 1);
        self.nodes[source.index()].add_outgoing_edge(edge);
        self.nodes[target.index()].add_incoming_edge(edge);

        edge
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
    /// let mut g: Graph<(),(),u8> = Graph::new();
    /// let n0 = g.add_node(());
    /// let n1 = g.add_node(());
    /// let n2 = g.add_node(());
    /// let n3 = g.add_node(());
    ///
    /// g.add_edges(&[(n0, n1), (n1, n2), (n1, n3), (n2, n3), (n3, n2)]);
    ///
    /// # }
    /// ```
    pub fn add_edges<'a, D>(&mut self, edges: &'a [D]) where D: 'a, <Self as Types>::EdgeType: From<&'a D> {
        for e in edges.iter() {
            let edge = <Self as Types>::EdgeType::from(e);
            let srcidx = edge.source.index();
            let destidx = edge.target.index();
            self.edges.push(edge);
            let index = EdgeIndex::new(self.edges.len() - 1);
            self.nodes[srcidx].add_outgoing_edge(index);
            self.nodes[destidx].add_incoming_edge(index);
        }
    }


    /// Fetch an iterator over the indices of all edges in the graph.
    pub fn edge_indices(&self) -> Indices<EdgeIndex<Ix>> {
        Indices::new(0..self.edges.len())
    }

    /// Fetch the index of the node from which a given edge originates.
    pub fn edge_source(&self, e: EdgeIndex<Ix>) -> Option<NodeIndex<Ix>> {
        if e.index() < self.edges.len() { Some(self.edges[e.index()].source) }
        else { None }
    }


    /// Fetch the index of the node at which a given edge terminates.
    pub fn edge_target(&self, e: EdgeIndex<Ix>) -> Option<NodeIndex<Ix>> {
        if e.index() < self.edges.len() { Some(self.edges[e.index()].target) }
        else { None }
    }

    /// Fetch the indices of an edge's source and target nodes.
    ///
    /// @return A tuple containing the requested indices, or `None` if the
    ///     specified edge index does not refer to a valid edge.
    pub fn edge_endpoints(&self, e: EdgeIndex<Ix>) -> Option<(NodeIndex<Ix>, NodeIndex<Ix>)> {
        if e.index() < self.edges.len() {
            let ref edge = self.edges[e.index()];
            Some((edge.source, edge.target))
        } else {
            None
        }
    }
}

// Implement node-data lookup via indexing.
macro_rules! graph_index_impl {
    ($tp: ty, $n: ident, $e: ident, $r: ident, $m: ident) => {
        impl<$n: Data, $e: Data, Ix: IndexType> ops::Index<$tp> for Graph<N, E, Ix> {
            type Output = $r;
            fn index(&self, ix: $tp) -> &$r {
                &self.$m[ix.index()].data
            }

        }
        impl<$n: Data, $e: Data, Ix: IndexType> ops::IndexMut<$tp> for Graph<N, E, Ix> {
            fn index_mut(&mut self, ix: $tp) -> &mut $r {
                &mut self.$m[ix.index()].data
            }

        }
    }
}
graph_index_impl!(EdgeIndex<Ix>, N, E, E, edges);
graph_index_impl!(NodeIndex<Ix>, N, E, N, nodes);

