//! "Weighted" graph types

use std;
use std::ops;
use std::fmt::Debug;

use smallvec::SmallVec;

use super::iter::{Indices,Successors};
use super::interface::{Id,Edge,Node,Graph};
use super::common::{IndexType,NodeIndex,EdgeIndex,DefaultIndexType};

/// Trait bounds for types used as edge and node data payloads.
pub trait Data: Clone + Debug {}
impl<T> Data for T where T: Clone + Debug {}

// ----------------------------------------------------------------
// Edge

/// Weighted edge type.
#[derive(Clone,Debug)]
pub struct WeightedEdge<T: Data, I: Id> {
    source: I,
    target: I,
    data: T
}

impl<T, I> Edge<I> for WeightedEdge<T, I>
    where T: Data, I: Id {
    impl_basic_edge!();
}

impl<T: Data, I: Id> WeightedEdge<T, I> {
    /// Create an edge with the given source & target node indices and
    /// weight data.
    pub fn new(source: I, target: I, data: T) -> Self {
        WeightedEdge{source: source, target: target, data: data}
    }
}
///
impl<T: Data, I: Id> std::ops::Deref for WeightedEdge<T, I> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.data
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

///  Weighted node implementation.
///
/// A reference to the node's weight data can be obtained using the type's
/// `Deref` implementation.
///
/// ```rust
/// use cripes::util::graph::{EdgeIndex,WeightedNode};
///
/// # fn main() {
/// let n = WeightedNode::<_, EdgeIndex<u8>>::new(32);
/// assert_eq!(32, *n);
/// # }
/// ```

#[derive(Clone,Debug)]
pub struct WeightedNode<T: Data, I: Id> {
    incoming_edges: SmallVec<[I; 8]>,
    outgoing_edges: SmallVec<[I; 8]>,
    data: T
}

impl<T: Data, I: Id> WeightedNode<T, I> {
    /// Instantiate a new node with the given data.
    pub fn new(data: T) -> Self {
        WeightedNode{incoming_edges: SmallVec::new(), outgoing_edges: SmallVec::new(), data: data}
    }
}

impl<T: Data, I: Id> std::ops::Deref for WeightedNode<T, I> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T, I> Node<I> for WeightedNode<T, I>
        where T: Data, I: Id {
    impl_basic_node!(I);
}


// ----------------------------------------------------------------
// Graph


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
    type EdgeIdIterator = Indices<Self::EdgeId>;
    type NodeIdIterator = Indices<Self::NodeId>;

    fn new() -> Self {
        WeightedGraph{nodes: Vec::new(), edges: Vec::new()}
    }

    fn add_node(&mut self, n: Self::Node) -> Self::NodeId {
        self.nodes.push(n);
        NodeIndex::new(self.nodes.len() - 1)
    }

    #[inline(always)]
    fn contains_node(&self, n: Self::NodeId) -> bool {
        n.index() < self.nodes.len()
    }

    
    #[inline(always)]
    fn node(&self, n: Self::NodeId) -> &Self::Node {
        &self.nodes[n.index()]
    }

    #[inline(always)]
    fn node_mut(&mut self, n: Self::NodeId) -> &mut Self::Node {
        &mut self.nodes[n.index()]
    }

    #[inline]
    fn incoming_edges(&self, n: Self::NodeId) -> std::slice::Iter<Self::EdgeId> { 
        self.nodes[n.index()].incoming_edges()
    }

    #[inline]
    fn outgoing_edges(&self, n: Self::NodeId) -> std::slice::Iter<Self::EdgeId> {
        self.nodes[n.index()].outgoing_edges()
    }

    #[inline]
    fn direct_successors(&self, n: Self::NodeId) -> Successors<Self> {
        Successors::new(self, self.outgoing_edges(n))
    }

    #[inline]
    fn node_ids(&self) -> Self::NodeIdIterator {
        Indices::new(0..self.nodes.len())
    }

    #[inline]
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

    fn edge(&self, e: Self::EdgeId) -> &Self::Edge {
        &self.edges[e.index()]
    }

    fn contains_edge(&self, e: Self::EdgeId) -> bool {
        e.index() < self.edges.len()
    }

    fn edge_source(&self, e: EdgeIndex<Ix>) -> Self::NodeId {
        self.edges[e.index()].source()
    }

    fn edge_target(&self, e: EdgeIndex<Ix>) -> Self::NodeId {
        self.edges[e.index()].target()
    }

    fn edge_endpoints(&self, e: EdgeIndex<Ix>) -> (Self::NodeId, Self::NodeId) {
        let ref edge = self.edges[e.index()];
        (edge.source(), edge.target())
    }

    fn edge_ids(&self) -> Self::EdgeIdIterator {
        Indices::new(0..self.edges.len())
    }

    fn edge_count(&self) -> usize {
        self.edges.len()
    }
}


impl<N: Data, E: Data, Ix: IndexType> WeightedGraph<N, E, Ix> {
    /// Add a node to the graph.
    ///
    /// @return Index of the newly-added node.
    pub fn add_node(&mut self, data: N) -> NodeIndex<Ix> {
        <Self as Graph>::add_node(self, <Self as Graph>::Node::new(data))
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