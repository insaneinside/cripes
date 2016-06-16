//! "Weighted" graph types

use std;
use std::ops::{Deref,DerefMut};
use std::fmt::Debug;

use smallvec::SmallVec;

use super::interface::{self, Id};
use super::common::{IndexType,NodeIndex,EdgeIndex};

/// Trait bounds for types used as edge and node data payloads.
pub trait Data: Clone + Debug {}
impl<T> Data for T where T: Clone + Debug {}

// ----------------------------------------------------------------
// Edge

/// Weighted edge type.
#[derive(Clone,Debug)]
pub struct Edge<T: Data, I: Id> {
    source: I,
    target: I,
    data: T
}

impl<T, I> interface::Edge for Edge<T, I>
    where T: Data, I: Id {
    impl_basic_edge!(I);
}

impl<T: Data, I: Id> Edge<T, I> {
    /// Create an edge with the given source & target node indices and
    /// weight data.
    pub fn new(source: I, target: I, data: T) -> Self {
        Edge{source: source, target: target, data: data}
    }
}

impl<T: Data, I: Id> Deref for Edge<T, I> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T: Data, I: Id> DerefMut for Edge<T, I> {
    fn deref_mut(&mut self) -> &mut <Self as Deref>::Target {
        &mut self.data
    }
}


impl<T: Data, I: Id, I2: Copy> From<(I2, I2)> for Edge<T, I>
    where T: Default, I: From<I2> {
    fn from(u: (I2, I2)) -> Self {
        Self::new(I::from(u.0), I::from(u.1), Default::default())
    }
}

impl<T: Data, I: Id, I2: Copy> From<(I2, I2, T)> for Edge<T, I>
    where I: From<I2> {
    fn from(u: (I2, I2, T)) -> Self {
        Self::new(I::from(u.0), I::from(u.1), u.2)
    }
}


impl<'a, T: Data, I: Id, I2: Copy> From<&'a (I2, I2)> for Edge<T, I>
    where T: Default, I: From<I2> {
    fn from(u: &'a (I2, I2)) -> Self {
        Self::new(I::from(u.0), I::from(u.1), Default::default())
    }
}
impl<'a, T: Data, I: Id, I2: Copy> From<&'a (I2, I2, T)> for Edge<T, I>
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
pub struct Node<T: Data, I: Id> {
    incoming_edges: SmallVec<[I; 8]>,
    outgoing_edges: SmallVec<[I; 8]>,
    data: T
}

impl<T: Data, I: Id> Node<T, I> {
    /// Instantiate a node with the given data.
    pub fn new(data: T) -> Self {
        Node{incoming_edges: SmallVec::new(), outgoing_edges: SmallVec::new(), data: data}
    }
}

impl<T: Data, I: Id> Deref for Node<T, I> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T: Data, I: Id> DerefMut for Node<T, I> {
    fn deref_mut(&mut self) -> &mut <Self as Deref>::Target {
        &mut self.data
    }
}

impl<T, I> interface::Node for Node<T, I>
        where T: Data, I: Id {
    impl_basic_node!(I);
}


// ----------------------------------------------------------------
// AdjacencyList

impl<N: Data, E: Data, Ix: IndexType> super::common::AdjacencyList<Node<N, EdgeIndex<Ix>>, Edge<E, NodeIndex<Ix>>> {
    /// Add a node to the graph.
    ///
    /// @return Index of the newly-added node.
    pub fn add_node(&mut self, data: N) -> NodeIndex<Ix> {
        <Self as interface::Graph>::add_node(self, /*<Self as interface::Graph>::*/Node::new(data))
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
    #[inline(always)]
    pub fn add_edge(&mut self, source: NodeIndex<Ix>, target: NodeIndex<Ix>, data: E) -> EdgeIndex<Ix> {

        // Add the edge itself
        <Self as interface::Graph>::add_edge(self, Edge::new(source, target, data))
    }
}
