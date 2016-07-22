//! "Weighted" graph types

use std;
use std::ops::{Deref,DerefMut};

use smallvec::SmallVec;

use super::interface::{self, Id};

/// Trait bounds for types used as edge and node data payloads.
pub trait Data: Clone {}
impl<T> Data for T where T: Clone {}

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
    where I: Id,
          T: Data {
    type NodeId = I;
    #[inline]
    fn endpoints(&self) -> (Self::NodeId, Self::NodeId) {
        (self.source, self.target)
    }
}

impl<T, I> interface::DirectedEdge for Edge<T, I>
    where I: Id,
          T: Data {
    #[inline]
    fn source(&self) -> I { self.source }

    #[inline]
    fn target(&self) -> I { self.target }
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
    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T: Data, I: Id> DerefMut for Node<T, I> {
    #[inline]
    fn deref_mut(&mut self) -> &mut <Self as Deref>::Target {
        &mut self.data
    }
}

impl<T: Data, I: Id> From<T> for Node<T, I> {
    #[inline]
    fn from(data: T) -> Self {
        Self::new(data)
    }
}

impl<T, I> interface::Node for Node<T, I>
    where T: Data, I: Id {
    type EdgeId = I;
    fn edges(&self) -> std::iter::Chain<std::slice::Iter<Self::EdgeId>,std::slice::Iter<Self::EdgeId>> {
        self.incoming_edges.iter().chain(self.outgoing_edges.iter())
    }
}

impl<T, I> interface::DirectedNode for Node<T, I>
    where T: Data, I: Id {
    impl_basic_node!(I);
}
