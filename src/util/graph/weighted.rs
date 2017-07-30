//! "Weighted" graph types

use std;
use std::ops::{Deref,DerefMut};

use smallvec::SmallVec;

use super::interface::{self, Id};

// ----------------------------------------------------------------
// Edge

/// Weighted edge type.
#[derive(Debug)]
pub struct Edge<T, I: Id> {
    source: I,
    target: I,
    data: T
}


impl<T, I: Id> Clone for Edge<T, I>
    where T: Clone
{
    fn clone(&self) -> Self {
        Edge{source: self.source, target: self.target, data: self.data.clone()}
    }
}


impl<T, I: Id> interface::Edge for Edge<T, I> {
    type NodeId = I;
    #[inline]
    fn endpoints(&self) -> (Self::NodeId, Self::NodeId) {
        (self.source, self.target)
    }
}

impl<T, I: Id> interface::DirectedEdge for Edge<T, I> {
    #[inline]
    fn source(&self) -> I { self.source }

    #[inline]
    fn target(&self) -> I { self.target }
}

impl<T, I: Id> interface::DirectedEdgeMut for Edge<T, I> {
    fn rev(&mut self) {
        ::std::mem::swap(&mut self.source, &mut self.target);
    }
}


impl<T, I: Id> Edge<T, I> {
    /// Create an edge with the given source & target node indices and
    /// weight data.
    pub fn new(source: I, target: I, data: T) -> Self {
        Edge{source: source, target: target, data: data}
    }

    /// Retrieve a reference to the edge's data (weight)
    pub fn data(&self) -> &T {
        &self.data
    }
}

impl<T, I: Id> Deref for Edge<T, I> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T, I: Id> DerefMut for Edge<T, I> {
    fn deref_mut(&mut self) -> &mut <Self as Deref>::Target {
        &mut self.data
    }
}


impl<T, I: Id, I2: Copy> From<(I2, I2)> for Edge<T, I>
    where T: Default, I: From<I2> {
    fn from(u: (I2, I2)) -> Self {
        Self::new(I::from(u.0), I::from(u.1), Default::default())
    }
}

impl<T, I: Id, I2: Copy> From<(I2, I2, T)> for Edge<T, I>
    where I: From<I2> {
    fn from(u: (I2, I2, T)) -> Self {
        Self::new(I::from(u.0), I::from(u.1), u.2)
    }
}


impl<'a, T, I: Id, I2: Copy> From<&'a (I2, I2)> for Edge<T, I>
    where T: Default, I: From<I2> {
    fn from(u: &'a (I2, I2)) -> Self {
        Self::new(I::from(u.0), I::from(u.1), Default::default())
    }
}
impl<'a, T, I: Id, I2: Copy> From<&'a (I2, I2, T)> for Edge<T, I>
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
/// use cripes_core::util::graph::{EdgeId,WeightedNode};
///
/// # fn main() {
/// let n = WeightedNode::<_, EdgeId<u8>>::new(32);
/// assert_eq!(32, *n);
/// # }
/// ```

#[derive(Debug)]
pub struct Node<T, I: Id> {
    incoming_edges: SmallVec<[I; 8]>,
    outgoing_edges: SmallVec<[I; 8]>,
    data: T
}

impl<T, I: Id> Node<T, I> {
    /// Instantiate a node with the given data.
    pub fn new(data: T) -> Self {
        Node{incoming_edges: SmallVec::new(), outgoing_edges: SmallVec::new(), data: data}
    }

    /// Retrieve a reference to the nodes's data (weight)
    pub fn data(&self) -> &T {
        &self.data
    }
}
impl<T, I: Id> Clone for Node<T, I>
    where T: Clone
{
    fn clone(&self) -> Self {
        Node{incoming_edges: self.incoming_edges.clone(),
             outgoing_edges: self.outgoing_edges.clone(),
             data: self.data.clone()}
    }
}

impl<T, I: Id> Deref for Node<T, I> {
    type Target = T;
    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T, I: Id> DerefMut for Node<T, I> {
    #[inline]
    fn deref_mut(&mut self) -> &mut <Self as Deref>::Target {
        &mut self.data
    }
}

impl<T, I: Id> From<T> for Node<T, I> {
    #[inline]
    fn from(data: T) -> Self {
        Self::new(data)
    }
}

impl<T, I: Id> interface::Node for Node<T, I> {
    type EdgeId = I;
    fn edges(&self) -> std::iter::Chain<std::slice::Iter<Self::EdgeId>,std::slice::Iter<Self::EdgeId>> {
        self.incoming_edges.iter().chain(self.outgoing_edges.iter())
    }
}

impl<T, I: Id> interface::DirectedNode for Node<T, I> {
    impl_basic_node!(I);
}

impl<T, I: Id> interface::DirectedNodeMut for Node<T, I> {
    impl_basic_node_mut!(I);
}
