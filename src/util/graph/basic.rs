//! Implementation of a basic, unweighted graph and supporting types.

use std;
use std::default::Default;

use smallvec::SmallVec;

use super::interface::{self, Id};

// ----------------------------------------------------------------
// Node

/// Simple unweighted node.
#[derive(Clone,Debug)]
pub struct Node<I: Id> {
    incoming_edges: SmallVec<[I; 8]>,
    outgoing_edges: SmallVec<[I; 8]>
}
impl<I: Id> Node<I> {
    /// Create a node with no edges attached.
    pub fn new() -> Self {
        Node{incoming_edges: SmallVec::new(),
             outgoing_edges: SmallVec::new()}
    }
}

impl<I: Id> Default for Node<I> {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

impl<I> interface::Node for Node<I>
    where I: Id {
    type EdgeId = I;
    fn edges(&self) -> std::iter::Chain<std::slice::Iter<Self::EdgeId>,std::slice::Iter<Self::EdgeId>> {
        self.incoming_edges.iter().chain(self.outgoing_edges.iter())
    }
}

impl<I: Id> interface::DirectedNode for Node<I> {
    impl_basic_node!(I);
}

impl<I: Id> From<()> for Node<I> {
    #[inline(always)]
    fn from(_: ()) -> Self {
        Node::new()
    }
}

// ----------------------------------------------------------------
// Edge

/// Simple directed edge.
#[derive(Clone,Debug)]
pub struct Edge<I: Id> {
    source: I,
    target: I
}

impl<I: Id> Edge<I> {
    /// Create an edge with the given source & target node IDs.
    #[inline]
    pub fn new(source: I, target: I) -> Self {
        Edge{source: source, target: target}
    }
}

impl<I> interface::Edge for Edge<I> where I: Id {
    type NodeId = I;
    #[inline]
    fn endpoints(&self) -> (Self::NodeId, Self::NodeId) {
        (self.source, self.target)
    }
}

impl<I> interface::DirectedEdge for Edge<I> where I: Id {
    #[inline]
    fn source(&self) -> I { self.source }

    #[inline]
    fn target(&self) -> I { self.target }
}


impl<I: Id, I2: Copy> From<(I2, I2)> for Edge<I>
    where I: From<I2> {
    fn from(u: (I2, I2)) -> Self {
        Self::new(I::from(u.0), I::from(u.1))
    }
}

impl<'a, I: Id, I2: Copy> From<&'a (I2, I2)> for Edge<I>
    where I: From<I2> {
    fn from(u: &'a (I2, I2)) -> Self {
        Self::new(I::from(u.0), I::from(u.1))
    }
}

impl<I: Id, I2: Copy> From<(I2, I2, ())> for Edge<I>
    where I: From<I2> {
    fn from(u: (I2, I2, ())) -> Self {
        Self::new(I::from(u.0), I::from(u.1))
    }
}

impl<'a, I: Id, I2: Copy> From<&'a (I2, I2, ())> for Edge<I>
    where I: From<I2> {
    fn from(u: &'a (I2, I2, ())) -> Self {
        Self::new(I::from(u.0), I::from(u.1))
    }
}

// ----------------------------------------------------------------
// Graph
