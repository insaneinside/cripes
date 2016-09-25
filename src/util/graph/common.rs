//! Common types used by multiple graph implementations.

use std::ops;
use std::fmt::Debug;
use std::default::Default;
use std::hash::{Hash};

use num_traits::{NumCast,ToPrimitive};

use super::interface::{self, Id, Graph};
use super::iter::Indices;

// ================================================================
// Indices: types used to identify nodes and edges.

/// Default integer type used to identify nodes and edges.
pub type DefaultIndexType = u32;


/// Any integer type used as the underlying type for node and edge indices.
pub trait IndexType: Copy + Eq + Hash + Ord + Debug + NumCast + ToPrimitive + 'static {}
impl<T> IndexType for T where T: Copy + Eq + Hash + Ord + Debug + NumCast + ToPrimitive + 'static {}

macro_rules! impl_index_wrapper_type {
    ($tp: ty, $n: ident, $T: ident) => {
        impl<$T: IndexType> Id for $tp {
            #[inline(always)]
            fn new(v: usize) -> Self { $n(NumCast::from(v).unwrap()) }
            #[inline(always)]
            fn index(&self) -> usize { NumCast::from(self.0).unwrap() }
        }

        impl<U: NumCast + ToPrimitive, $T: IndexType> From<U> for $tp {
            fn from(v: U) -> Self {
                Self::new(NumCast::from(v).unwrap())
            }
        }
    }
}

/// Type used to identify and index the edges of a graph.
#[derive(Copy, Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct EdgeIndex<T: IndexType>(T);
impl_index_wrapper_type!(EdgeIndex<T>,EdgeIndex,T);

/// Type used to identify and index the nodes of a graph.
#[derive(Copy, Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct NodeIndex<T: IndexType>(T);
impl_index_wrapper_type!(NodeIndex<T>,NodeIndex,T);


// --------------------------------

/// Implement `interface::Node` for a type with members `incoming_edges` and
/// `outgoing_edges` of types compatible with `Vec<$I>`.
macro_rules! impl_basic_node {
    ($I: ident) => {
        //type EdgeIter<'a> = std::slice::Iter<'a,$I>;

        #[inline(always)]
        fn incoming_edges(&self) -> std::slice::Iter<$I> {
            self.incoming_edges.iter()
        }


        fn incoming_edge_count(&self) -> usize {
            self.incoming_edges.len()
        }

        #[inline(always)]
        fn outgoing_edges(&self) -> std::slice::Iter<$I> {
            self.outgoing_edges.iter()
        }

        fn outgoing_edge_count(&self) -> usize {
            self.outgoing_edges.len()
        }
    }
}

macro_rules! impl_basic_node_mut {
    ($I: ident) => {
        #[inline(always)]
        fn add_incoming_edge(&mut self, e: $I) {
            self.incoming_edges.push(e);
        }

        #[inline(always)]
        fn add_outgoing_edge(&mut self, e: $I) {
            self.outgoing_edges.push(e);
        }
    }
}

// --------------------------------
// AdjacencyList

/// Adjacency-list graph implementation.
#[derive(Clone,Debug)]
pub struct AdjacencyList<N, E> {
    nodes: Vec<N>,
    edges: Vec<E>
}


impl<N, E> AdjacencyList<N, E> {
    /// Create an empty AdjacencyList instance.
    pub fn new() -> Self {
        AdjacencyList{nodes: Vec::new(), edges: Vec::new()}
    }
}

impl<N, E> Default for AdjacencyList<N, E> {
    #[inline(always)]
    fn default() -> Self {
        AdjacencyList::new()
   }
}


impl<N, E> interface::Graph for AdjacencyList<N, E>
    where N: interface::Node,
          E: interface::Edge
{
    type NodeId = <E as interface::Edge>::NodeId;
    type EdgeId = <N as interface::Node>::EdgeId;
    type EdgeIdIterator = Indices<Self::EdgeId>;
    type NodeIdIterator = Indices<Self::NodeId>;

    #[inline(always)]
    fn contains_node(&self, n: Self::NodeId) -> bool {
        n.index() < self.nodes.len()
    }

    #[inline]
    fn node_ids(&self) -> Self::NodeIdIterator {
        Indices::new(0..self.nodes.len())
    }

    #[inline]
    fn node_count(&self) -> usize {
        self.nodes.len()
    }

    fn contains_edge(&self, e: Self::EdgeId) -> bool {
        e.index() < self.edges.len()
    }

    fn edge_ids(&self) -> Self::EdgeIdIterator {
        Indices::new(0..self.edges.len())
    }

    fn edge_count(&self) -> usize {
        self.edges.len()
    }

}

impl<N, E> interface::ConcreteGraph for AdjacencyList<N, E>
    where N: interface::DirectedNode,
          E: interface::DirectedEdge
{
    type Node = N;
    type Edge = E;

    #[inline(always)]
    fn node(&self, n: Self::NodeId) -> &Self::Node {
        &self.nodes[n.index()]
    }

    #[inline(always)]
    fn edge(&self, e: Self::EdgeId) -> &Self::Edge {
        &self.edges[e.index()]
    }
}

impl<N, E> interface::ConcreteGraphMut for AdjacencyList<N, E>
    where N: interface::DirectedNodeMut,
          E: interface::DirectedEdgeMut
{
    #[inline(always)]
    fn node_mut(&mut self, n: Self::NodeId) -> &mut Self::Node {
        &mut self.nodes[n.index()]
    }

    #[inline(always)]
    fn edge_mut(&mut self, e: Self::EdgeId) -> &mut Self::Edge {
        &mut self.edges[e.index()]
    }

    #[inline(always)]
    fn add_node<T: Into<Self::Node>>(&mut self, n: T) -> Self::NodeId {
        self.nodes.push(n.into());
        Self::NodeId::new(self.nodes.len() - 1)
    }

    fn add_edge<T: Into<Self::Edge>>(&mut self, e: T) -> Self::EdgeId {
        let edge = e.into();
        let (a, b) = edge.endpoints();
        let srcidx = a.index();
        let destidx = b.index();

        let node_count = self.node_count();
        panic_unless!(srcidx < node_count, "Invalid source node provided to `Graph::add_edge`: {:?}", a);
        panic_unless!(destidx < node_count, "Invalid target node provided to `Graph::add_edge`: {:?}", b);

        self.edges.push(edge);
        let index = Self::EdgeId::new(self.edges.len() - 1);

        // Update cross-referenced edge lists for the connected nodes.
        self.nodes[srcidx].add_outgoing_edge(index);
        self.nodes[destidx].add_incoming_edge(index);

        index
    }
}


// Implement node-data lookup via indexing.
macro_rules! graph_index_impl {
    ($tp: ty, $n: ident, $e: ident, $r: ident, $m: ident) => {
        impl<$n, $e, Ix> ops::Index<$tp> for AdjacencyList<N, E>
            where N: interface::Node,
                  E: interface::Edge,
                  Ix: IndexType {
            type Output = $r;
            fn index(&self, ix: $tp) -> &$r {
                &self.$m[ix.index()]
            }

        }
        impl<'a, $n, $e, Ix> ops::Index<&'a $tp> for AdjacencyList<N, E>
            where N: interface::Node,
                  E: interface::Edge,
                  Ix: IndexType {
            type Output = $r;
            fn index(&self, ix: &'a $tp) -> &$r {
                &self.$m[ix.index()]
            }

        }

        impl<$n, $e, Ix> ops::IndexMut<$tp> for AdjacencyList<N, E>
            where N: interface::Node,
                  E: interface::Edge,
                  Ix: IndexType {
            fn index_mut(&mut self, ix: $tp) -> &mut $r {
                &mut self.$m[ix.index()]
            }

        }
        impl<'a, $n, $e, Ix> ops::IndexMut<&'a $tp> for AdjacencyList<N, E>
            where N: interface::Node,
                  E: interface::Edge,
                  Ix: IndexType {
            fn index_mut(&mut self, ix: &'a $tp) -> &mut $r {
                &mut self.$m[ix.index()]
            }

        }

    }
}
graph_index_impl!(EdgeIndex<Ix>, N, E, E, edges);
graph_index_impl!(NodeIndex<Ix>, N, E, N, nodes);
