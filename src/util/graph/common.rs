//! Common types used by multiple graph implementations.

use std::ops;
use std::fmt::Debug;
use std::hash::{Hash,Hasher};

use num::{NumCast,ToPrimitive};

use super::interface::{self,Id};
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


// --------------------------------
// AdjacencyList

/// Adjacency-list graph implementation.
#[derive(Clone,Debug)]
pub struct AdjacencyList<N, E>
    where N: interface::Node,
          E: interface::Edge {
    nodes: Vec<N>,
    edges: Vec<E>
}

impl<N, E> interface::Graph for AdjacencyList<N, E>
    where N: interface::Node,
          E: interface::Edge {
    type NodeId = <E as interface::Edge>::NodeId;
    type EdgeId = <N as interface::Node>::EdgeId;
    type Node = N;
    type Edge = E;
    type EdgeIdIterator = Indices<Self::EdgeId>;
    type NodeIdIterator = Indices<Self::NodeId>;

    fn new() -> Self {
        AdjacencyList{nodes: Vec::new(), edges: Vec::new()}
    }

    fn add_node<T: Into<Self::Node>>(&mut self, n: T) -> Self::NodeId {
        self.nodes.push(n.into());
        Self::NodeId::new(self.nodes.len() - 1)
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
    fn node_ids(&self) -> Self::NodeIdIterator {
        Indices::new(0..self.nodes.len())
    }

    #[inline]
    fn node_count(&self) -> usize {
        self.nodes.len()
    }

    fn add_edge<T: Into<Self::Edge>>(&mut self, e: T) -> Self::EdgeId {
        let edge = e.into();
        let srcidx = edge.source().index();
        let destidx = edge.target().index();
        let node_count = self.node_count();
        panic_unless!(srcidx < node_count, "Invalid source-node index provided to `Graph::add_edge`");
        panic_unless!(destidx < node_count, "Invalid target-node index provided to `Graph::add_edge`");

        self.edges.push(edge);
        let index = Self::EdgeId::new(self.edges.len() - 1);

        // Update cross-referenced edge lists for the connected nodes.
        self.nodes[srcidx].add_outgoing_edge(index);
        self.nodes[destidx].add_incoming_edge(index);

        index
    }

    fn edge(&self, e: Self::EdgeId) -> &Self::Edge {
        &self.edges[e.index()]
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
        impl<$n, $e, Ix> ops::IndexMut<$tp> for AdjacencyList<N, E>
            where N: interface::Node,
                  E: interface::Edge,
                  Ix: IndexType {
            fn index_mut(&mut self, ix: $tp) -> &mut $r {
                &mut self.$m[ix.index()]
            }

        }
    }
}
graph_index_impl!(EdgeIndex<Ix>, N, E, E, edges);
graph_index_impl!(NodeIndex<Ix>, N, E, N, nodes);
