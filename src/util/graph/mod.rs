//! Interfaces and implementations for directed graphs.
//!
//! This module provides a set of interfaces and implementations of software
//! components for graph representations.
//!
//! These types are designed to maximize composability and ergonomics;
//! currently we have a single graph implementation
//! ([AdjacencyList](struct.AdjacencyList.html), which uses the adjacency-list
//! pattern and integer-based IDs) and two sets of edge/node implementations
//! ([basic](basic/) and [weighted](weighted/)).
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

mod common;
pub mod iter;
pub mod interface;
pub mod transform;
pub mod visit;

pub use self::common::{EdgeIndex,NodeIndex,AdjacencyList};
pub use self::weighted::{Edge as WeightedEdge, Node as WeightedNode};
pub use self::interface::{Id, Edge, Node, Graph, ConcreteGraph};
pub use self::interface::{DirectedGraph, DirectedNode, DirectedEdge};
pub use self::transform::{Builder,Rewriter};

/// Simple data-bearing directed graph.  `N` determines the type of data
/// attached to nodes, `E` the type attached to edges, and `Ix` the integer
/// type used to identify nodes and edges.
pub type WeightedGraph<N, E, Ix = common::DefaultIndexType> = AdjacencyList<WeightedNode<N, EdgeIndex<Ix>>, WeightedEdge<E, NodeIndex<Ix>>>;

/// Unweighted directed graph.  `Ix` determines the integer type used to
/// identify nodes and edges.
pub type BasicGraph<Ix = common::DefaultIndexType> = AdjacencyList<basic::Node<EdgeIndex<Ix>>, basic::Edge<NodeIndex<Ix>>>;

/// Implement `interface::Edge` for a type with members
/// `source: $I` and `target: $I`.
macro_rules! impl_basic_edge {
    ($I: ident) => {
        type NodeId = $I;
        #[inline(always)]
        fn source(&self) -> I { self.source }
        #[inline(always)]
        fn target(&self) -> I { self.target }
    }
}


/// Implement `interface::Node` for a type with members `incoming_edges` and
/// `outgoing_edges` of types compatible with `Vec<$I>`.
macro_rules! impl_basic_node {
    ($I: ident) => {
        type EdgeId = $I;
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


pub mod weighted;
pub mod basic;
