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

#[macro_use]
mod common;
pub mod basic;
pub mod weighted;
pub mod iter;
pub mod interface;
pub mod transform;
pub mod visit;

pub use self::common::{EdgeIndex,NodeIndex,AdjacencyList};
pub use self::weighted::{Edge as WeightedEdge, Node as WeightedNode};
pub use self::interface::{Id, Edge, Node, Graph, ConcreteGraph, ConcreteGraphMut};
pub use self::interface::{DirectedGraph, DirectedEdge, DirectedNode, DirectedNodeMut};
pub use self::transform::{Build, Builder, Target};

/// Simple data-bearing directed graph.  `N` determines the type of data
/// attached to nodes, `E` the type attached to edges, and `Ix` the integer
/// type used to identify nodes and edges.
pub type WeightedGraph<N, E, Ix = common::DefaultIndexType> = AdjacencyList<WeightedNode<N, EdgeIndex<Ix>>, WeightedEdge<E, NodeIndex<Ix>>>;

/// Unweighted directed graph.  `Ix` determines the integer type used to
/// identify nodes and edges.
pub type BasicGraph<Ix = common::DefaultIndexType> = AdjacencyList<basic::Node<EdgeIndex<Ix>>, basic::Edge<NodeIndex<Ix>>>;
