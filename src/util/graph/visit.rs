//! Visitation patterns for graphs.

use std::default::Default;

use bit_vec::BitVec;

use super::{Graph,Id};


// ================================================================
// Algorithm

/// Visitation-order selection helper.
pub trait Algorithm: Default {
    /// Select the next edge to follow given the current node index and
    fn next<G>(&mut self, g: &G, n: <G as Graph>::NodeId) -> Option<<G as Graph>::EdgeId>
        where G: Graph;
}

// We can use free-standing functions as visit algorithms.
impl<G: Graph, F> Algorithm for F
    where F: FnMut(&G, <G as Graph>::NodeId) -> Option<<G as Graph>::EdgeId> {
    fn next<G>(&mut self, g: &G, n: <G as Graph>::NodeId) -> Option<<G as Graph>::EdgeId> {
        self(g, n)
    }
}



// ================================================================
// Visitor

/// Tracks required state when traversing a graph.
pub struct Visitor<G: Graph, A: Algorithm> {
    current_node: <G as Graph>::NodeId,
    visited: BitVec<u32>,
    algo: A
}
impl<G: Graph, A: Algorithm> Visitor<G, A> {
    /// Create a Visitor instance.
    pub fn new(g: &G, entry: <G as Graph>::NodeId) -> Self {
        Visitor{visited: BitVec::from_elem(g.node_count(), false), algo: Default::default()}
    }

    /// Check if a particular node has been visited.
    pub fn visited(&self, node: <G as Graph>::NodeId) -> bool {
        self.visited.get(node.index()).or_else(|| false)
    }
}

// ================================================================
// Algorithm implementations
