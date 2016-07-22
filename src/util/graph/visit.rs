//! Visitation patterns for graphs.
//!
//! As a simple example, the following creates two subgraphs and verifies that
//! iterators over each, using both depth- and breadth-first algorithms, visit
//! each node in the expected order.
//!
//! ```rust
//! use cripes::util::graph::*;
//!
//! let mut g = BasicGraph::<u8>::new();
//!
//! // Add some nodes to the graph so we can add edges between them.
//! for _ in 0..14 {
//!     g.add_node(());
//! }
//!
//! // Build a few sub-graphs to play with; we'll refer to each using the ID of
//! // the root node.
//! //
//! //       (0)            __(7)__
//! //       / \           /       \
//! //     (1) (2)       (8)       (9)
//! //     / \           / \       / \
//! //   (3) (4)      (10) (11) (12) (13)
//! //       / \
//! //     (5) (6)
//! g.add_edges(&[(0, 1), (1, 3), (1, 4), (4, 5), (4, 6), (0, 2),
//!               (7, 8), (8, 10), (8, 11), (7, 9), (9, 12), (9, 13)]);
//!
//! // Define a convenience function to determine the order in which nodes are
//! // walked.
//! fn walk_order<G, A>(g: &G, entry: <G as Graph>::NodeId) -> Vec<usize>
//!     where G: Graph, A: visit::RootedAlgorithm<G> {
//!     g.iter::<A>(entry).map(|id| id.index()).collect::<Vec<_>>()
//! }
//!
//!
//! //                         TREE (0)
//! // Walk the tree using depth-first search:
//! assert_eq!(walk_order::<_, visit::DepthFirst<_>>(&g, 0.into()),
//!            vec![0, 1, 3, 4, 5, 6, 2]);
//!
//!
//! // Breadth-first search:
//! assert_eq!(walk_order::<_, visit::BreadthFirst<_>>(&g, 0.into()),
//!           vec![0, 1, 2, 3, 4, 5, 6]);
//!
//! //                         TREE (7)
//! // Walk the second tree using depth- and breadth-first search.
//! assert_eq!(walk_order::<_, visit::DepthFirst<_>>(&g, 7.into()),
//!            vec![7, 8, 10, 11, 9, 12, 13]);
//! assert_eq!(walk_order::<_, visit::BreadthFirst<_>>(&g, 7.into()),
//!           vec![7, 8, 9, 10, 11, 12, 13]);
//!
//! ```
//!
//!
//!

use std::fmt::Debug;
use std::collections::VecDeque;
use smallvec::SmallVec;

use bit_vec::BitVec;

use super::interface::{DirectedGraph, Graph, Id};


// ================================================================
// Algorithm

/// Visitation-order selection helper.
pub trait Algorithm<G: Graph>: Debug {
    /// Select the next node to visit.
    fn next(&mut self, v: &VisitorState<G>, g: &G) -> Option<<G as Graph>::NodeId>;
}


/// Interface required for "rooted" algorithms.
///
/// Rooted algorithms are those that require a pre-defined entry point on
/// the graph.
pub trait RootedAlgorithm<G: Graph>: Algorithm<G> {
    /// Instantiate and initialize the algorithm object.
    fn new(g: &G, entry: <G as Graph>::NodeId) -> Self;
}


// ================================================================
// Visitor

/// Mutable common state for visitors.
pub struct VisitorState<G: Graph> {
    /// Node most recently visited.
    pub current_node: <G as Graph>::NodeId,
    /// Map of visited nodes.
    visited: BitVec<u32>
}

impl<G: Graph> VisitorState<G> {
    /// Crate a new state object for the specified graph and entry node.
    pub fn new(g: &G, entry: <G as Graph>::NodeId) -> VisitorState<G> {
        VisitorState{current_node: entry, visited: BitVec::from_elem(g.node_count(), false)}
    }

    /// Check if a particular node has been visited.
    pub fn visited(&self, node: <G as Graph>::NodeId) -> bool {
        self.visited.get(node.index()).unwrap_or_else(|| false)
    }

    /// Move to the specified node and mark it as visited.
    fn move_to(&mut self, n: <G as Graph>::NodeId) {
        self.visited.set(n.index(), true);
        self.current_node = n;
    }
}

// ----------------------------------------------------------------

/// Iterator adaptor for Visitor.
///
/// `VisitorIter`s are produced by calling a Visitor's `into_iterator` method
/// with a graph reference as the argument.
pub struct VisitorIter<'a,G: 'a + Graph, V: Visit<G>> {
    graph: &'a G,
    visitor: V
}

impl<'a, G: 'a + Graph, V: Visit<G>> Iterator for VisitorIter<'a, G, V> {
    type Item = <G as Graph>::NodeId;
    fn next(&mut self) -> Option<Self::Item> {
        self.visitor.next(self.graph)
    }
}

// ----------------------------------------------------------------

/// Tracks required state when traversing a graph.
pub struct Visitor<G: Graph, A: Algorithm<G>> where A: Sized {
    state: VisitorState<G>,
    algo: A
}

/// Visitor methods that operate the same regardless of the kind of search
/// algorithm used.
pub trait Visit<G: Graph> {
    /// Determine the next node to inspect.
    fn next(&mut self, g: &G) -> Option<<G as Graph>::NodeId>;

    /*/// Fetch a const reference to the visitor's state data.
    fn state(&self) -> &VisitorState<G>;

    /// Fetch a mutable reference to the visitor's state data.
    fn state_mut(&mut self) -> &mut VisitorState<G>;*/

    /// Transform the visitor into an iterator.
    fn into_iter<'a>(self, g: &'a G) -> VisitorIter<'a, G, Self>
        where Self: Sized {
        VisitorIter{graph: g, visitor: self}
    }
}

impl<G, A> Visitor<G, A>
    where G: Graph, A: Sized + Algorithm<G> + RootedAlgorithm<G> {
    /// Create a Visitor instance for the given entry point.
    pub fn new(g: &G, entry: <G as Graph>::NodeId) -> Self {
        Visitor{state: VisitorState::new(g, entry), algo: A::new(g, entry)}
    }
}

impl<G: Graph, A: Sized + Algorithm<G>> Visit<G> for Visitor<G, A> {
    fn next(&mut self, g: &G) -> Option<<G as Graph>::NodeId> {
        if let Some(node) = self.algo.next(&self.state, g) {
            panic_unless!(g.contains_node(node), "Invalid node ID {:?} produced by algorithm {:?}", node, self.algo);
            self.state.move_to(node);
            Some(node)
        }
        else { None }
    }
}

// ================================================================
// Algorithm implementations

/// Initial capacity for the queues of DepthFirst instances.
const DEPTH_FIRST_INITIAL_STACK_CAPACITY: usize = 8;

/// Initial capacity for the queues of BreadthFirst instances.
const BREADTH_FIRST_INITIAL_QUEUE_CAPACITY: usize = 8;

/// Depth-first traversal algorithm.
///
/// Memory: allocates on the heap.
#[derive(Debug)]
pub struct DepthFirst<G: Graph> {
    stack: SmallVec<[<G as Graph>::NodeId; DEPTH_FIRST_INITIAL_STACK_CAPACITY]>
}

impl<G: Graph + Debug> RootedAlgorithm<G> for DepthFirst<G> {
    /// Create a depth-first search object starting at the specified node.
    fn new(_: &G, entry: <G as Graph>::NodeId) -> Self {
        let mut stack = SmallVec::new(); stack.push(entry);
        DepthFirst{stack: stack}
    }
}


impl<G> Algorithm<G> for DepthFirst<G>
    where G: Debug + DirectedGraph
{
    fn next(&mut self, _: &VisitorState<G>, g: &G) -> Option<G::NodeId> {
        if let Some(next) = self.stack.pop() {
            self.stack.extend(g.direct_successors(next).rev());
            Some(next)
        } else {
            None
        }
    }
}


/// Breadth-first traversal algorithm.
///
/// Memory:   allocates on the heap.

#[derive(Debug)]
pub struct BreadthFirst<G: Graph> {
    queue: VecDeque<<G as Graph>::NodeId>
}

impl<G> RootedAlgorithm<G> for BreadthFirst<G>
    where G: Debug + DirectedGraph
{
    /// Create a breadth-first search object starting at the specified node.
    fn new(_: &G, entry: <G as Graph>::NodeId) -> Self {
        let mut queue = VecDeque::with_capacity(BREADTH_FIRST_INITIAL_QUEUE_CAPACITY);
        queue.push_back(entry);
        BreadthFirst{queue: queue}
    }
}

impl<G> Algorithm<G> for BreadthFirst<G>
    where G: Debug + DirectedGraph
{
    fn next(&mut self, _: &VisitorState<G>, g: &G) -> Option<<G as Graph>::NodeId> {
        if let Some(next) = self.queue.pop_front() {
            self.queue.extend(g.direct_successors(next));
            Some(next)
        } else {
            None
        }
    }
}
