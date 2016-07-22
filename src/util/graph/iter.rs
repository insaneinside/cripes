//! Iterators used by the graph implementations.

use std::ops;
use std::slice;
use std::marker::PhantomData;
use std::iter::{Iterator,DoubleEndedIterator};

use super::interface::{Id, Graph, DirectedGraph};


/// Iterator over the (node or edge) indices of a graph.
#[derive(Clone)]
pub struct Indices<T: Id>(ops::Range<usize>, PhantomData<T>);

impl<T: Id> Indices<T> {
    /// Create an `Indices` iterator over the given range.
    pub fn new(r: ops::Range<usize>) -> Self {
        Indices(r, PhantomData)
    }
}

impl<T: Id> Iterator for Indices<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|v| Id::new(v))
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}



/// Iterator over some node's direct successors in a particular graph.
pub struct Successors<'a, G>
    where G: 'a + DirectedGraph,
          G::EdgeId: 'a
{
    graph: &'a G,
    //iter: G::Node::EdgeIdIterator;
    iter: slice::Iter<'a, G::EdgeId>
}

impl<'a,G> Successors<'a, G>
    where G: 'a + DirectedGraph,
          G::EdgeId: 'a
{
    /// Create an iterator over the successors of a node using that node's
    /// outgoing edge list.
    pub fn new(g: &'a G, iter: slice::Iter<'a, <G as Graph>::EdgeId>) -> Self {
        Successors{graph: g, iter: iter}
    }
}

impl<'a,G> Iterator for Successors<'a,G>
    where G: 'a + DirectedGraph
{
    type Item = <G as Graph>::NodeId;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|id| self.graph.edge_target(*id))
    }
}

impl<'a,G> DoubleEndedIterator for Successors<'a,G>
    where G: 'a + DirectedGraph
{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.next_back().map(|id| self.graph.edge_target(*id))
    }
}
