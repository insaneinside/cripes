//! Visitation patterns for graphs.

use super::{Graph,NodeIndex,EdgeIndex,IndexType,Data};

/// Visitation-order selection helper.
pub trait Visitor<N: Data, E: Data, Ix: IndexType> {
    fn next(&mut self, g: &Graph<N, E, Ix>, n: NodeIndex<Ix>) -> Option<EdgeIndex<Ix>>;
}
