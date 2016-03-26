//! Common types used by multiple graph implementations.

use std::fmt::Debug;
use std::hash::{Hash,Hasher};

use num::{NumCast,ToPrimitive};

use super::interface::Id;

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

