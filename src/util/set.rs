//! Interfaces for set-theoretic operations and relations on arbitrary types.
//!
//! These traits define interfaces for set-like operations on pre-defined
//! types, and when implemented for a type allow use of several
//! included algorithms.

use std::iter::Iterator;

/// Trait for checking if a set contains the given element.
pub trait Contains<E>
    where E: PartialEq
{
    /// Check if the set contains the given element.
    fn contains(&self, element: &E) -> bool;
}

/// Interface used to check if a set contains zero elements.
pub trait IsEmpty {
    /// Check if the set is empty.
    fn is_empty(&self) -> bool;
}


/// Trait for checking if a particular set's members are all contained in
/// another set.
pub trait IsSubsetOf<S> {
    /// Determine whether all elements in this are also elements of the
    /// given set.
    fn is_subset_of(&self, other: &S) -> bool;
}


/// Trait for checking if a particular set contains all members of another set.
///
/// `IsSupersetOf<S>` is automatically defined for all types `T` where
/// `S: [IsSubsetOf](trait.IsSubsetOf.html)<T>`; therefore you should prefer to
/// implement `IsSubsetOf` instead.
pub trait IsSupersetOf<S> {
    /// Check if this set contains all elements of the given set.
    fn is_superset_of(&self, other: &S) -> bool;
}


impl<T, S> IsSupersetOf<S> for T
    where S: IsSubsetOf<T>
{
    #[inline(always)]
    fn is_superset_of(&self, other: &S) -> bool {
        other.is_subset_of(self)
    }
}


/// Trait for set types whose elements can be accessed by iteration.
pub trait IterableSet<'a, E: 'a> {
    /// Iterator type used when iterating over the set's elements.
    type Iter: Iterator<Item=&'a E>;

    /// Fetch an iterator over (references to) the members of the set.
    fn iter(&self) -> Self::Iter;
}


/// Trait for set types that support the [absolute
/// complement][absolute-complement] operation.
///
/// [absolute-complement]: https://en.wikipedia.org/wiki/Complement_(set_theory)#Absolute_complement
pub trait Complement {
    /// Type returned by an instance's `complement()` method.
    type Output;

    /// Calculate the absolute complement of the set.
    fn complement(&self) -> Self::Output;
}


/// Trait for set types that support the [set-theoretic difference][setdiff]
/// operation, which is also called the relative complement.
///
/// [setdiff]: https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement
pub trait Difference<S> { 
    /// Type of the result of computing this difference.
    type Output;
    /// Calculate the set-theoretic difference of this set with the one given.
    fn difference(&self, other: &S) -> Self::Output;
}

/// Trait for computing the [set-theoretic difference][setdiff] in-place, modifying
/// the receiver.
///
/// [setdiff]: https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement
pub trait Subtract<S> {
    /// Remove the values in `other` from `self`, and return `self`.
    fn subtract(&mut self, other: &S) -> &Self;
}


/// Utility for working with multiple sets as an aggregate.
pub struct AggregateSet<'a, S: 'a> {
    sets: Vec<&'a S>
}

impl<'a, S: 'a, E> Contains<E> for AggregateSet<'a, S>
    where S: Contains<E>, E: PartialEq
{
    #[inline]
    fn contains(&self, e: &E) -> bool {
        self.sets.iter().any(|set| set.contains(e))
    }
}

impl<'a, S: 'a> IsSubsetOf<AggregateSet<'a, S>> for S
    where S: IsEmpty + IsSubsetOf<S> + Subtract<S> + Clone
{
    fn is_subset_of(&self, agg: &AggregateSet<'a, S>) -> bool {
        // If this set is empty, or is completely contained by any of the
        // aggregate's members, it's a subset.
        if self.is_empty() || agg.sets.iter().any(|set| self.is_subset_of(set)) {
            return true
        }

        // Otherwise we'll go the brute-force route: make a clone of `self`,
        // then subtract each member set from the clone until either the clone
        // is empty (=> this set is a subset of the aggregate) or we run out of
        // members (=> not a subset).
        let mut iter = agg.sets.iter();
        let mut diff = self.clone();

        while let Some(set) = iter.next() {
            if diff.subtract(set).is_empty() {
                return true;
            }
        }

        false
    }
}
