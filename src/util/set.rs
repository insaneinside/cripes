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
    fn contains(&self, element: E) -> bool;
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
    fn difference(self, other: &S) -> Self::Output;
}
