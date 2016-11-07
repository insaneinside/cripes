//! Pattern-sequence data structures and implementations.

use std;
use std::ops::Index;
use std::iter::FromIterator;

use util::set::{self, Contains};
use super::{Anchor, Atom, Element, Repetition, Union};
use super::{Reduce, flatten_and_reduce};
#[cfg(feature = "pattern_class")]
use super::Class;


// To hide the implementation details, we wrap the type alias in
// a private submodule.
mod seq_impl {
    pub type Inner<T> = Vec<T>;
}

/// Sequence of patterns.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct Sequence<T>(seq_impl::Inner<T>);

impl<T> Sequence<T> {
    /// Create a new sequence from the given vector.
    ///
    /// This method will **panic** if the given vector is not at least two
    /// elements long.
    pub fn new(v: Vec<T>) -> Self {
        if v.len() < 2 {
            panic!("Sequences must have at least two elements");
        }
        Sequence(v)
    }

    /// Get the number of elements in the sequence.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Fetch an iterator over the elements in the sequence
    pub fn iter<'a>(&'a self) -> impl Iterator<Item=&'a T> {
        self.0.iter()
    }

    /// Produce an iterator over the elements in the sequence, consuming it.
    pub fn into_iter(self) -> impl Iterator<Item=T> {
        self.0.into_iter()
    }

    fn into_inner(self) -> seq_impl::Inner<T> {
        self.0
    }

    /// Append an item to the sequence.
    pub fn push(&mut self, t: T) {
        self.0.push(t);
    }

    /// Insert a value at the specified index.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds.
    pub fn insert(&mut self, index: usize, value: T) {
        self.0.insert(index, value)
    }

}


impl<T> ::std::iter::Extend<T> for Sequence<T> {
    fn extend<I>(&mut self, iter: I)
        where I: IntoIterator<Item=T>
    {
        self.0.extend(iter)
    }
}

// ----------------------------------------------------------------


impl<T, U> super::MapAtoms<T, U> for Sequence<Element<T>>
    where T: Atom, U: Atom
{
    type Output = Sequence<Element<U>>;

    /// Transforms each contained atom using the supplied function or closure
    /// to produce a new sequence.
    fn map_atoms<F>(self, f: F) -> Self::Output
        where F: Fn(T) -> U,
    {
        self.0.into_iter().map(|elt| elt.map_atoms(&f)).collect()
    }
}

impl<T: Atom> Reduce for Sequence<Element<T>> {
    type Output = Option<Element<T>>;

    /// Flatten the sequence and apply implementation-defined optimizations.
    fn reduce(mut self) -> Self::Output {
        // Flatten and reduce the vector *first* so we can reduce `self` based
        // on the size of the *reduced* vector.
       flatten_and_reduce(&mut self.0, Element::Wildcard,
                           |elt| match elt { &Element::Sequence(ref s) => Some(s.len()), _ => None },
                           |elt| match elt { Element::Sequence(s) => s.into_inner(), _ => unreachable!() });
        match self.0.len() {
            0 => None,
            1 => Some(self.0.swap_remove(0)),
            _ => Some(Element::Sequence(self))
        }
    }
}

impl<T> Index<usize> for Sequence<T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }

}

impl<T> IntoIterator for Sequence<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;
    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T> FromIterator<T> for Sequence<T> {
    fn from_iter<I>(iter: I) -> Self
        where I: IntoIterator<Item=T>
    {
        Sequence(iter.into_iter().collect())
    }
}


impl<T: Atom> set::Contains<T> for Sequence<Element<T>> {
    /// A sequence always contains at least two elements, and thus will never
    /// match a single atom.
    #[inline(always)]
    fn contains(&self, _: T) -> bool {
        false
    }
}

// ----------------------------------------------------------------
// set::IsSubsetOf

macro_rules! is_subset_if_length_1_and_first_element_is_subset {
    ($T: ident,$Other: ty, $name: ident; $doc: expr) => {
        impl<$T: Atom> set::IsSubsetOf<$Other> for Sequence<Element<$T>> {
            #[doc = $doc]
            #[inline]
            fn is_subset_of(&self, $name: &$Other) -> bool {
                self.len() == 1 && self[0].is_subset_of($name)
            }
        }}}

is_subset_if_length_1_and_first_element_is_subset! {
    T, T, atom;
    "A sequence is a subset of a particular atom whenever the sequence has length one, and the first element is a subset of that atom."}

is_subset_if_length_1_and_first_element_is_subset! {
    T, Anchor<T>, anchor;
    "A sequence is a subset of an anchor whenever the sequence has length one, and the first element is a subset of that atom."}

#[cfg(feature = "pattern_class")]
is_subset_if_length_1_and_first_element_is_subset! {
    T, Class<T>, class;
    "A sequence is a subset of an atom class whenever the sequence has length one, and the first element is a subset of that class."}

impl<T> set::IsSubsetOf<Sequence<T>> for Sequence<T>
    where T: set::IsSubsetOf<T>
{
    /// A sequence `A` can be a subset of another sequence `B` *if* both have the
    /// same length and each element in `A` is a subset of the corresponding element
    /// in `B`:
    ///
    /// A ⊂ B ⇔ |A|=|B|=n ∧ Aᵢ ⊂ Bᵢ ∀ i∈[0,n]
    fn is_subset_of(&self, other: &Sequence<T>) -> bool {
        other.len() == self.len() &&
            self.iter().zip(other.iter()).all(|(u, v)| u.is_subset_of(v))
    }
}

impl<T, M> set::IsSubsetOf<Union<M>> for Sequence<T>
    where Sequence<T>: set::IsSubsetOf<M>
{
    /// A sequence is a subset of a union whenever it is a subset of one of the
    /// union's members.
    fn is_subset_of(&self, union: &Union<M>) -> bool {
        union.iter().any(|m| self.is_subset_of(m))
    }
}

impl<T, M> set::IsSubsetOf<Repetition<M>> for Sequence<T>
    where T: set::IsSubsetOf<M>
{
    /// A sequence is a subset of a repetition if the sequence's length is
    /// contained by the repetition's count and all members of the sequence are
    /// subsets of the repeated element.
    fn is_subset_of(&self, rep: &Repetition<M>) -> bool {
        rep.count().contains(self.len()) && self.iter().all(|elt| elt.is_subset_of(rep.value()))
    }
}

impl<T: Atom> set::IsSubsetOf<Element<T>> for Sequence<Element<T>> {
    fn is_subset_of(&self, elt: &Element<T>) -> bool {
        match elt {
            &Element::Tagged{ref element, ..} => self.is_subset_of(&**element),
            &Element::Sequence(ref s) => self.is_subset_of(s),
            &Element::Union(ref u) => self.is_subset_of(u),
            &Element::Repeat(ref r) => self.is_subset_of(&**r),

            &Element::Anchor(ref a) => self.is_subset_of(a),
            &Element::Atom(ref a) => self.is_subset_of(a),
            #[cfg(feature = "pattern_class")]
            &Element::Class(ref c) => self.is_subset_of(c),
            &Element::Wildcard => self.len() == 1 && self[0].is_subset_of(elt),
            &Element::Not(ref element) => ! self.is_subset_of(&**element),
        }
    }
}
