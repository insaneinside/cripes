//! Pattern-sequence data structures and implementations.

use std::ops::Index;
use std::iter::FromIterator;

use util::set::{self, Contains};
use super::{Atom, Class, Element, Repetition, Union};
use super::{Reduce, flatten_and_reduce};

// To hide the implementation details, we wrap the type alias in
// a private submodule.
mod seq_impl {
    use ::pattern::Element;
    pub type Inner<T> = Vec<Element<T>>;
}

/// Sequence of patterns.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct Sequence<T: Atom>(seq_impl::Inner<T>);

impl<T: Atom> Sequence<T> {
    /// Create a new sequence from the given vector.
    ///
    /// This method will **panic** if the given vector is not at least two
    /// elements long.
    pub fn new(v: Vec<Element<T>>) -> Self {
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
    pub fn iter<'a>(&'a self) -> impl Iterator<Item=&'a Element<T>> {
        self.0.iter()
    }

    fn into_inner(self) -> seq_impl::Inner<T> {
        self.0
    }

    /// Transforms each contained atom using the supplied function or closure
    /// to produce a new sequence.
    pub fn map_atoms<U, F>(self, f: F) -> Sequence<U>
        where F: Fn(T) -> U,
              U: Atom
    {
        self.0.into_iter().map(|elt| elt.map_atoms(&f)).collect()
    }
}

impl<T: Atom> Reduce for Sequence<T> {
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

impl<T: Atom> Index<usize> for Sequence<T> {
    type Output = Element<T>;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }

}

impl<T: Atom> FromIterator<Element<T>> for Sequence<T> {
    fn from_iter<I>(iter: I) -> Self
        where I: IntoIterator<Item=Element<T>>
    {
        Sequence(iter.into_iter().collect())
    }
}


impl<T: Atom> set::Contains<T> for Sequence<T> {
    /// A sequence always contains at least two elements, and thus will never
    /// match a single atom.
    #[inline(always)]
    fn contains(&self, _: T) -> bool {
        false
    }
}

impl<T: Atom> set::IsSubsetOf<T> for Sequence<T> {
    /// A sequence is a subset of a particular atom whenever the sequence has
    /// length one, and the first element is a subset of that atom.
    #[inline(always)]
    fn is_subset_of(&self, atom: &T) -> bool {
        self.len() == 1 && self[0].is_subset_of(atom)
    }
}

// A sequence `A` can be a subset of another sequence `B` *if* both have the
// same length and each element in `A` is a subset of the corresponding element
// in `B`:
//
// A ⊂ B ⇔ |A|=|B|=n ∧ Aᵢ ⊂ Bᵢ ∀ i∈[0,n]
impl<T: Atom> set::IsSubsetOf<Sequence<T>> for Sequence<T> {
    fn is_subset_of(&self, other: &Sequence<T>) -> bool {
        other.len() == self.len() &&
            self.iter().zip(other.iter()).all(|(u, v)| u.is_subset_of(v))
    }
}

impl<T: Atom> set::IsSubsetOf<Class<T>> for Sequence<T> {
    /// A sequence is never a subset of an atom class because sequences are
    /// guaranteed to contain at least two elements.
    #[inline(always)]
    fn is_subset_of(&self, _: &Class<T>) -> bool {
        false
    }
}

impl<T: Atom> set::IsSubsetOf<Union<T>> for Sequence<T> {
    /// A sequence is a subset of a union whenever it is a subset of one of the
    /// union's members.
    fn is_subset_of(&self, union: &Union<T>) -> bool {
        union.iter().any(|m| self.is_subset_of(m))
    }
}

impl<T: Atom> set::IsSubsetOf<Repetition<T>> for Sequence<T> {
    /// A sequence is a subset of a repetition if the sequence's length is
    /// contained by the repetition's count and all members of the sequence are
    /// subsets of the repeated element.
    fn is_subset_of(&self, rep: &Repetition<T>) -> bool {
        rep.count().contains(self.len()) && self.iter().all(|elt| elt.is_subset_of(rep.element()))
    }
}

impl<T: Atom> set::IsSubsetOf<Element<T>> for Sequence<T> {
    fn is_subset_of(&self, elt: &Element<T>) -> bool {
        match elt {
            &Element::Tagged{ref element, ..} => self.is_subset_of(&**element),
            &Element::Sequence(ref s) => self.is_subset_of(s),
            &Element::Union(ref u) => self.is_subset_of(u),
            &Element::Repeat(ref r) => self.is_subset_of(r),

            &Element::Anchor(_) => false,
            &Element::Atom(_) => false,
            &Element::Class(_) => false,
            &Element::Wildcard => false,
        }
    }
}
