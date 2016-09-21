//! Data structures and implementations for union-of-patterns.

use std::mem;
use std::cmp::Ordering;
use std::iter::FromIterator;

use itertools::Itertools;

use util::set::{self, IsSubsetOf};
use super::{Anchor, Atom, Element, Repetition, Sequence};
#[cfg(feature = "pattern_class")]
use super::Class;

use super::{Reduce, flatten_and_reduce};

// To hide the implementation details, we wrap the type alias in
// a private submodule.
mod union_impl {
    use super::super::Element;
    pub type Inner<T> = Vec<Element<T>>;
}

/// Union of patterns.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct Union<T: Atom>(union_impl::Inner<T>);

impl<T: Atom> Union<T> {
    /// Create a new union from the given vector.
    pub fn new(v: Vec<Element<T>>) -> Self {
        if v.len() < 2 {
            panic!("Unions must have at least two members");
        }
        Union(v)
    }

    /// Fetch an iterator over the members of the union
    #[inline]
    pub fn iter<'a>(&'a self) -> impl Iterator<Item=&'a Element<T>> {
        self.0.iter()
    }

    /// Get the number of members in the union
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Transforms each contained atom using the supplied function or closure
    /// to produce a new sequence.
    pub fn map_atoms<U, F>(self, f: F) -> Union<U>
        where F: Fn(T) -> U,
              U: Atom
    {
        self.0.into_iter().map(|elt| elt.map_atoms(&f)).collect()
    }

    #[inline(always)]
    fn into_inner(self) -> union_impl::Inner<T> {
        self.0
    }
}

impl<T: Atom> Reduce for Union<T> {
    type Output = Option<Element<T>>;

    /// Flatten the union and apply implementation-defined optimizations.
    fn reduce(mut self) -> Self::Output {
        if self.0.is_empty() { None }
        else {
            flatten_and_reduce(&mut self.0, Element::Wildcard,
                               |elt| match elt { &Element::Union(ref u) => Some(u.len()), _ => None },
                               |elt| match elt { Element::Union(u) => u.into_inner(), _ => unreachable!() });

            let mut reduced = self.0.clone().into_iter()
                .sorted_by(|a, b| { if a.is_subset_of(b) || b.is_subset_of(a) { Ordering::Equal }
                                    else { a.cmp(b) } })
                .into_iter()
                .coalesce(|a, b| { if a.is_subset_of(&b) { Ok(b) }
                                   else if b.is_subset_of(&a) { Ok(a) }
                                   else { Err((a, b)) } })
                .collect();
            mem::swap(&mut self.0, &mut reduced);
            Some(Element::Union(self))
        }
    }
}

impl<T: Atom> set::Contains<T> for Union<T> {
    fn contains(&self, atom: T) -> bool {
        self.0.iter().any(|m| m.contains(atom))
    }
}

impl<T: Atom> FromIterator<Element<T>> for Union<T> {
    fn from_iter<I>(iter: I) -> Self
        where I: IntoIterator<Item=Element<T>>
    {
        Union::new(iter.into_iter().collect())
    }
}

impl<T: Atom> set::IsSubsetOf<Sequence<T>> for Union<T> {
    /// A union is a subset of a sequence S if it consists entirely of
    /// sequences whose elements are subsets of the corresponding element in S.
    fn is_subset_of(&self, seq: &Sequence<T>) -> bool {
        self.iter().all(|m| match m {
            &Element::Sequence(ref member_seq) =>
                seq.len() == member_seq.len() &&
                seq.iter().zip(member_seq.iter()).all(|(seq_elt, member_seq_elt)| member_seq_elt.is_subset_of(seq_elt)),
            _ => false })
    }
}

macro_rules! is_subset_if_all_members_are_subsets {
    ($T: ident,$Other: ty, $name: ident; $doc: expr) => {
        impl<$T: Atom> set::IsSubsetOf<$Other> for Union<$T> {
            #[doc = $doc]
            fn is_subset_of(&self, $name: &$Other) -> bool {
                self.iter().all(|m| m.is_subset_of($name))
            }
        }
    };
}

// This one should never happen, but we'll happily support whatever brain-dead
// comparisons you want to try!
is_subset_if_all_members_are_subsets! {
    T, T, atom;
    "A union is a subset of an atom if all members are subsets of that atom"}

// Also unlikely.
is_subset_if_all_members_are_subsets! {
    T, Anchor<T>, anchor;
    "A union is a subset of an anchor if all members of the union are subsets of the anchor"}


is_subset_if_all_members_are_subsets! {
    T, Repetition<T>, repetition;
    "A union is a subset of a repetition if all members of the union are subsets of the repetition"}

#[cfg(feature = "pattern_class")]
is_subset_if_all_members_are_subsets! {
    T, Class<T>, class;
    "A union is a subset of a class of atoms if all members of the union are subsets of the class"}

is_subset_if_all_members_are_subsets! {
    T, Union<T>, union;
    "A union is a subset of another union if all members of the former are subsets of the latter"}

impl<T: Atom> set::IsSubsetOf<Element<T>> for Union<T> {
    fn is_subset_of(&self, elt: &Element<T>) -> bool {
        match elt {
            &Element::Tagged{ref element, ..} => self.is_subset_of(&**element),
            #[cfg(feature = "pattern_class")]
            &Element::Class(ref c) => self.is_subset_of(c),
            &Element::Sequence(ref s) => self.is_subset_of(s),
            &Element::Repeat(ref r) => self.is_subset_of(r),
            &Element::Union(ref u) => self.is_subset_of(u),
            &Element::Atom(atom) => self.is_subset_of(&atom),
            &Element::Anchor(ref a) => self.is_subset_of(a),

            // A union is a subset of a wildcard if all members are
            // also subsets.
            &Element::Wildcard => self.iter().all(|m| m.is_subset_of(&Element::Wildcard)),

            &Element::Not(ref element) => ! self.is_subset_of(&**element),
        }
    }
}
