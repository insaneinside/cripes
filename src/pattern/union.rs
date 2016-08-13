//! Data structures and implementations for union-of-patterns.

use std::mem;
use std::cmp::Ordering;
use std::iter::FromIterator;

use itertools::Itertools;

use util::set::{self, IsSubsetOf};
use super::{Atom, Class, Element, Repetition, Sequence, flatten_vec};

// To hide the implementation details, we wrap the type alias in
// a private submodule.
mod union_impl {
    use std::slice;
    use super::super::Element;
    pub type Inner<T> = Vec<Element<T>>;
    pub type Iter<'a,T> = slice::Iter<'a,Element<T>>;
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
    pub fn iter<'a>(&'a self) -> UnionIterator<'a,T> {
        UnionIterator(self.0.iter())
    }

    /// Get the number of members in the union
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

    fn into_inner(self) -> union_impl::Inner<T> {
        self.0
    }

    /// Flatten the union and apply implementation-defined optimizations.
    pub fn reduce(&mut self) {
        for elt in self.0.iter_mut() {
            elt.reduce();
        }
        flatten_vec(&mut self.0,
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
            &Element::Sequence(ref s) => 
                seq.len() == s.len() &&
                seq.iter().zip(s.iter()).all(|(seq_elt, s_elt)| s_elt.is_subset_of(seq_elt)),
            _ => false })
    }
}

macro_rules! is_subset_if_all_members_are_subsets {
    ($T: ident,$Other: ty, $name: ident) => {
        impl<$T: Atom> set::IsSubsetOf<$Other> for Union<$T> {
            /// A union is a subset of a $name if all members are subsets.
            fn is_subset_of(&self, $name: &$Other) -> bool {
                self.iter().all(|m| m.is_subset_of($name))
            }
        }
    };
}

is_subset_if_all_members_are_subsets!(T, Repetition<T>, repetition);
is_subset_if_all_members_are_subsets!(T, Class<T>, class);
is_subset_if_all_members_are_subsets!(T, Union<T>, union);

impl<T: Atom> set::IsSubsetOf<Element<T>> for Union<T> {
    fn is_subset_of(&self, elt: &Element<T>) -> bool {
        match elt {
            &Element::Tagged{ref element, ..} => self.is_subset_of(&**element),
            &Element::Class(ref c) => self.is_subset_of(c),
            &Element::Sequence(ref s) => self.is_subset_of(s),
            &Element::Repeat(ref r) => self.is_subset_of(r),
            &Element::Union(ref u) => self.is_subset_of(u),

            // A union is a subset of a wildcard if all members are
            // also subsets.
            &Element::Wildcard => self.iter().all(|m| m.is_subset_of(&Element::Wildcard)),

            // Unions always contain at least two unique members, thus they
            // cannot be subsets of single atoms.
            &Element::Atom(_) => false,
            &Element::Anchor(_) => false,
        }
    }
}


/// Iterator over the patterns within a union.
pub struct UnionIterator<'a,T: 'a + Atom>(union_impl::Iter<'a,T>);

impl<'a,T: 'a + Atom> Iterator for UnionIterator<'a,T> {
    type Item = &'a Element<T>;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}