//! Data structures and implementations for union-of-patterns.

use std::mem;
use std::usize;
use std::cmp::Ordering;
use std::iter::FromIterator;

use itertools::Itertools;

use util::set::{self, IsSubsetOf};
use super::{Atom, Element};
#[cfg(feature = "pattern_class")]
use super::Class;

use super::{Reduce, flatten_and_reduce};
use super::SizeBound;

// To hide the implementation details, we wrap the type alias in
// a private submodule.
mod union_impl {
    pub type Inner<T> = Vec<T>;
}

/// Union of patterns.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct Union<T>(union_impl::Inner<T>);

impl<T> Union<T> {
    /// Create a new union from the given vector.
    pub fn new(v: Vec<T>) -> Self {
        if v.len() < 2 {
            panic!("Unions must have at least two members");
        }
        Union(v)
    }

    /// Fetch an iterator over the members of the union
    #[inline]
    pub fn iter<'a>(&'a self) -> impl Iterator<Item=&'a T> {
        self.0.iter()
    }

    /// Produce an iterator over the members of the union, consuming it.
    pub fn into_iter(self) -> impl Iterator<Item=T> {
        self.0.into_iter()
    }

    /// Get the number of members in the union
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline(always)]
    fn into_inner(self) -> union_impl::Inner<T> {
        self.0
    }

    /// Add a member ot the union
    pub fn push(&mut self, t: T) {
        self.0.push(t);
    }
}

impl<T: Atom> super::AtomicLen for Union<Element<T>> {
    fn atomic_len(&self) -> SizeBound {
        use std::cmp::{min, max};

        let b = self.iter()
            .map(|elt| elt.atomic_len())
            .fold((usize::MAX, usize::MIN),
                  |accum, eb| (min(accum.0, eb.min()),
                               max(accum.1, eb.max())));
        if b.0 == b.1 { SizeBound::Exact(b.0) }
        else { SizeBound::Range(b.0, b.1) }
    }
}

impl<T, U> super::MapAtoms<T, U> for Union<Element<T>>
    where T: Atom, U: Atom {
    type Output = Union<Element<U>>;

    /// Transforms each contained atom using the supplied function or closure
    /// to produce a new sequence.
    fn map_atoms<F>(self, f: F) -> Union<Element<U>>
        where F: Fn(T) -> U,
    {
        self.0.into_iter().map(|elt| elt.map_atoms(&f)).collect()
    }
}



impl<T: Atom> Reduce for Union<Element<T>> {
    type Output = Option<Element<T>>;

    /// Flatten the union and apply implementation-defined optimizations.
    fn reduce(mut self) -> Self::Output {
        if self.0.is_empty() { None }
        else {
            // Reduce all child elements and flatten unions into self.
            flatten_and_reduce(&mut self.0, Element::Wildcard,
                               |elt| match elt { &Element::Union(ref u) => Some(u.len()), _ => None },
                               |elt| match elt { Element::Union(u) => u.into_inner(), _ => unreachable!() });

            // Remove elements that are subsets of others.
            let mut reduced = self.0.clone().into_iter()
                .sorted_by(|a, b| { if a.is_subset_of(b) || b.is_subset_of(a) { Ordering::Equal }
                                    else { a.cmp(b) } })
                .into_iter()
                .coalesce(|a, b| { if a.is_subset_of(&b) { Ok(b) }
                                   else if b.is_subset_of(&a) { Ok(a) }
                                   else { Err((a, b)) } })
                .collect::<Vec<_>>();

            match reduced.len() {
                0 => None,
                1 => Some(reduced.swap_remove(0)),
                _ => {
                    mem::swap(&mut self.0, &mut reduced);
                    Some(Element::Union(self))
                }
            }
        }
    }
}


impl<T> FromIterator<T> for Union<T> {
    fn from_iter<I>(iter: I) -> Self
        where I: IntoIterator<Item=T>
    {
        Union::new(iter.into_iter().collect())
    }
}

// ----------------------------------------------------------------
apply_attrs! {
    cfg(feature = "regex") => {
        use regex_syntax;
        use char_iter;
        use super::ByteOrChar;

        macro_rules! impl_union_from {
            ($T: ty, $from: ty, $map_input: ident => $map_expr: expr) => {
                impl From<$from> for Union<Element<$T>> {
                    fn from(c: $from) -> Self {
                        Union::from_iter(c.into_iter().flat_map(|$map_input| $map_expr))
                    }
                }
            };
        }

        impl_union_from!(char, regex_syntax::CharClass, cr => char_iter::new(cr.start, cr.end).map(|c| c.into()));
        impl_union_from!(u8, regex_syntax::ByteClass, cr => (cr.start...cr.end).map(|c| c.into()));
        impl_union_from!(ByteOrChar, regex_syntax::CharClass, cr => char_iter::new(cr.start, cr.end).map(|c| c.into()));
        impl_union_from!(ByteOrChar, regex_syntax::ByteClass, cr => (cr.start...cr.end).map(|c| c.into()));
    }
}

// ----------------------------------------------------------------

impl<T> set::Contains<T> for Union<T>
    where T: PartialEq {
    fn contains(&self, v: T) -> bool {
        self.0.iter().any(|m| *m == v)
    }
}

impl<T: Atom> set::Contains<T> for Union<Element<T>> {
    fn contains(&self, atom: T) -> bool {
        self.0.iter().any(|m| m.contains(atom))
    }
}


/*
impl<T: Atom, U: Atom> set::IsSubsetOf<Sequence<Element<U>>> for Union<Element<T>> {
    /// A union is a subset of a sequence S if it consists entirely of
    /// sequences whose elements are subsets of the corresponding element in S.
    fn is_subset_of(&self, seq: &Sequence<Element<T>>) -> bool {
        self.iter().all(|m| match m {
            &Element::Sequence(ref member_seq) =>
                seq.len() == member_seq.len() &&
                seq.iter().zip(member_seq.iter()).all(|(seq_elt, member_seq_elt)| member_seq_elt.is_subset_of(seq_elt)),
            _ => false })
    }
}
*/



impl<T, U> set::IsSubsetOf<U> for Union<T>
    where T: set::IsSubsetOf<U>
{
    /// In general, a union is a subset of another pattern if all of its
    /// members are subsets of the pattern.
    fn is_subset_of(&self, other: &U) -> bool {
        self.iter().all(|m| m.is_subset_of(other))
    }
}
