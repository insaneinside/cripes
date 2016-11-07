//! Data structure and implementations for repeated pattern elements.

use std::usize;
use std::fmt::{self, Debug, Display};

use std::ops::{Range, RangeInclusive, RangeFull, RangeFrom, RangeTo, RangeToInclusive};

#[cfg(feature = "regex")] use regex_syntax::Repeater;

use util::set;
use super::{Anchor, Atom, Element, Sequence, Union};
#[cfg(feature = "pattern_class")]
use super::Class;
use super::{AtomicLen, SizeBound};

/// Repeated pattern element.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct Repetition<T> {
    /// Value being repeated
    value: T,

    /// Number of times the value should be repeated
    count: RepeatCount
}

/// Specification of the number of times a repeated element is to be repeated
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum RepeatCount {
    /// Element occurs as many times as it occurs (if it does indeed occur)
    Any,

    /// Element occurs *exactly* N times
    Exact(usize),

    /// Element occurs *at least* N times (N..)
    AtLeast(usize),

    /// Element occurs *at most* N times (..N)
    AtMost(usize),

    /// Element occurs between M and N times inclusive (M..(N + 1))
    Between(usize, usize)
}

// ----------------------------------------------------------------

impl<T> Repetition<T> {
    /// Create a new repetition object for the given value and repeat count
    pub fn new(value: T, count: RepeatCount) -> Self {
        if count.min() == 1 && count.max() == Some(1) {
            panic!("Repetition count must not be exactly 1");
        }
        Repetition{value: value, count: count}
    }

    /// Get a copy of the repetition-count object
    pub fn count(&self) -> RepeatCount {
        self.count
    }

    /// Get a reference to the repeated element
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Consume the Repetition object, returning the contained value and
    /// repetition count.
    pub fn into_inner(self) -> (T, RepeatCount) {
        (self.value, self.count)
    }

    /// Produce a repetition using the same element and different repeat count,
    /// consuming the original.
    pub fn with_count(self, new_count: RepeatCount) -> Self {
        Repetition{value: self.value, count: new_count}
    }

}

impl<T: Atom> AtomicLen for Repetition<Element<T>> {
    fn atomic_len(&self) -> SizeBound {
        let b = self.value().atomic_len();
        let (min, max) = (self.count.min() * b.min(), self.count.max().map(|v| v * b.max()));
        if let Some(max) = max {
            if min == max { SizeBound::Exact(min) }
            else { SizeBound::Range(min, max) }
        } else {
            SizeBound::Range(min, ::std::usize::MAX)
        }
    }
}

impl<T, U> super::MapAtoms<T, U> for Repetition<Element<T>>
    where T: Atom, U: Atom
{
    type Output = Repetition<Element<U>>;

    /// Transforms each contained atom using the supplied function or closure
    /// to produce a new repeated element.
    fn map_atoms<F>(self, f: F) -> Self::Output
        where F: Fn(T) -> U,
    {
        Repetition{value: self.value.map_atoms(f), count: self.count}
    }
}

impl<T> Display for Repetition<T> where T: Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ({:?})", self.value, self.count)
    }
}

impl<T> set::Contains<T> for Repetition<T>
    where T: PartialEq + set::Contains<T> {
    fn contains(&self, atom: T) -> bool {
        self.value.contains(atom) && self.count.contains(1)
    }
}


impl<T: Atom> set::Contains<T> for Repetition<Element<T>>
    where T: PartialEq {
    fn contains(&self, atom: T) -> bool {
        self.value.contains(atom) && self.count.contains(1)
    }
}


// ----------------------------------------------------------------
// set::IsSubsetOf

impl<T: Atom> set::IsSubsetOf<T> for Repetition<Element<T>> {
    /// A repetition is never a subset of an atom because a repetition's repeat
    /// count is guaranteed to never be equal to {1}.
    #[inline(always)]
    fn is_subset_of(&self, _: &T) -> bool {
        false
    }
}


impl<T: Atom> set::IsSubsetOf<Anchor<T>> for Repetition<Element<T>> {
    /// A repetition is never a subset of an anchor.
    #[inline(always)]
    fn is_subset_of(&self, _: &Anchor<T>) -> bool {
        false
    }
}
#[cfg(feature = "pattern_class")]
impl<T: Atom> set::IsSubsetOf<Class<T>> for Repetition<Element<T>> {
    /// A repetition is never a subset of an atom class because a repetition's
    /// repeat count is guaranteed to never be equal to {1}.
    #[inline(always)]
    fn is_subset_of(&self, _: &Class<T>) -> bool {
        false
    }
}

impl<T, U> set::IsSubsetOf<Repetition<U>> for Repetition<T>
    where T: set::IsSubsetOf<U>
{
    /// A repetition A is a subset of another repetition B if both repeat the
    /// same element and A's repetition count is a subset of B's
    /// repetition count.
    fn is_subset_of(&self, rep: &Repetition<U>) -> bool {
        self.value.is_subset_of(&rep.value) && self.count.is_subset_of(&rep.count)
    }
}

impl<T, M> set::IsSubsetOf<Union<M>> for Repetition<T>
    where Repetition<T>: set::IsSubsetOf<M> {
    /// A repetition is a subset of a union if it is a subset of any of the
    /// union's members.
    fn is_subset_of(&self, union: &Union<M>) -> bool {
        union.iter().any(|m| self.is_subset_of(m))
    }
}

impl<T, M> set::IsSubsetOf<Sequence<M>> for Repetition<T>
    where T: set::IsSubsetOf<M> {
    /// A repetition is a subset of a sequence if it is a subset of any of the
    /// sequence's members.
    fn is_subset_of(&self, seq: &Sequence<M>) -> bool {
        self.count.min() == seq.len() && self.count.max() == Some(seq.len()) &&
            seq.iter().all(|elt| self.value.is_subset_of(elt))
    }
}


impl<T: Atom> set::IsSubsetOf<Element<T>> for Repetition<Element<T>> {
    fn is_subset_of(&self, elt: &Element<T>) -> bool {
        match elt {
            // A repetition cannot be a subset of any single-atom pattern
            // because the repeat count is guaranteed to be a superset of {1}.
            &Element::Wildcard => false,
            &Element::Anchor(ref a) => self.is_subset_of(a),
            &Element::Atom(ref a) => self.is_subset_of(a),
            #[cfg(feature = "pattern_class")]
            &Element::Class(ref c) => self.is_subset_of(c),
            &Element::Sequence(ref s) => self.is_subset_of(s),
            &Element::Union(ref u) => self.is_subset_of(u),
            &Element::Repeat(ref r) => self.is_subset_of(&**r),
            &Element::Tagged{ref element, ..} => self.is_subset_of(&**element),
            &Element::Not(ref element) => ! self.is_subset_of(&**element),
        }
    }
}

// ----------------------------------------------------------------
// RepeatCount implementations


impl set::Contains<usize> for RepeatCount {
    fn contains(&self, n: usize) -> bool {
        n >= self.min() && n <= self.max().unwrap_or(usize::MAX)
    }
}

impl set::IsSubsetOf<RepeatCount> for RepeatCount {
    fn is_subset_of(&self, count: &RepeatCount) -> bool {
        self.min() >= count.min() && match count.max() {
            None => true,
            Some(cmax) => match self.max() {
                None => false,
                Some(smax) => smax <= cmax
            }
        }
    }
}

impl RepeatCount {
    /// Get the minimum number of repetitions allowed
    pub fn min(&self) -> usize {
        match self {
            &RepeatCount::Any |
            &RepeatCount::AtMost(_)
                => 0,
            &RepeatCount::Exact(n) |
            &RepeatCount::AtLeast(n) |
            &RepeatCount::Between(n, _)
                => n
        }
    }
    /// Get the maximum number of repetitions allowed
    pub fn max(&self) -> Option<usize> {
        match self {
            &RepeatCount::Any |
            &RepeatCount::AtLeast(_)
                => None,
            &RepeatCount::Exact(n) |
            &RepeatCount::AtMost(n) |
            &RepeatCount::Between(_, n)
                => Some(n)
        }
    }
}

impl From<usize> for RepeatCount {
    fn from(n: usize) -> Self {
        RepeatCount::Exact(n)
    }
}

impl From<Range<usize>> for RepeatCount {
    fn from(r: Range<usize>) -> Self {
        RepeatCount::Between(r.start, r.end - 1)
    }
}

impl From<RangeInclusive<usize>> for RepeatCount {
    fn from(r: RangeInclusive<usize>) -> Self {
        match r {
            RangeInclusive::Empty{at}
                => RepeatCount::Exact(at),
            RangeInclusive::NonEmpty{start, end}
                => RepeatCount::Between(start, end)
        }
    }
}

impl From<RangeFull> for RepeatCount {
    fn from(_: RangeFull) -> Self {
        RepeatCount::Any
    }
}

impl From<RangeTo<usize>> for RepeatCount {
    fn from(r: RangeTo<usize>) -> Self {
        RepeatCount::AtMost(r.end - 1)
    }
}

impl From<RangeToInclusive<usize>> for RepeatCount {
    fn from(r: RangeToInclusive<usize>) -> Self {
        RepeatCount::AtMost(r.end)
    }
}

impl From<RangeFrom<usize>> for RepeatCount {
    fn from(r: RangeFrom<usize>) -> Self {
        RepeatCount::AtLeast(r.start)
    }
}

#[cfg(feature = "regex")]
impl From<Repeater> for RepeatCount {
    fn from(r: Repeater) -> Self {
        match r {
            Repeater::ZeroOrOne => RepeatCount::AtMost(1),
            Repeater::ZeroOrMore => RepeatCount::Any,
            Repeater::OneOrMore => RepeatCount::AtLeast(1),
            Repeater::Range{min, max} => {
                if min > 0 {
                    if let Some(max) = max {
                        if max != min { RepeatCount::Between(min as usize, max as usize) }
                        else { RepeatCount::Exact(min as usize) }
                    } else {
                        RepeatCount::AtLeast(min as usize)
                    }
                } else if let Some(max) = max {
                    RepeatCount::AtMost(max as usize)
                } else {
                    RepeatCount::Any
                }
            }
        }
    }
}

impl Debug for RepeatCount {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &RepeatCount::Any => write!(f, "any number of times"),
            &RepeatCount::Exact(n) => write!(f, "{} {}", n, if n == 1 { "time" } else { "times" }),
            &RepeatCount::AtLeast(n) => write!(f, "at least {} {}", n, if n == 1 { "time" } else { "times" }),
            &RepeatCount::AtMost(n) => write!(f, "at most {} {}", n, if n == 1 { "time" } else { "times" }),
            &RepeatCount::Between(m, n)=> write!(f, "{}-{} times", m, n)
        }
    }
}


