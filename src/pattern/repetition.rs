//! Data structure and implementations for repeated pattern elements.

use std::usize;
use std::fmt::{self, Debug, Display};

use regex_syntax::Repeater;

use util::set;
use super::{Anchor, Atom, Class, Element, Sequence, Union};


/// Repeated pattern element.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct Repetition<T: Atom> {
    /// Element being repeated
    element: Box<Element<T>>,

    /// Number of times the element should be repeated
    count: RepeatCount
}

impl<T: Atom> Repetition<T> {
    /// Create a new repetition object for the given element and repeat count
    pub fn new(elt: Element<T>, count: RepeatCount) -> Self {
        if count.min() == 1 && count.max() == Some(1) {
            panic!("Repetition count must not be exactly 1");
        }
        Repetition{element: Box::new(elt), count: count}
    }

    /// Get a copy of the repetition-count object
    pub fn count(&self) -> RepeatCount {
        self.count
    }

    /// Get a reference to the repeated element
    pub fn element(&self) -> &Element<T> {
        &*self.element
    }

    /// Consume the Repetition object, returning the contained boxed element
    /// and repetition count.
    pub fn into_inner(self) -> (Box<Element<T>>, RepeatCount) {
        (self.element, self.count)
    }

    /// Produce a repetition using the same element and different repeat count,
    /// consuming the original.
    pub fn with_count(self, new_count: RepeatCount) -> Self {
        Repetition{element: self.element, count: new_count}
    }

    /// Transforms each contained atom using the supplied function or closure
    /// to produce a new repeated element.
    pub fn map_atoms<U, F>(self, f: F) -> Repetition<U>
        where F: Fn(T) -> U,
              U: Atom
    {
        Repetition{element: Box::new(self.element.map_atoms(f)), count: self.count}
    }
}

impl<T: Atom> Display for Repetition<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} ({:?})", self.element, self.count)
    }
}

impl<T: Atom> set::Contains<T> for Repetition<T> {
    fn contains(&self, atom: T) -> bool {
        self.element.contains(atom) && self.count.contains(1)
    }
}


// ----------------------------------------------------------------
// set::IsSubsetOf

impl<T: Atom> set::IsSubsetOf<T> for Repetition<T> {
    /// A repetition is never a subset of an atom because a repetition's repeat
    /// count is guaranteed to never be equal to {1}.
    #[inline(always)]
    fn is_subset_of(&self, _: &T) -> bool {
        false
    }
}


impl<T: Atom> set::IsSubsetOf<Anchor<T>> for Repetition<T> {
    /// A repetition is never a subset of an anchor.
    #[inline(always)]
    fn is_subset_of(&self, _: &Anchor<T>) -> bool {
        false
    }
}
impl<T: Atom> set::IsSubsetOf<Class<T>> for Repetition<T> {
    /// A repetition is never a subset of an atom class because a repetition's
    /// repeat count is guaranteed to never be equal to {1}.
    #[inline(always)]
    fn is_subset_of(&self, _: &Class<T>) -> bool {
        false
    }
}

impl<T: Atom> set::IsSubsetOf<Repetition<T>> for Repetition<T> {
    /// A repetition A is a subset of another repetition B if both repeat the
    /// same element and A's repetition count is a subset of B's
    /// repetition count.
    fn is_subset_of(&self, rep: &Repetition<T>) -> bool {
        self.element.is_subset_of(&*rep.element) && self.count.is_subset_of(&rep.count)
    }
}

impl<T: Atom> set::IsSubsetOf<Union<T>> for Repetition<T> {
    /// A repetition is a subset of a union if it is a subset of any of the
    /// union's members.
    fn is_subset_of(&self, union: &Union<T>) -> bool {
        union.iter().any(|m| self.is_subset_of(m))
    }
}

impl<T: Atom> set::IsSubsetOf<Sequence<T>> for Repetition<T> {
    /// A repetition is a subset of a sequence if it is a subset of any of the
    /// sequence's members.
    fn is_subset_of(&self, seq: &Sequence<T>) -> bool {
        self.count.min() == seq.len() && self.count.max() == Some(seq.len()) &&
            seq.iter().all(|elt| self.element.is_subset_of(elt))
    }
}


impl<T: Atom> set::IsSubsetOf<Element<T>> for Repetition<T> {
    fn is_subset_of(&self, elt: &Element<T>) -> bool {
        match elt {
            // A repetition cannot be a subset of any single-atom pattern
            // because the repeat count is guaranteed to be a superset of {1}.
            &Element::Wildcard => false,
            &Element::Anchor(ref a) => self.is_subset_of(a),
            &Element::Atom(ref a) => self.is_subset_of(a),
            &Element::Class(ref c) => self.is_subset_of(c),
            &Element::Sequence(ref s) => self.is_subset_of(s),
            &Element::Union(ref u) => self.is_subset_of(u),
            &Element::Repeat(ref r) => self.is_subset_of(r),
            &Element::Tagged{ref element, ..} => self.is_subset_of(&**element),
        }
    }
}

// ----------------------------------------------------------------

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


