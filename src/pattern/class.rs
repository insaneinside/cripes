//! Atom-class implementation.

use std::ops::{BitXor, Sub, Range, RangeInclusive};
use std::iter::FromIterator;
use std::fmt::{self, Debug};

use regex_syntax;
use itertools::Itertools;

use super::{Anchor, Atom, ByteOrChar, Element, Repetition, Sequence, Union};
use super::{Distance, Step};
use util::set::{self, Contains};

// ----------------------------------------------------------------
// Class

/// Any member of a class of atoms
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub enum ClassMember<T: Atom> {
    /// A single atom
    Atom(T),

    /// A range of atoms
    Range(T, T)
}

impl<T: Atom> ClassMember<T> {
    /// Map the class-member's atoms to a different type
    pub fn map_atoms<U, F>(self, f: F) -> ClassMember<U>
        where F: Fn(T) -> U,
              U: Atom
    {
        match self {
            ClassMember::Atom(a) => ClassMember::Atom(f(a)),
            ClassMember::Range(a, b) => ClassMember::Range(f(a), f(b))
        }
    }

    /// Get the number of individual atoms that this class member represents.
    pub fn len(&self) -> usize
        where T: Distance
    {
        match self {
            &ClassMember::Atom(_) => 1,
            &ClassMember::Range(first, last) => last.distance(&first) + 1,
        }
    }

    /// Fetch an iterator over each discrete value contained by this member.
    #[inline]
     pub fn iter(&self) -> impl Iterator<Item=T>
        where T: Step {
        match self {
            &ClassMember::Atom(a) => AtomIter(a...a),
            &ClassMember::Range(a, b) => AtomIter(a...b),
        }
    }
}

impl<T: Atom> set::Contains<T> for ClassMember<T> {
    /// Check if the member is or contains a particular atom.
    fn contains(&self, x: T) -> bool {
        match self {
            &ClassMember::Atom(a) => x == a,
            &ClassMember::Range(a, b) => (a...b).contains(x),
        }
    }
}



/// Iterator over the atoms specified by a class member.
struct AtomIter<T: Atom>(RangeInclusive<T>);

impl<T: Atom> Iterator for AtomIter<T>
        where T: Step
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            RangeInclusive::NonEmpty{start, end} => {
                let next_start = start.increment();
                if next_start > end {
                    self.0 = RangeInclusive::Empty{at: end};
                } else {
                    self.0 = RangeInclusive::NonEmpty{start: next_start, end: end};
                }
                Some(start) },
            RangeInclusive::Empty{..} => None
        }
    }
}


/*impl<T: Atom> Debug for ClassMember<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ClassMember::Atom(a) => write!(f, "{:?}", a),
            ClassMember::Range(first, last) => write!(f, "{:?}-{:?}", first, last)
        }
    }
}*/

impl<T: Atom> From<T> for ClassMember<T> {
    fn from(a: T) -> Self { ClassMember::Atom(a) }
}

impl From<regex_syntax::ClassRange> for ClassMember<ByteOrChar> {
    fn from(cr: regex_syntax::ClassRange) -> Self {
        ClassMember::Range(cr.start.into(), cr.end.into())
    }
}

impl From<regex_syntax::ByteRange> for ClassMember<ByteOrChar> {
    fn from(cr: regex_syntax::ByteRange) -> Self {
        ClassMember::Range(cr.start.into(), cr.end.into())
    }
}


impl From<regex_syntax::ClassRange> for ClassMember<char> {
    fn from(cr: regex_syntax::ClassRange) -> Self {
        ClassMember::Range(cr.start, cr.end)
    }
}


impl From<regex_syntax::ByteRange> for ClassMember<u8> {
    fn from(cr: regex_syntax::ByteRange) -> Self {
        ClassMember::Range(cr.start, cr.end)
    }
}

impl<T: Atom> From<Range<T>> for ClassMember<T>
    where T: Sub<usize,Output=T> + Sub<T,Output=usize>
{
    fn from(r: Range<T>) -> Self {
        if r.end - r.start > 1 { ClassMember::Range(r.start, r.end - 1usize) }
        else { ClassMember::Atom(r.start) }
    }
}
// ----------------------------------------------------------------

/// A set of atoms and/or ranges of atoms.
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct Class<T: Atom> {
    members: Vec<ClassMember<T>>
}

// FIXME: [optimize] All operations on a Class<T>, including observer and
// predicate methods, currently require at least O(N) time.  We could probably
// improve this by changing the storage representation.
impl<T: Atom> Class<T> {
    /// Create a new Class instance with members taken from the given iterator.
    pub fn new<I,U>(items: I) -> Self
        where I: IntoIterator<Item=U>,
              ClassMember<T>: From<U> {
        Class{members: Vec::from_iter(items.into_iter().map(|x| x.into()))}
    }

    /// Create a new Class instance from an iterator of ClassMember instances.
    pub fn from_members<I>(m: I) -> Self
        where I: IntoIterator<Item=ClassMember<T>>
    {
        Class{members: m.into_iter().collect()}
    }

    /// Map the atoms in the class to a different atom type.
    pub fn map_atoms<U, F>(self, f: F) -> Class<U>
        where F: Fn(T) -> U,
              U: Atom
    {
        Class::new(self.members.into_iter().map(|m| m.map_atoms(&f)))
    }

    /// Check whether the class would match a particular atom.
    pub fn matches(&self, x: T) -> bool {
        self.members.iter().any(|&range| range.contains(x))
    }

    /// Get the number of members (atoms) in the class.
    ///
    /// Ranges of atoms count as the number of atoms in each range.
    pub fn len(&self) -> usize where T: Distance {
        self.members.iter().map(|m| m.len()).sum()
    }

    /// Check if the class contains zero members.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.members.is_empty()
    }

    /// Fetch an iterator over the members specified in the class.
    pub fn iter_members<'a>(&'a self) -> impl Iterator<Item=&'a ClassMember<T>> {
        self.members.iter()
    }

    /// Fetch an iterator over the atoms specified in the class.
    pub fn iter<'a>(&'a self) -> impl Iterator<Item=T> + 'a  where T: Step {
        self.members.iter().flat_map(|m| m.iter())
    }
}

impl<T: Atom> Debug for Class<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]", self.members.iter().map(|m| format!("{:?}", m)).join(","))
    }
}

impl<'a, T: 'a + Atom> set::Contains<&'a ClassMember<T>> for Class<T> {
    fn contains(&self, cm: &'a ClassMember<T>) -> bool {
        self.iter_members().any(|m| cm == m)
    }
}
impl<T: Atom> set::Contains<T> for Class<T> {
    /// Check if any member of the class matches the given atom.
    fn contains(&self, x: T) -> bool {
        self.matches(x)
    }
}

impl<T: Atom> set::IsSubsetOf<T> for Class<T> {
    /// A class *can* be a subset of an atom -- as long as that atom is the
    /// only member of the class!
    #[inline]
    fn is_subset_of(&self, atom: &T) -> bool {
        self.contains(*atom) && self.len() == 1
    }
}

impl<T: Atom> set::IsSubsetOf<Anchor<T>> for Class<T> {
    /// A class is never a subset of an anchor.
    #[inline(always)]
    fn is_subset_of(&self, _: &Anchor<T>) -> bool {
        false
    }
}


// FIXME [optimize] I suspect that we need some sort of specialized data
// structure or clever algorithm in order to perform this test in a time better
// than O(N·M).
impl<T: Atom> set::IsSubsetOf<Class<T>> for Class<T> {
    /// A class `A` is a subset of another class `B` if all atoms in `A` are
    /// also in `B`.
    fn is_subset_of(&self, other: &Self) -> bool {
        self.iter().all(|a| other.contains(a))
    }
}
impl<T: Atom> set::IsSubsetOf<Sequence<T>> for Class<T> {
    /// Any single-atom pattern like Class cannot match the same set of inputs
    /// as a Sequence, because a Sequence is guaranteed to contain at least
    /// two elements.
    #[inline]
    fn is_subset_of(&self, _: &Sequence<T>) -> bool {
        false
    }
}

impl<T: Atom> set::IsSubsetOf<Union<T>> for Class<T> {
    /// A class is a subset of a union if all members of the class are subsets
    /// of the union.
    fn is_subset_of(&self, union: &Union<T>) -> bool {
        self.iter().all(|a| a.is_subset_of(union))
    }        
}

impl<T: Atom> set::IsSubsetOf<Repetition<T>> for Class<T> {
    fn is_subset_of(&self, rep: &Repetition<T>) -> bool {
        self.iter().all(|a| a.is_subset_of(rep.element())) && rep.count().contains(1)
    }
}


impl<T: Atom> set::IsSubsetOf<Element<T>> for Class<T> {
    /// A class is always a subset of a wildcard, and never a subset of an
    /// anchor.  See the other `IsSubsetOf` implementations for `Class` for
    /// explanations of how this trait is implements for the other variants of
    /// `Element`.
    fn is_subset_of(&self, other: &Element<T>) -> bool {
        match other {
            // Anything we can do, Wildcard can do betteeeeer...
            &Element::Wildcard => true,

            &Element::Atom(a) => self.is_subset_of(&a),
            &Element::Class(ref d) => self.is_subset_of(d),
            &Element::Tagged{ref element, ..} => self.is_subset_of(&**element),
            &Element::Sequence(ref seq) => self.is_subset_of(seq),
            &Element::Union(ref union) => self.is_subset_of(union),
            &Element::Repeat(ref rep) => self.is_subset_of(rep),
            &Element::Anchor(_) => false,
        }
    }
}
                

impl<T: Atom, U> FromIterator<U> for Class<T>
where ClassMember<T>: From<U> {
    fn from_iter<I>(iter: I) -> Self
        where I: IntoIterator<Item=U> {
        Class{members: Vec::from_iter(iter.into_iter().map(|x| x.into()))}
    }
}

macro_rules! impl_class_from {
    ($T: ty, $from: ty) => {
        impl From<$from> for Class<$T> {
            fn from(c: $from) -> Self {
                Self::new(c.into_iter().map(|cr| if cr.end != cr.start { ClassMember::Range(cr.start.into(), cr.end.into()) }
                                                 else { ClassMember::Atom(cr.start.into()) }))
            }
        }
    };
}

impl_class_from!(char, regex_syntax::CharClass);
impl_class_from!(u8, regex_syntax::ByteClass);
impl_class_from!(ByteOrChar, regex_syntax::CharClass);
impl_class_from!(ByteOrChar, regex_syntax::ByteClass);
