//! Atoms and atomic traits.

use std::{char, u8, u16, u32, u64, usize};
use std::{ascii, cmp};
use std::ops::Range;
use std::fmt::Debug;

use super::{Anchor, Class, Element, Sequence, Repetition, Union};

use util::set::{self, Contains};

/// Trait-bounds requirements for atomic values in a pattern.
pub trait Atom: Debug + Copy + Clone + Eq + Ord + Distance + Step {}
impl<T> Atom for T where T: Debug + Copy + Clone + Eq + Ord + Distance + Step {}

impl<T: Atom> set::IsSubsetOf<T> for T {
    #[inline(always)]
    fn is_subset_of(&self, other: &T) -> bool {
        self == other
    }
}
impl<T: Atom> set::IsSubsetOf<Anchor<T>> for T {
    /// An atom is never a subset of an anchor.
    #[inline(always)]
    fn is_subset_of(&self, _: &Anchor<T>) -> bool {
        false
    }
}


impl<T: Atom> set::IsSubsetOf<Class<T>> for T {
    #[inline(always)]
    fn is_subset_of(&self, class: &Class<T>) -> bool {
        class.contains(*self)
    }
}

impl<T: Atom> set::IsSubsetOf<Union<T>> for T {
    #[inline]
    fn is_subset_of(&self, union: &Union<T>) -> bool {
        union.iter().any(|m| self.is_subset_of(m))
    }
}

impl<T: Atom> set::IsSubsetOf<Sequence<T>> for T {
    #[inline]
    fn is_subset_of(&self, seq: &Sequence<T>) -> bool {
        seq.len() == 1 && self.is_subset_of(&seq[0])
    }
}

impl<T: Atom> set::IsSubsetOf<Repetition<T>> for T {
    #[inline]
    fn is_subset_of(&self, rep: &Repetition<T>) -> bool {
        self.is_subset_of(rep.element()) && rep.count().contains(1)
    }
}


impl<T: Atom> set::IsSubsetOf<Element<T>> for T {
    fn is_subset_of(&self, elt: &Element<T>) -> bool {
        match elt {
            &Element::Wildcard => true,
            &Element::Atom(a) => self.is_subset_of(&a),

            &Element::Class(ref class) => self.is_subset_of(class),
            &Element::Union(ref union) => self.is_subset_of(union),
            &Element::Repeat(ref rep) => self.is_subset_of(rep),
            &Element::Tagged{ref element, ..} => self.is_subset_of(&**element),

            &Element::Sequence(ref seq) => self.is_subset_of(seq),
            &Element::Anchor(ref anch) => self.is_subset_of(anch)
        }
    }
}


/// Trait for types whose values can be stepped through by increment/decrement
/// operatons
pub trait Step: PartialEq<Self> {
    ///  Fetch the next value after the one provided in the positive direction.
    ///  If the supplied value is already at its maximum, return it.
    fn increment(&self) -> Self;

    ///  Fetch the next value after the one provided in the negative direction.
    ///  If the supplied value is already at its minimum, return it.
    fn decrement(&self) -> Self;
}

/// Trait used to determine the distance between two atoms.
pub trait Distance {
    /// Calculate the distance between `self` and the given value, returning
    /// the number of `Self`-sized steps between them.
    fn distance(&self, b: &Self) -> usize;
}

macro_rules! impl_step_distance_for_primitives {
    ($first: tt, $($rest:tt),+) => (
        impl_step_distance_for_primitives!($first);
        impl_step_distance_for_primitives!($($rest),+);
    );
    ($tp: tt) => (
        impl Step for $tp {
            fn increment(&self) -> Self {
                if *self >= $tp::MAX { *self }
                else { ((*self) + 1) as $tp }
            }
            fn decrement(&self) -> Self {
                if *self == 0 { *self }
                else { (*self - 1) as $tp }
            }
        }
        impl Distance for $tp {
            fn distance(&self, b: &Self) -> usize {
                if *self > *b { (*self - *b) as usize }
                else { (*b - *self) as usize }
            }
        }
    );
}

impl_step_distance_for_primitives!(u8, u16, u32, u64, usize);

const SURROGATE_RANGE: Range<u32> = 0xD800..0xE000;

impl Distance for char {
    fn distance(&self, b: &Self) -> usize {
        let (a, b) = (cmp::min(*self, *b) as u32, cmp::max(*self, *b) as u32);
        let a_surrogate = SURROGATE_RANGE.contains(a);
        let b_surrogate = SURROGATE_RANGE.contains(b);

        if a_surrogate && b_surrogate { 0 }
        else if a_surrogate { b as usize }
        else if b_surrogate { a as usize }
        else if (a < SURROGATE_RANGE.start && b < SURROGATE_RANGE.start) || (a >= SURROGATE_RANGE.end && b >= SURROGATE_RANGE.end) {
            (b - a) as usize
        } else {
            (b - SURROGATE_RANGE.end + SURROGATE_RANGE.start - a) as usize
        }
    }
}

impl Step for char {
    fn increment(&self) -> Self {
        if *self >= char::MAX {
            *self
        } else {
            let mut o = *self as u32 + 1;
            if SURROGATE_RANGE.contains(o) {
                o += SURROGATE_RANGE.len() as u32;
            }

            // We've manually skipped the surrogate range and done range
            // checking above, so this *should* actually be safe.
            unsafe { char::from_u32_unchecked(o) }
        }
    }

    fn decrement(&self) -> Self {
        if *self == '\0' {
            *self
        } else {
            let mut o = *self as u32 - 1;
            if SURROGATE_RANGE.contains(o) {
                o -= SURROGATE_RANGE.len() as u32;
            }

            // We've manually skipped the surrogate range and done range
            // checking above, so this *should* actually be safe.
            unsafe { char::from_u32_unchecked(o) }
        }
    }
}


/// Atom type for use with mixed binary/text data.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub enum ByteOrChar {
    /// Single byte
    Byte(u8),
    /// Character
    Char(char)
}

impl From<u8> for ByteOrChar {
    fn from(byte: u8) -> Self {
        ByteOrChar::Byte(byte)
    }
}

impl From<char> for ByteOrChar {
    fn from(c: char) -> Self {
        ByteOrChar::Char(c)
    }
}

impl Distance for ByteOrChar {
    fn distance(&self, b: &Self) -> usize {
        Distance::distance(&Into::<char>::into(*self), &Into::<char>::into(*b))
    }
}

impl Step for ByteOrChar {
    fn increment(&self) -> Self {
        match self {
            &ByteOrChar::Byte(b) => ByteOrChar::Byte(b.increment()),
            &ByteOrChar::Char(c) => ByteOrChar::Char(c.increment()),
        }
    }
    fn decrement(&self) -> Self {
        match self {
            &ByteOrChar::Byte(b) => ByteOrChar::Byte(b.increment()),
            &ByteOrChar::Char(c) => ByteOrChar::Char(c.increment()),
        }
    }
}

impl Into<char> for ByteOrChar {
    fn into(self) -> char {
        match self {
            ByteOrChar::Byte(b) => b as char,
            ByteOrChar::Char(c) => c,
        }
    }
}

macro_rules! BoC_ascii_ext_forward {
    ($slf: ident, $name: ident) => (match $slf {
        &ByteOrChar::Byte(b) => b.$name(),
        &ByteOrChar::Char(c) => c.$name()
    });
    (mut $slf: ident, $name: ident) => (match *$slf {
        ByteOrChar::Byte(ref mut b) => b.$name(),
        ByteOrChar::Char(ref mut c) => c.$name()
    });
    ($slf: ident, $name: ident -> $conv: ident) => (match $slf {
        &ByteOrChar::Byte(b) => b.$name().$conv(),
        &ByteOrChar::Char(c) => c.$name().$conv()
    });
}

impl ascii::AsciiExt for ByteOrChar {
    type Owned = ByteOrChar;
    #[inline]
    fn is_ascii(&self) -> bool { BoC_ascii_ext_forward!(self, is_ascii) }
    #[inline]
    fn to_ascii_uppercase(&self) -> Self::Owned { BoC_ascii_ext_forward!(self, to_ascii_uppercase -> into) }
    #[inline]
    fn to_ascii_lowercase(&self) -> Self::Owned { BoC_ascii_ext_forward!(self, to_ascii_uppercase -> into) }
    #[inline]
    fn make_ascii_uppercase(&mut self) { BoC_ascii_ext_forward!(mut self, make_ascii_uppercase) }
    #[inline]
    fn make_ascii_lowercase(&mut self) { BoC_ascii_ext_forward!(mut self, make_ascii_lowercase) }

    #[inline]
    fn eq_ignore_ascii_case(&self, other: &Self) -> bool {
        match self {
            &ByteOrChar::Byte(b1) => match other {
                &ByteOrChar::Byte(b2) => b1.eq_ignore_ascii_case(&b2),
                _ => false },
            &ByteOrChar::Char(c1) => match other {
                &ByteOrChar::Char(c2) => c1.eq_ignore_ascii_case(&c2),
                _ => false }
        }
    }
}
