//! Structural representations of regular patterns.
//!
//! Patterns are parameterized by the _kind_ of thing &mdash; bounded by the
//! [`Atom`][atom] trait &mdash; those patterns describe; regular expressions,
//! for example, are usually patterns of characters.  Like their real-world
//! counterparts, `Atom`s are indivisible: they represent the smallest unit
//! that will be analyzed by any algorithm.
//!
//! Alone, an atom matches only itself; the variants of the
//! [`Element`][element] are what provides the capability to specify arbitrary
//! regular patterns.  For more details about these variants, visit
//! `Element`'s documentation.
//!
//! ## Semantics of Set Implementations
//!
//! Types in this module implement several traits from the [`set`
//! module][set-module] like [IsSubsetOf][set-IsSubsetOf] and
//! [Contains][set-Contains].  When using these traits, we view a pattern
//! element as the *set of possible inputs* that would match that element; for
//! example a wildcard is always a superset of an atom or atom class:
//!
//! ```rust
//! # extern crate cripes;
//! use std::iter::FromIterator;
//! use cripes::pattern::*;
//! use cripes::util::set::IsSubsetOf;
//! # fn main() {
//! assert!(Element::Atom('x').is_subset_of(&Element::Wildcard));
//! assert!(Class::from_iter("xyz".chars()).is_subset_of(&Element::Wildcard));
//! # }
//! ```
//!
//! It is not, however, a superset of any pattern of non-unit length:
//!
//! ```rust
//! # extern crate cripes;
//! use std::iter::FromIterator;
//! use cripes::pattern::*;
//! use cripes::util::set::{IsSubsetOf, IsSupersetOf};
//! # fn main() {
//! let union = Union::from_iter(["abc", "def", "ghi"].iter()
//!                              .map(|s| Sequence::<char>::from_iter(s.chars())));
//! let wild = Element::Wildcard;
//!
//! assert!(! union.is_subset_of(&wild));
//! assert!(! wild.is_subset_of(&union));
//! # }
//! ```
//!
//! [atom]: trait.Atom.html
//! [element]: enum.Element.html
//! [set-module]: ../util/set/index.html

use std;
use std::{char, u8, u16, u32, u64, usize};
use std::{ptr, cmp};
use std::convert::{TryFrom, TryInto};
use std::ops::{Deref, DerefMut, Range};
use std::iter::{FromIterator,IntoIterator};
use std::fmt::{self,Display,Debug};

#[cfg(feature="regex")]
use regex_syntax::Expr;
use arrayvec::ArrayVec;

use util::set::{self, Contains};

mod class;
mod union;
mod sequence;
mod wildcard;
mod repetition;

pub mod codegen;

pub use self::class::*;
pub use self::union::*;
pub use self::sequence::*;
pub use self::repetition::*;
pub use self::wildcard::*;

/// Trait-bounds requirements for atomic values in a pattern.
pub trait Atom: Debug + Copy + Clone + Eq + Ord + Distance + Step {}
impl<T> Atom for T where T: Debug + Copy + Clone + Eq + Ord + Distance + Step {}


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

macro_rules! impl_for_primitives {
    ($first: tt, $($rest:tt),+) => (
        impl_for_primitives!($first);
        impl_for_primitives!($($rest),+);
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

impl_for_primitives!(u8, u16, u32, u64, usize);

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

impl std::ascii::AsciiExt for ByteOrChar {
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

/// An action to be performed immediately before or after any pattern element
/// is consumed.
#[derive(Clone,Debug,PartialEq)]
pub enum Action {
    /// Mark the start of a captured portion of input at the current cursor
    /// location
    BeginSubmatch(String),
    /// Mark the end of a captured portion of input at the current cursor
    /// location
    EndSubmatch(String)
}


// ================================================================
// Patterns

/// Top-level pattern type
pub type Pattern<T> = Element<T>;

// ----------------------------------------------------------------
// Anchor

/// Non-consuming pattern matchers.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum Anchor/*<T: Atom>*/ {
    /// Beginning of the input buffer.
    StartOfInput,
    /// End of the input buffer.
    EndOfInput,
    /*/// Match the *FA with entry at the given node, ahead of the
    /// current position.
    LookAhead(Box<Transition<T>>),
    /// Match the *FA with entry at the given node, behind the
    /// current position.
    LookBehind(Box<Transition<T>>)*/
}

// ----------------------------------------------------------------

/// Metadata-storage wrapper for pattern elements.
#[derive(Clone,Debug)]
pub struct Tagged<T>
    where T: Clone + Debug {
    value: T,

    /// Actions that should be executed before the contained value is processed
    pub preactions: Vec<Action>,

    /// Actions that should be executed after the contained value is processed
    pub postactions: Vec<Action>
}

impl<T: Clone + Debug> Tagged<T> {
    /// Create a new Tagged instance with the given value.
    pub fn new(value: T) -> Self {
        Tagged{value: value,
               preactions: Vec::new(),
               postactions: Vec::new()}
    }

    /// Deconstruct the Tagged instance into its components
    pub fn unwrap(self) -> (T, Vec<Action>, Vec<Action>) {
        (self.value, self.preactions, self.postactions)
    }
}

impl<T: Clone + Debug> Deref for Tagged<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T: Clone + Debug> DerefMut for Tagged<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T: Clone + Debug> From<T> for Tagged<T> {
    fn from(thing: T) -> Self {
        Tagged::new(thing)
    }
}

impl<'a,T: Clone + Debug> From<&'a T> for Tagged<T> {
    fn from(thing: &'a T) -> Self {
        Tagged::new(thing.clone())
    }
}


// ----------------------------------------------------------------
// Element
/// High-level description of a matchable element in a pattern's structure
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub enum Element<T: Atom> {
    /// Any single atom.
    Wildcard,

    /// atomic literal value
    Atom(T),

    /// Any one of a set of atoms
    Class(Class<T>),

    /// Condition that must match the current input for pattern-matching to
    /// continue
    Anchor(Anchor/*<T>*/),

    /// Sequence or concatenation of elements.
    Sequence(Sequence<T>),

    /// Alternation (union) of elements.
    Union(Union<T>),

    /// Repeated element.
    ///
    /// No provision is made for non-greedy repetition; instead we label
    /// defective those patterns for which it would be required.
    Repeat(Repetition<T>),


    /// Captured element
    Tagged {
        /// Actual element
        element: Box<Element<T>>,

        /// Name used for the capture
        name: String
    }

}

/// Flatten certain elements of a parent vector into the parent, using the
/// passed functions or closures to determine which elements should be
/// flattened, and how to extract the child vectors from those.
// FIXME: should we try flattening more than one level at a time?
fn flatten_vec<T, F, G>(v: &mut Vec<T>, f: F, g: G)
    where F: Fn(&T) -> Option<usize>, G: Fn(T) -> Vec<T> {

    // Iterate over the elements in the sequence, calling `f` on each and
    // counting the total number of elements we should have after flattening.

    // To avoid excess heap allocations, we'll use a fixed-size stack-allocated
    // array to store the locations of expandable child elements; we simply
    // loop while we have a start offset.
    let mut child_seqs = ArrayVec::<[_;128]>::new();
    let mut start_offset = Some(0);
    let cur_len = v.len();
    let mut total_len = 0;

    while let Some(offset) = start_offset.take() {
        for (index, elt) in v[offset..cur_len].iter_mut().enumerate() {
            if let Some(len) = f(elt) {
                total_len += len;
                // ArrayVec returns `Some(overflow_value)` when it's full.
                if child_seqs.push((index, len)).is_some() {
                    // Stored start offset needs to be increased by the number
                    // of additional elements the expanded child vectors
                    // will store.
                    start_offset = Some(index + total_len - cur_len);
                    break;
                }
            } else {
                total_len += 1;
            }
        }

        // `total_len > cur_len` means we have child sequences that
        // can be flattened.
        if total_len > cur_len {
            unsafe {
                v.set_len(total_len);
                let mut csiter = child_seqs.iter().peekable();
                let vp = v.as_mut_ptr();
                while let Some(&(idx, len)) = csiter.next() {
                    let mut cp = vp.clone().offset(idx as isize);
                    let child_vec = g(ptr::read(cp));

                    // Move the non-expandable elements following `child_vec` so
                    // they follow the expanded `child_vec`
                    let next_idx = csiter.peek().map(|il| il.0).unwrap_or(cur_len);
                    for i in (idx + 1)..next_idx {
                        ptr::write(vp.clone().offset((i + len) as isize),
                                   ptr::read(vp.clone().offset(i as isize)))
                    }

                    // Expand the child vector.
                    for elt in child_vec {
                        ptr::write(cp, elt);
                        cp = cp.offset(1);
                    }

                    assert_eq!(cp, vp.clone().offset((idx + len) as isize));
                }
            }
        }
    }
}

#[test]
fn test_flatten_vec() {
    #[derive(Debug, PartialEq, Clone)]
    enum R { L(u32), V(Vec<R>) }
    impl R {
        fn vec_len(&self) -> Option<usize> {
            match *self {
                R::V(ref v) => Some(v.len()),
                _ => None
            }
        }

        fn into_vec(self) -> Vec<R> {
            match self {
                R::V(v) => v,
                _ => Vec::from(&[self][..])
            }
        }
    }
    let mut v = vec![R::L(1), R::L(2), R::V(vec![R::L(3), R::L(4), R::L(5)])];
    flatten_vec(&mut v,  R::vec_len, R::into_vec);
    assert_eq!(&[R::L(1), R::L(2), R::L(3), R::L(4), R::L(5)], &v[..]);
}

impl<T: Atom> Element<T> {
    /// Transforms each contained atom using the supplied function or closure
    /// to produce a new Element.
    pub fn map_atoms<U, F>(self, f: F) -> Element<U>
        where F: Fn(T) -> U,
              U: Atom
    {
        match self {
            Element::Wildcard => Element::Wildcard,
            Element::Atom(a) => Element::Atom(f(a)),
            Element::Class(c) => Element::Class(c.map_atoms(f)),
            Element::Anchor(a) => Element::Anchor(a),
            Element::Sequence(s) => Element::Sequence(s.map_atoms(f)),
            Element::Union(u) => Element::Union(u.map_atoms(f)),
            Element::Repeat(rep) => Element::Repeat(rep.map_atoms(f)),
            Element::Tagged{element, name} => Element::Tagged{element: Box::new(element.map_atoms(f)), name: name},
        }
    }

    /// "Reduce" the element in some implementation-defined way.  This method
    /// is used for common-prefix extraction, elimination of redundant
    /// alternatives, and flattening of unnecessarily-nested pattern elements.
    pub fn reduce(&mut self) {
        match *self {
            Element::Sequence(ref mut seq) => seq.reduce(),
            Element::Union(ref mut union) => union.reduce(),
            _ => ()
        }
    }
}

impl<T: Atom> set::Contains<T> for Element<T> {
    fn contains(&self, atom: T) -> bool {
        match self {
            &Element::Wildcard => true,
            &Element::Atom(a) => atom == a,
            &Element::Class(ref c) => c.contains(atom),
            &Element::Sequence(ref s) => s.contains(atom),
            &Element::Union(ref u)  => u.contains(atom),
            &Element::Repeat(ref r) => r.contains(atom),
            &Element::Tagged{ref element, ..} => element.contains(atom),
            &Element::Anchor(_) => false,
        }
    }
}

macro_rules! element_is_subset_of_impl {
    ($T: ident, $Tp: ty, $name: ident, $s: ident; $($arms:tt)+) => {
        impl<$T: Atom> set::IsSubsetOf<$Tp> for Element<$T> {
            fn is_subset_of(&$s, $name: &$Tp) -> bool {
                match $s {
                    $($arms)+,
                    &Element::Class(ref c) => c.is_subset_of($name),
                    &Element::Sequence(ref s) => s.is_subset_of($name),
                    &Element::Union(ref u) => u.is_subset_of($name),
                    &Element::Tagged{ref element, ..} => element.is_subset_of($name),
                    &Element::Repeat(ref r) => r.is_subset_of($name),
                    &Element::Anchor(_) => false,
                }
            }
        }

    };
}
element_is_subset_of_impl! { 
    T, Repetition<T>, rep, self;

    &Element::Atom(_) |
    &Element::Wildcard
        => self.is_subset_of(rep.element()) && rep.count().contains(1)
}
element_is_subset_of_impl! {
    T, Class<T>, class, self;
    &Element::Atom(a) => class.contains(a),
    &Element::Wildcard => false
}
element_is_subset_of_impl! {
    T, Union<T>, union, self;
    &Element::Atom(a) => union.contains(a),
    &Element::Wildcard => union.iter().any(|elt| Element::Wildcard.is_subset_of(elt))
}
                            


impl<T: Atom> set::IsSubsetOf<Element<T>> for Element<T> {
    fn is_subset_of(&self, other: &Element<T>) -> bool {
        match self {
            &Element::Wildcard => Wildcard.is_subset_of(other),
            &Element::Atom(a) => match other {
                &Element::Wildcard => true,
                &Element::Atom(b) => a == b,
                &Element::Class(ref c) => c.contains(a),
                &Element::Repeat(ref r) => self.is_subset_of(r),
                _ => false },
            &Element::Class(ref c) => c.is_subset_of(other),
            &Element::Sequence(ref s) => s.is_subset_of(other),
            &Element::Union(ref u) => u.is_subset_of(other),
            &Element::Repeat(ref rep) => (*rep).is_subset_of(other),
            &Element::Tagged{ref element, ..} => element.is_subset_of(other),
            &Element::Anchor(_) => false,
        }
    }
}


impl<T: Atom> From<T> for Element<T> {
    fn from(atom: T) -> Self {
        Element::Atom(atom)
    }
}

#[cfg(feature = "regex")]
#[allow(missing_docs)]
mod regex_conv {
    use regex_syntax::Expr;
    error_chain! {
        types { RegexConvError, RegexConvErrorKind, RegexConvErrorChain, RegexConvResult; }
        errors {
            UnsupportedFeature(expr: Expr, feature: String) {
                description("unsupported feature")
                    display("{} is not supported", feature)
            }
        }
    }
}
use self::regex_conv::*;

/// Macro for composing the match in a `From<regex_syntax::Expr> for
/// Element<...>` impl.
///
/// Useful because we have several types (u8, char, ByteOrChar) that can act as
/// atoms, and parts of their implementations of this trait can be shared.
macro_rules! element_from_expr_impl {
    ($T:ty => ($($rest:ident)+)) => { element_from_expr_impl!($T => ($($rest)+) ;); };


    ($T:ty => ( ); $($built:tt)*) => (
        #[cfg(feature="regex")]
        impl TryFrom<Expr> for Element<$T> {
            type Err = RegexConvError;
            fn try_from(expr: Expr) -> RegexConvResult<Self> {
                match expr {
                    $($built)+
                }
            }
        }
    );

    ($T:ty => (_char $($rest:ident)*); $($built:tt)*) => (
        element_from_expr_impl!($T => ( $($rest)* );
                                    $($built)*
                                    Expr::Literal{chars, casei} => {
                                        if casei {
                                            Err(RegexConvErrorKind::UnsupportedFeature(Expr::Literal{chars: chars, casei: casei},
                                                                                       "case-insensitive matching".into()).into())
                                        } else if chars.len() > 1 {
                                            Ok(Sequence::from_iter(chars.into_iter().map(|c| c.into())).into())
                                        } else {
                                            Ok(Element::Atom(chars[0].into()))
                                        } },
                                    Expr::AnyChar => Ok(Element::Wildcard),
                                    Expr::AnyCharNoNL =>  Ok(Element::Class(Class::new(Polarity::INVERTED, [ClassMember::Atom('\n'.into())].iter().cloned()))),
                                    Expr::Class(c) => {
                                        let first = c.iter().cloned().nth(0).unwrap();
                                        if c.len() > 1 || first.start != first.end {
                                            Ok(Element::Class(c.into()))
                                        } else {
                                            Ok(Element::Atom(first.start.into()))
                                        } });
    );
    ($T:ty => (_byte $($rest:ident)*); $($built:tt)*) => (
        element_from_expr_impl!($T => ( $($rest)* );
                                    $($built)*
                                    Expr::LiteralBytes{bytes, casei} => {
                                        if casei {
                                            Err(RegexConvErrorKind::UnsupportedFeature(Expr::LiteralBytes{bytes: bytes, casei: casei},
                                                                                       "case-insensitive matching".into()).into())
                                        } else if bytes.len() > 1 {
                                            Ok(Element::Sequence(Sequence::from_iter(bytes.into_iter().map(|b| b.into()))))
                                        } else {
                                            Ok(Element::Atom(bytes[0].into()))
                                        } },
                                    Expr::AnyByte => Ok(Element::Wildcard),
                                    Expr::AnyByteNoNL => Ok(Element::Class(Class::new(Polarity::INVERTED, [ClassMember::Atom(b'\n'.into())].iter().cloned()))),
                                    Expr::ClassBytes(c) => {
                                        let first = c.iter().cloned().nth(0).unwrap();
                                        if c.len() > 1 || first.start != first.end { Ok(Element::Class(c.into())) }
                                        else { Ok(Element::Atom(first.start.into())) } });
    );
    ($T:ty => (_common $($rest:ident)*); $($built:tt)*) => (
        element_from_expr_impl!($T => ( $($rest)* ) ;
                                $($built)*
                                Expr::Group{e, name, ..} => {
                                    if let Some(name) = name {
                                        match (*e).try_into() {
                                            Ok(elt) => Ok(Element::Tagged{element: Box::new(elt), name: name}),
                                            Err(e) => Err(e)
                                        }
                                    } else {
                                        (*e).try_into()
                                    } },
                                Expr::Repeat{e, r, greedy} => {
                                    if ! greedy {
                                        Err(RegexConvErrorKind::UnsupportedFeature(Expr::Repeat{e: e, r: r, greedy: greedy},
                                                                                   "non-greedy repetition".into()).into())
                                    } else {
                                        match (*e).try_into() {
                                            Ok(elt) => Ok(Element::Repeat(Repetition::new(elt, r.into()))),
                                            Err(e) => Err(e)
                                        }
                                    } },
                                Expr::Concat(exprs) => {
                                    match FromIterator::from_iter(exprs.into_iter().map(|e| e.try_into())) {
                                        Ok(v) => Ok(Element::Sequence(Sequence::new(v))),
                                        Err(e) => Err(e)
                                    } },
                                Expr::Alternate(exprs) => {
                                    match FromIterator::from_iter(exprs.into_iter().map(|e| e.try_into())) {
                                        Ok(v) => Ok(Element::Union(Union::new(v))),
                                        Err(e) => Err(e)
                                    } },
                                Expr::Empty => Err(RegexConvErrorKind::UnsupportedFeature(Expr::Empty, "empty expressions".into()).into()),
                                Expr::StartText => Ok(Element::Anchor(Anchor::StartOfInput)),
                                Expr::EndText => Ok(Element::Anchor(Anchor::EndOfInput)),
                                /*Expr::StartLine => unimplemented!(),
                                Expr::EndLine => unimplemented!(),
                                Expr::WordBoundary => unimplemented!(),
                                Expr::NotWordBoundary => unimplemented!(),
                                Expr::WordBoundaryAscii => unimplemented!(),
                                Expr::NotWordBoundaryAscii => unimplemented!(),*/
                                _ => unimplemented!());
        );
}
element_from_expr_impl!(char => (_char _common));
element_from_expr_impl!(u8 => (_byte _common));
element_from_expr_impl!(ByteOrChar => (_byte _char _common));

macro_rules! element_from_atom_impl {
    ($A: ty => $T: ty) => {
        impl From<$A> for Element<$T> {
            fn from(a: $A) -> Self {
                Element::Atom(a.into())
            }
        }
    };
}

element_from_atom_impl!(u8 => ByteOrChar);
element_from_atom_impl!(char => ByteOrChar);


macro_rules! element_from_variant_impl {
    ($T: ident, $Ty: ty, $variant: ident) => {
        impl<$T: Atom> From<$Ty> for Element<$T> {
            fn from(val: $Ty) -> Self {
                Element::$variant(val)
            }
        }
    };
}

element_from_variant_impl!(T, Sequence<T>, Sequence);
element_from_variant_impl!(T, Union<T>, Union);
element_from_variant_impl!(T, Class<T>, Class);
element_from_variant_impl!(T, Repetition<T>, Repeat);

impl Display for Element<char> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Element::Repeat(ref repetition)
                => Display::fmt(repetition, f),
            _ => <Self as Debug>::fmt(self, f)
        }
    }
}
