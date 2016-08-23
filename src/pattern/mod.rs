//! Structural representations of regular patterns.
//!
//! ## Atoms
//!
//! Patterns are parameterized by the _kind_ of thing &mdash; bounded by the
//! [`Atom`][Atom] trait &mdash; those patterns describe; regular expressions,
//! for example, are usually patterns of characters.  Like their real-world
//! counterparts, `Atom`s are indivisible: they represent the smallest unit
//! that will be analyzed by any algorithm.
//!
//! Atom and its prerequisites are implemented for all of Rust's primitive
//! types; the crate also provides a [`ByteOrChar`][ByteOrChar] atom type,
//! which is capable of representing either a `u8` or a `char` and is suitable
//! for use with mixed binary/UTF-8 patterns.
//!
//! [Atom]: trait.Atom.html
//! [ByteOrChar]: enum.ByteOrChar.html
//!
//! ### `Distance` and `Step`
//!
//! If an atom is to be used in range-like constructs like the `Range` variant
//! of [`ClassMember`][ClassMember], it must have implementations of the
//! [`Distance`][Distance] and [`Step`][Step] traits available.  These traits
//! allow iterating over ranges of values of types that implement them.
//!
//! [ClassMember]: enum.ClassMember.html
//! [Step]: trait.Step.html
//! [Distance]: trait.Distance.html
//!
//! ## Pattern types
//!
//! Alone, an atom matches only itself; other the variants of the
//! [`Element`][Element] are what provides the capability to specify arbitrary
//! regular patterns.  For more details about these variants, visit
//! `Element`'s documentation.
//!
//! [Element]: enum.Element.html
//!
//! ## Semantics of Set Implementations
//!
//! Types in this module implement several traits from the [`set`
//! module][set-module] like [`IsSubsetOf`][set-IsSubsetOf] and
//! [`Contains`][set-Contains].  When using these traits, we view a pattern
//! element as the *set of possible inputs* that would match that element; for
//! example, a wildcard is always a superset of an atom or atom class &mdash;
//! but never a superset of any pattern of non-unit length.
//!
//! Each `IsSubsetOf` implementation is documented with the reasoning behind
//! its implementation.
//!
//! [set-module]: ../util/set/index.html
//! [set-Contains]: ../util/set/trait.Contains.html
//! [set-IsSubsetOf]: ../util/set/trait.IsSubsetOf.html

use std;
use std::{char, u8, u16, u32, u64, usize};
use std::{ptr, cmp, mem};
use std::convert::{TryFrom, TryInto};
use std::ops::Range;
use std::iter::{FromIterator,IntoIterator};
use std::fmt::{self,Display,Debug};

#[cfg(feature="regex")]
use regex_syntax::Expr;

use util::set::{self, Contains};

mod atom;
mod class;
mod union;
mod sequence;
mod repetition;

pub mod codegen;

pub use self::atom::*;
pub use self::class::*;
pub use self::union::*;
pub use self::sequence::*;
pub use self::repetition::*;

/// Single-element pattern that matches atom.
///
/// This is re-exported to parallel the types contained by
/// [`Element`](enum.Element.html)'s data-bearing variants, which can be used
/// separately from `Element`.
pub use self::Element::Wildcard;


// ================================================================
/// Interface for arbitrary transformations of pattern types.
///
/// This provides a common entry point for reduction implementations.
pub trait Reduce {
    /// Result of calling `reduce` for an implementation.
    type Output;

    /// Reduce the receiver.
    fn reduce(self) -> Self::Output;
}

// ================================================================
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

/// Flag that indicates how a particular element in the input vector passed to
/// `flatten_and_reduce` is to be handled.
#[doc(hidden)]
#[derive(Copy, Clone, PartialEq)]
enum Mark {
    /// Keep the element without modifications.
    Keep,

    /// Remove the element (because it reduced itself into oblivion).
    Remove,

    /// Replace the element with the contents of its contained vector.
    Flatten(usize)
}

/// Reduce all elements of a vector, selectively flattening child vectors into
/// the parent.
///
/// Uses the passed functions or closures to determine which elements should be
/// flattened (`fetch_child_length`), and how to extract the child vectors from
/// those (`into_child_vec`).  It is guaranteed that `into_child_vec` will only
/// be called on those elements for which `fetch_child_length` returns
/// `Some(length)`.
// FIXME: should we try flattening more than one level at a time?
fn flatten_and_reduce<T, F, G>(v: &mut Vec<T>, placeholder: T,
                               fetch_child_length: F,
                               into_child_vec: G)
    where T: Clone + Reduce<Output=Option<T>>,
          F: Fn(&T) -> Option<usize>, G: Fn(T) -> Vec<T>

{
    use self::Mark::*;

    let mut marks = Vec::with_capacity(v.len());
    let mut total_len = 0;

    // Iterate over the elements in the sequence, calling `f` on each and
    // counting the total number of elements we should have after flattening.
    for elt in v.iter_mut() {
            let val = mem::replace(elt, placeholder.clone());
            match val.reduce() {
                None => { marks.push(Remove); },
                Some(val) => {
                    // Swap the reduced value back into place.
                    mem::replace(elt, val);
                    let (len, mark) = match fetch_child_length(elt) {
                        None => (1, Keep),
                        Some(len) => (len, Flatten(len)) };
                    total_len += len;
                    marks.push(mark);
                }
            }
    }

    unsafe {
        // We'll iterate through the marks and modify the vector in reverse
        // order, since any uninitialized elements added by the below `set_len`
        // call will be at the end.

        let mut read_ptr = v.as_mut_ptr().offset(v.len() as isize - 1);
        let mut write_ptr = v.as_mut_ptr().offset(total_len as isize - 1);

        v.set_len(total_len);

        let mut marks_iter = marks.into_iter().rev().peekable();

        while let Some(mark) = marks_iter.next() {
            match mark {
                // For `remove` we simply drop the element.
                Remove => ptr::drop_in_place(read_ptr),
                Keep => {
                    // Count the number of consecutive elements to be copied,
                    // consuming the entire run of `Keep` marks.
                    let mut n = 1;
                    while let Some(&Keep) = marks_iter.peek() {
                        n += 1;
                        marks_iter.next();
                    }
                    write_ptr = write_ptr.offset(-(n - 1));
                    read_ptr = read_ptr.offset(-(n - 1));

                    // move items up to the next mark back one element
                    ptr::copy(read_ptr, write_ptr, n as usize);
                },
                Flatten(len) => {
                    write_ptr = write_ptr.offset(-(len as isize - 1));

                    let child_vec = into_child_vec(ptr::read(read_ptr));
                    if child_vec.len() != len {
                        panic!("Length mismatch when flattening child element: expected {}, found {}",
                               len, child_vec.len());
                    }

                    // Expand the child vector.
                    ptr::copy(child_vec.as_ptr(), write_ptr, len);
                }
            }

            read_ptr = read_ptr.offset(-1);
            if mark != Remove {
                write_ptr = write_ptr.offset(-1);
            }
        }
    }
}

#[test]
fn test_flatten_and_reduce() {
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
                _ => panic!("cannot convert `{:?}` into inner vec", self)
            }
        }
    }

    impl Reduce for R {
        type Output = Option<R>;
        fn reduce(self) -> Self::Output {
            Some(self)
        }
    }

    let mut v = vec![R::L(1), R::L(2), R::V(vec![R::L(3), R::L(4), R::L(5)])];
    flatten_and_reduce(&mut v,  R::L(std::u32::MAX), R::vec_len, R::into_vec);
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
}

impl<T: Atom> Reduce for Element<T> {
    type Output = Option<Element<T>>;

    /// "Reduce" the element in some implementation-defined way.  This method
    /// is currently used for flattening of unnecessarily-nested pattern
    /// elements and elimination of redundant structure.
    fn reduce(self) -> Self::Output {
        match self {
            Element::Sequence(seq) => seq.reduce(),
            Element::Union(union) => union.reduce(),
            x => Some(x)
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
    T, T, atom, self;

    &Element::Atom(a) => a.is_subset_of(atom),
    &Element::Wildcard
        => false
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


element_is_subset_of_impl! {
    T, Sequence<T>, seq, self;
    &Element::Atom(_) => false,
    &Element::Wildcard => false
}

impl<T: Atom> set::IsSubsetOf<Element<T>> for Element<T> {
    fn is_subset_of(&self, other: &Element<T>) -> bool {
        match self {
            &Element::Wildcard => match other {
                &Element::Tagged{ref element, ..} => self.is_subset_of(&**element),
                &Element::Repeat(ref rep) => self.is_subset_of(rep.element()) && rep.count().contains(1),
                &Element::Union(ref union) => union.iter().any(|m| self.is_subset_of(m)),
                &Element::Class(ref class) => class.is_empty() && class.polarity() == Polarity::INVERTED,

                &Element::Wildcard => true,

                &Element::Atom(_) |
                &Element::Sequence(_) |
                &Element::Anchor(_)
                    => false, },
            &Element::Atom(a) => a.is_subset_of(other),
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
    use regex_syntax;
    error_chain! {
        types { RegexConvError, RegexConvErrorKind, RegexConvErrorChain, RegexConvResult; }
        foreign_links { regex_syntax::Error, ParseFailure; }
        errors {
            UnsupportedFeature(expr: regex_syntax::Expr, feature: String) {
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



// ----------------------------------------------------------------

/// Parse a `regex`-style regular expression and return the corresponding
/// Pattern object.
#[cfg(feature="regex")]
pub fn parse_regex<T: Atom>(s: &str) -> RegexConvResult<Element<T>>
    where Element<T>: TryFrom<Expr,Err=RegexConvError>
{
    match Expr::parse(s) {
        Err(e) => Err(RegexConvErrorKind::ParseFailure(e).into()),
        Ok(expr) => expr.try_into()
    }
}
