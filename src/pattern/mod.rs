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
use std::{char, u8, u32, usize};
use std::{iter, ptr, mem};
use std::fmt::{self,Display,Debug};
use std::ops::{Add, AddAssign, Range};


use util::set::{self, Contains};

mod atom;
mod union;
mod sequence;
mod repetition;
#[cfg(feature = "pattern_class")]
mod class;

pub mod build;
pub mod codegen;

pub use self::atom::*;
pub use self::union::*;
pub use self::sequence::*;
pub use self::repetition::*;
#[cfg(feature = "pattern_class")]
pub use self::class::*;

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

/// Interface for applying the "filter-map" operation to a collection.
///
/// This operation calls a closure on each element in the collection.
///
///   * If the closure returns `None` the element is simply dropped;
///   * otherwise the element is replaced with the value returned in `Some`.
///
///
pub trait FilterMap<T, U> {
    /// Type produced by the `filter_map` operation on the implementor.
    ///
    /// In general, this should be the type you'd get by substituting `T` for
    /// `U` in the implementing type &mdash; it may however also be wrapped in
    /// `Option` if an empty collection is logically invalid.
    type Output;

    /// Produce a new version of the receiver by filtering its elements and
    /// transforming the ones that are not discarded.
    fn filter_map<F>(self, f: F) -> Self::Output
        where F: Copy + Fn(T) -> Option<U>;
}

/// Interface for applying the "map" operation to a collection.
///
/// This operation calls a closure on each element in the collection, creating
/// a replacement element.
pub trait Map<T, U> {
    /// Type produced by the `map` operation on the implementor.
    ///
    /// In general, this should be the type you'd get by substituting `T` for
    /// `U` in the implementing type.
    type Output;

    /// Produce a new version of the receiver by transforming its elements.
    fn map<F>(self, f: F) -> Self::Output
        where F: Copy + Fn(T) -> U;
}

impl<T, U> Map<T, U> for Vec<T> {
    type Output = Vec<U>;
    fn map<F>(self, f: F) -> Self::Output
        where F: Copy + Fn(T) -> U
    {
        self.into_iter().map(f).collect()
    }
}

/// Interface for mapping between atom types in a pattern element.
pub trait MapAtoms<T, U> where T: Atom, U: Atom {
    /// Type produced by the implementation.
    type Output;

    /// Use the provided function to transform all atoms in `self` to the
    /// provided type
    fn map_atoms<F>(self, f: F) -> Self::Output
        where F: Fn(T) -> U;
}


/// Trait for retrieving the expected number of atoms required to match
/// a pattern.
pub trait AtomicLen {
    /// Fetch a bound on the number of atoms required to match the element.
    fn atomic_len(&self) -> SizeBound;
}

/// A bound on some expected integer-typed size value.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SizeBound {
    /// Exact size is known in advance
    Exact(usize),

    /// Size can be within the given range of values.
    Range(usize, usize)
}

impl SizeBound {
    /// Get the minimum number of bytes specified by this SizeBound.
    #[inline]
    fn min(&self) -> usize {
        match self {
            &SizeBound::Exact(s) => s,
            &SizeBound::Range(a, _) => a,
        }
    }
    /// Get the maximum number of bytes specified by this SizeBound.
    #[inline]
    fn max(&self) -> usize {
        match self {
            &SizeBound::Exact(s) => s,
            &SizeBound::Range(_, b) => b,
        }
    }

}

impl iter::Sum<SizeBound> for SizeBound {
    #[inline]
    fn sum<I>(iter: I) -> Self where I: Iterator<Item=SizeBound> {
        let mut sum = SizeBound::Exact(0);
        for rs in iter {
            sum += rs;
        }
        sum
    }
}

impl Add<SizeBound> for SizeBound {
    type Output = SizeBound;
    #[inline]
    fn add(self, rhs: SizeBound) -> Self::Output {
        let min = self.min() + rhs.min();
        let max = self.max() + rhs.max();
        if min == max { SizeBound::Exact(min) }
        else { SizeBound::Range(min, max) }
    }
}

impl AddAssign<SizeBound> for SizeBound {
    #[inline]
    fn add_assign(&mut self, rhs: SizeBound) {
        *self = *self + rhs;
    }
}


impl From<usize> for SizeBound {
    #[inline]
    fn from(s: usize) -> Self {
        SizeBound::Exact(s)
    }
}

impl From<Range<usize>> for SizeBound {
    #[inline]
    fn from(r: Range<usize>) -> Self {
        if r.len() > 1 { SizeBound::Range(r.start, r.end - 1) }
        else { SizeBound::Exact(r.start) }
    }
}

// ================================================================
// Anchor

/// Non-consuming pattern matchers.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum Anchor<T: Atom> {
    /// Beginning of the input buffer.
    StartOfInput,
    /// End of the input buffer.
    EndOfInput,

    /// Dummy variant to allow us to use a type parameter, which is required
    /// when implementing traits for which we want to use that type parameter
    /// in an associated type (see the impl of `set::Difference` for `Anchor`).
    #[doc(hidden)]
    __PhantomData(std::marker::PhantomData<T>)
    /*/// Match the *FA with entry at the given node, ahead of the
    /// current position.
    LookAhead(Box<Transition<T>>),
    /// Match the *FA with entry at the given node, behind the
    /// current position.
    LookBehind(Box<Transition<T>>)*/
}


impl<T: Atom> Anchor<T> {
    /// Dummy `map_atom` implementation.  Since `Anchor` doesn't actually
    /// contain any atoms -- i.e. its type parameter exists solely to enable
    /// the use of certain `set` traits¹ -- we simply transmute `self`.
    ///
    /// This may change in the future if/when additional anchor types
    /// are added.
    ///
    /// ¹: In the partial monomorphization of `set::Difference` used in this
    ///    module, the associated type `Output` is `Element<T>` -- hence the
    ///    need for a type parameter.
    #[doc(hidden)]
    pub fn map_atoms<U, F>(self, _: F) -> Anchor<U>
        where F: Fn(T) -> U,
              U: Atom
    {
        match self {
            Anchor::StartOfInput => Anchor::StartOfInput,
            Anchor::EndOfInput => Anchor::EndOfInput,
            _ => unreachable!()
        }
    }
}

impl<T: Atom> set::IsSubsetOf<Element<T>> for Anchor<T> {
    fn is_subset_of(&self, elt: &Element<T>) -> bool {
        match elt {
            &Element::Tagged{ref element, ..} => self.is_subset_of(&**element),
            &Element::Sequence(ref s) => self.is_subset_of(s),
            &Element::Union(ref u) => self.is_subset_of(u),
            &Element::Repeat(ref r) => self.is_subset_of(&**r),

            &Element::Anchor(ref a) => self.is_subset_of(a),
            &Element::Atom(ref a) => self.is_subset_of(a),
            #[cfg(feature = "pattern_class")]
            &Element::Class(ref c) => self.is_subset_of(c),
            &Element::Wildcard => false,
            &Element::Not(ref element) => ! self.is_subset_of(&**element),
        }
    }
}

impl<T: Atom> AtomicLen for Anchor<T> {
    fn atomic_len(&self) -> SizeBound {
        // For now we don't support lookahead/lookbehind, so this will be zero.
        0.into()
    }
}

impl<T: Atom> set::IsSubsetOf<T> for Anchor<T> {
    /// An anchor is never a subset of an atom.
    #[inline(always)]
    fn is_subset_of(&self, _: &T) -> bool {
        false
    }
}

impl<T: Atom> set::IsSubsetOf<Anchor<T>> for Anchor<T> {
    /// One anchor is a subset of another if both match the same conditions.
    #[inline]
    fn is_subset_of(&self, anchor: &Self) -> bool {
        self == anchor
    }
}

#[cfg(feature = "pattern_class")]
impl<T: Atom> set::IsSubsetOf<Class<T>> for Anchor<T> {
    /// An anchor is never a subset of a class of atoms, because currently
    /// anchors always describes conditions outside the scope of an atom.
    #[inline(always)]
    fn is_subset_of(&self, _: &Class<T>) -> bool {
        false
    }
}

impl<T: Atom> set::IsSubsetOf<Union<Element<T>>> for Anchor<T> {
    /// An anchor is a subset of a union if it is a subset of any member of
    /// the union.
    #[inline]
    fn is_subset_of(&self, union: &Union<Element<T>>) -> bool {
        union.iter().any(|elt| match elt { &Element::Anchor(ref t) if t == self => true, _ => false })
    }
}

impl<T: Atom> set::IsSubsetOf<Sequence<Element<T>>> for Anchor<T> {
    /// An anchor is a subset of a sequence if that sequence has length one and
    /// the anchor is a subset of the sequence's first element.
    #[inline]
    fn is_subset_of(&self, seq: &Sequence<Element<T>>) -> bool {
        seq.len() == 1 && self.is_subset_of(&seq[0])
    }
}

impl<T: Atom> set::IsSubsetOf<Repetition<Element<T>>> for Anchor<T> {
    /// An anchor is a subset of a repetition if it is a subset of the repeated
    /// element and the repeat-count includes 1.
    #[inline]
    fn is_subset_of(&self, rep: &Repetition<Element<T>>) -> bool {
        self.is_subset_of(rep.value()) && rep.count().contains(1)
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
    #[cfg(feature = "pattern_class")]
    Class(Class<T>),

    /// Condition that must match the current input for pattern-matching to
    /// continue
    Anchor(Anchor<T>),

    /// Sequence or concatenation of elements.
    Sequence(Sequence<Element<T>>),

    /// Alternation (union) of elements.
    Union(Union<Element<T>>),

    /// Repeated element.
    ///
    /// No provision is made for non-greedy repetition; instead we label
    /// defective those patterns for which it would be required.
    Repeat(Box<Repetition<Element<T>>>),


    /// Captured element
    Tagged {
        /// Actual element
        element: Box<Element<T>>,

        /// Name used for the capture
        name: String
    },

    /// Negated expression
    Not(Box<Element<T>>)
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
            #[cfg(feature = "pattern_class")]
            Element::Class(c) => Element::Class(c.map_atoms(f)),
            Element::Anchor(a) => Element::Anchor(a.map_atoms(f)),
            Element::Sequence(s) => Element::Sequence(s.map_atoms(f)),
            Element::Union(u) => Element::Union(u.map_atoms(f)),
            Element::Repeat(rep) => Element::Repeat(Box::new(rep.map_atoms(f))),
            Element::Tagged{element, name} => Element::Tagged{element: Box::new(element.map_atoms(f)), name: name},
            Element::Not(element) => Element::not(element.map_atoms(f)),
        }
    }

    /// Negate the given pattern element.
    pub fn not(elt: Self) -> Self {
        Element::Not(Box::new(elt))
    }
}

impl<T: Atom> AtomicLen for Element<T> {
    fn atomic_len(&self) -> SizeBound {
        match self {
            &Element::Wildcard |
            &Element::Atom(_)
                => SizeBound::Exact(1),
            #[cfg(feature = "pattern_class")]
            &Element::Class(ref c) => c.atomic_len(),
            &Element::Anchor(ref a) => a.atomic_len(),
            &Element::Sequence(ref s) => s.atomic_len(),
            &Element::Union(ref u) => u.atomic_len(),
            &Element::Repeat(ref rep) => rep.atomic_len(),
            &Element::Tagged{ref element, ..} |
            &Element::Not(ref element)
                => element.atomic_len(),
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
            #[cfg(feature = "pattern_class")]
            &Element::Class(ref c) => c.contains(atom),
            &Element::Sequence(ref s) => s.contains(atom),
            &Element::Union(ref u)  => u.contains(atom),
            &Element::Repeat(ref r) => (**r).contains(atom),
            &Element::Tagged{ref element, ..} => element.contains(atom),
            &Element::Anchor(_) => false,
            &Element::Not(ref element) => ! element.contains(atom),
        }
    }
}

macro_rules! element_is_subset_of_impl {
    ($T: ident, $Tp: ty, $name: ident, $s: ident; $($arms:tt)+) => {
        impl<$T: Atom> set::IsSubsetOf<$Tp> for Element<$T> {
            fn is_subset_of(&$s, $name: &$Tp) -> bool {
                match $s {
                    $($arms)+,
                    #[cfg(feature = "pattern_class")]
                    &Element::Class(ref c) => c.is_subset_of($name),
                    &Element::Sequence(ref s) => s.is_subset_of($name),
                    &Element::Union(ref u) => u.is_subset_of($name),
                    &Element::Tagged{ref element, ..} => (&**element).is_subset_of($name),
                    &Element::Repeat(ref r) => (&**r).is_subset_of($name),
                    &Element::Anchor(ref a) => a.is_subset_of($name),
                    &Element::Not(ref element) => ! (&**element).is_subset_of($name),
                }
            }
        }

    };
}
element_is_subset_of_impl! {
    T, T, atom, self;

    &Element::Atom(a) => a == *atom,//a.is_subset_of(atom),
    &Element::Wildcard
        => false
}

element_is_subset_of_impl! {
    T, Anchor<T>, anchor, self;

    &Element::Atom(_) |
    &Element::Wildcard
        => false
}

element_is_subset_of_impl! {
    T, Repetition<Element<T>>, rep, self;

    &Element::Atom(_) |
    &Element::Wildcard
        => self.is_subset_of(rep.value()) && rep.count().contains(1)
}

#[cfg(feature = "pattern_class")]
element_is_subset_of_impl! {
    T, Class<T>, class, self;
    &Element::Atom(a) => class.contains(a),
    &Element::Wildcard => false
}
element_is_subset_of_impl! {
    T, Union<Element<T>>, union, self;
    &Element::Atom(a) => union.contains(a),
    &Element::Wildcard => union.iter().any(|elt| Element::Wildcard.is_subset_of(elt))
}


element_is_subset_of_impl! {
    T, Sequence<Element<T>>, seq, self;
    &Element::Atom(_) => false,
    &Element::Wildcard => false
}

impl<T: Atom> set::IsSubsetOf<Element<T>> for Element<T>
{
    fn is_subset_of(&self, other: &Element<T>) -> bool {
        match self {
            &Element::Wildcard => match other {
                &Element::Tagged{ref element, ..} => self.is_subset_of(&**element),
                &Element::Repeat(ref rep) => self.is_subset_of(rep.value()) && rep.count().contains(1),
                &Element::Union(ref union) => union.iter().any(|m| self.is_subset_of(m)),
                #[cfg(feature = "pattern_class")]
                &Element::Class(ref class) => class.is_empty() && class.polarity() == Polarity::INVERTED,

                &Element::Wildcard => true,

                &Element::Atom(_) |
                &Element::Sequence(_) |
                &Element::Anchor(_)
                    => false,
                &Element::Not(ref element) => ! self.is_subset_of(&**element),
            },
            &Element::Atom(a) => <T as set::IsSubsetOf<Element<T>>>::is_subset_of(&a, other),
            #[cfg(feature = "pattern_class")]
            &Element::Class(ref c) => c.is_subset_of(other),
            &Element::Sequence(ref s) => s.is_subset_of(other),
            &Element::Union(ref u) => u.is_subset_of(other),
            &Element::Repeat(ref rep) => (*rep).is_subset_of(other),
            &Element::Tagged{ref element, ..} => (&**element).is_subset_of(other),
            &Element::Anchor(_) => false,
            &Element::Not(ref element) => ! (&**element).is_subset_of(other),
        }
    }
}


// ----------------------------------------------------------------
// Conversions from atomic types

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


// ----------------------------------------------------------------
// Conversions from variant value-types

macro_rules! element_from_variant_impl {
    ($T: ident, $Ty: ty, $variant: ident) => {
        impl<$T: Atom> From<$Ty> for Element<$T> {
            fn from(val: $Ty) -> Self {
                Element::$variant(val)
            }
        }
    };
}

element_from_variant_impl!(T, T, Atom);
#[cfg(feature = "pattern_class")]
element_from_variant_impl!(T, Class<T>, Class);
element_from_variant_impl!(T, Anchor<T>, Anchor);
element_from_variant_impl!(T, Sequence<Element<T>>, Sequence);
element_from_variant_impl!(T, Union<Element<T>>, Union);

impl<T: Atom> From<Repetition<Element<T>>> for Element<T> {
    fn from(rep: Repetition<Element<T>>) -> Self {
        Element::Repeat(rep.into())
    }
}


impl Display for Element<char> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Element::Repeat(ref repetition)
                => Display::fmt(repetition, f),
            _ => <Self as Debug>::fmt(self, f)
        }
    }
}
