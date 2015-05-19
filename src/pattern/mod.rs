//! Support for patterns of arbitrary element types.
use std;
use std::ops::Deref;
//use std::convert::AsRef;
use std::slice::*;
use std::fmt::Debug;
use std::clone::Clone;
use std::marker::PhantomData;
//use std::collections::HashSet;
use std::hash::{Hash,Hasher};
use std::cmp::{Eq,PartialEq};
use std::iter::{Iterator,FromIterator,IntoIterator};

use util::hash::{hash,Hashable};

use util::flex_box::*;
use util::iter;

pub mod walk;
use self::walk::{action,Action,Walkable,WalkType};

/// Hash algorithm we'll use to provide an object-safe `Eq` implementation on
/// the top-level Pattern interface.
type HashAlgo = std::hash::SipHasher;

/// Interface for descriptions of syntactic patterns.
pub trait Pattern<T>:
    //Clone +
    //Walkable<FirstSet<T>> +

    // FIXME: Walkable<_> needs to be made object-safe
    Walkable<AtomicFirstSet<T>,Item=Element<T>> +

    Nullable +
    Hashable +
    std::fmt::Debug
    where T: Copy + Hash + std::fmt::Debug + Sized
{
    fn clone_to_box(&self) -> Box<Pattern<T>>;

    /// Fetch an iterator over the pattern's first set.
    ///
    /// @param buf Buffer into which the iterator should be placed.
    ///
    /// @return Result containing either a reference to the iterator stored in
    /// `buf`, or the required buffer size if the buffer was too small.
    fn first_set<'a,'b>(&'a self, buf: &'b mut FlexBox) -> Ref<'b,Iterator<Item=T>> where 'a: 'b;

    /// Determine whether the pattern is "fixed", i.e. matches a known single
    /// input sequence.
    fn is_fixed(&self) -> bool;

    #[unstable]
    /// Obtain lower and upper bounds on the length of an input sequence that
    /// would match the pattern.  A value of `None` in the second element of
    /// the returned tuple indicates that there *may* be no upper bound.
    ///
    /// Unstable until a decision is reached on whether this method must
    /// recurse subpatterns.
    fn input_length_bounds(&self) -> (usize, Option<usize>);
}

// FIXME: Walkable needs to be made object-safe.
/*impl<T> Hash for Pattern<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        <Pattern<T> as Hashable>::hash(self, state);
    }
}*/


/// Walk-type tag for walking patterns' first sets. A pattern's first set is
/// defined here as the set of elements that may begin a sequence matching
/// that pattern.
pub struct FirstSet<'a,T: 'a> { _marker: PhantomData<&'a T> }

impl<'a,T> WalkType for FirstSet<'a,T> {
    type Yield = &'a Element<T>;
}


/// Walk-type tag for walking pattern's *atomic* first sets.  A pattern's
/// atomic first set is the set of *atoms* that may begin a sequence matching
/// that pattern.
pub struct AtomicFirstSet<T> { _marker: PhantomData<T> }
impl<T> WalkType for AtomicFirstSet<T>  {
    type Yield = T;
}


/// Provides a method to determine whether a pattern is "nullable", i.e., can
/// match an empty sequence.
pub trait Nullable {
    /// Check if the token is nullable.
    fn is_nullable(&self) -> bool { false }
}

/* **************************************************************** */
/// Empty pattern type.

#[derive(Debug)]
pub struct Empty<T> { _marker: PhantomData<T> }

impl<T> Empty<T> {
    pub fn new() -> Self {
        Empty{ _marker: PhantomData }
    }
}

impl<T> Pattern<T> for Empty<T> where T: Copy + Hash + std::fmt::Debug + Sized {
     fn clone_to_box(&self) -> Box<Pattern<T>> {
         Box::new(Empty::new())
     }

    fn first_set<'a,'b>(&self, buf: &'b mut FlexBox) -> Ref<'b,Iterator<Item=T>>
        where 'a: 'b, T: 'a + 'b {
        buf.store(iter::Empty::new())
    }

    fn is_fixed(&self) -> bool { true }
    fn input_length_bounds(&self) -> (usize, Option<usize>) { (0, Some(0)) }
}

impl<'a,T> IntoIterator for &'a Empty<T> {
    type Item = &'a Element<T>;
    type IntoIter = iter::Empty<&'a Element<T>>;

    fn into_iter(self) -> <Self as IntoIterator>::IntoIter {
        iter::Empty::new()
    }
}

impl<T> Walkable<AtomicFirstSet<T>> for Empty<T> {
    fn action<'a>(&'a self, element: &'a Element<T>) -> walk::Action<AtomicFirstSet<T>> {
        Action::new(None, false, action::TERMINATE as u8)
    }
}

impl<T> Hashable for Empty<T> {
    fn hash(&self, _state: &mut Hasher) {
        // No-op.
    }
}

impl<T> Nullable for Empty<T> {
    fn is_nullable(&self) -> bool { true }
}

/* **************************************************************** */
/// Component of a pattern.  Each element is either an atomic value, or
/// a Pattern trait object.
#[derive(Debug)]
pub enum Element<T> {
    Empty,
    Atom(T),
    Pattern(Box<Pattern<T>>)
}

impl<T> Element<T> {
    // pub fn from_pattern<P: Pattern<T>>(x: P) -> Self {
    //     Element::Pattern(Box::new(x))
    // }
}


impl<T> Clone for Element<T> where T: Copy + Clone + Debug + Hash {
    fn clone(&self) -> Element<T> {
        match *self {
            Element::Empty => Element::Empty,
            Element::Atom(ref x) => Element::Atom(x.clone()),
            Element::Pattern(ref x) => Element::Pattern(x.clone_to_box())
        }
    }
}

impl<T> Hashable for Element<T> where T: Copy + Hash {
    fn hash(&self, state: &mut Hasher) {
        match *self {
            Element::Empty => (),
            Element::Atom(ref atom) => state.write(unsafe { from_raw_parts::<u8>(std::mem::transmute(atom),
                                                                                 std::mem::size_of::<T>()) }),
            Element::Pattern(ref pat) => <Pattern<T> as Hashable>::hash((*pat).deref(), state)
        }
    }
}

impl<T> Hash for Element<T> where T: Copy + Hash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Element::Empty => (),
            Element::Atom(ref atom) => atom.hash(state),
            Element::Pattern(ref pat) => pat.hash(state)
        }
    }
    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H) {
        for ref elt in data.iter() {
            elt.hash(state)
        }
    }
}

impl<T> Eq                       for Element<T> where T: Copy + Hash + Eq + PartialEq<T> {}
impl<T> PartialEq<Element<T>> for Element<T> where T: Copy + Hash + Eq + PartialEq<T> {
    fn eq(&self, other: &Self) -> bool {
        match *self {
            Element::Empty => match *other {
                Element::Empty => true,
                _ => false },
            Element::Atom(ref x) => match *other {
                Element::Atom(ref y) => x == y,
                _ => false },
            Element::Pattern(ref x) => match *other {
                Element::Pattern(ref y) => hash::<_,HashAlgo>(x.deref()) == hash::<_,HashAlgo>(y.deref()),
                _ => false }
        }
    }
}


//impl<T> WalkableExt for Element<T> where T: Copy + Hash {}

impl<T> Pattern<T> for Element<T>
where T: Eq + PartialEq + Hash + Copy + std::fmt::Debug {
    fn clone_to_box(&self) -> Box<Pattern<T>> where T: 'static {
        match *self {
            Element::Empty => Box::new(Element::Empty),
            Element::Atom(ref x) => Box::new(Element::Atom(x.clone())),
            Element::Pattern(ref x) => Box::new(Element::Pattern(x.clone_to_box()))
        }
    }

    fn first_set<'a,'b>(&'a self, buf: &'b mut FlexBox) -> Ref<'b,Iterator<Item=T>> where 'a: 'b {
        match *self {
            Element::Empty => buf.store(iter::Empty::new()),
            Element::Atom(ref atom) => buf.store(iter::Once::new(*atom)),
            Element::Pattern(ref pat) => pat.first_set(buf)
        }
    }

      // fn first_set(&self, buf: &'b mut [u8]) -> IterResult<T> {
      //     match *self {
      //         Element::Atom(ref atom) => {
      //             if  buf.len() < std::mem::size_of::<Once<T>>() {
      //                 Err(std::mem::size_of::<Once<T>>())
      //             } else {
      //                 let it = Once::new(atom);
      //                 unsafe { memcpy(&mut buf[..],
      //                                 std::mem::transmute::<_,&mut [u8]>(&mut it));
      //                          Ok(std::mem::transmute::<_,&Iterator<Item=T>>(buf)) }
      //             } },
      //         _ => Err(0)
      //     }
      // }

    fn is_fixed(&self) -> bool {
        match *self {
            Element::Empty => true,
            Element::Atom(..) => true,
            Element::Pattern(ref pat) => pat.is_fixed()
        }
    }

    fn input_length_bounds(&self) -> (usize, Option<usize>) {
        match *self {
            Element::Empty => (0, Some(0)),
            Element::Atom(..) => (1, Some(1)),
            Element::Pattern(ref pat) => pat.input_length_bounds()
        }
    }
}

//impl<T> Iterable<IterBox<&'a <FirstSet<T> as WalkType>::Item>> for Element<T>
/*impl<T> Iterable<Box<Iterator<Item=&'a <FirstSet<T> as WalkType>::Item>>> for Element<T>
where T: 'a {
    #[inline(always)]
    fn iter(&'a self) -> Box<Iterator<Item=&'a Element<T>> + 'a> {//Box<Iterator<Item=<FirstSet<T> as WalkType>::Item> + 'a> /*IterBox<&'a <FirstSet<T> as WalkType>::Item>*/ {
        Box::new(Once::new(self))
    }
}*/


/*impl<T: Copy + Hash + Eq + PartialEq> Walkable<'a,FirstSet<T>> for Element<T>
where T: 'a {
    #[inline(always)]
    fn iter(&'a self) -> Option<WalkIterator<FirstSet<T>>> {//Box<Iterator<Item=<FirstSet<T> as WalkType>::Item> + 'a> /*IterBox<&'a <FirstSet<T> as WalkType>::Item>*/ {
        Some(Box::new(iter::Once::new(self)))
    }

    fn action(&'a self, element: &'a <FirstSet<T> as WalkType>::Item) -> walk::Action<FirstSet<T>> {
        match *element {
            Element::Atom(..) => Action::new(Some(element), false, action::TERMINATE as u8),
            Element::Pattern(..) => Action::new(Some(element), true, action::TERMINATE as u8)
        }
    }
}*/

impl<'a,T> IntoIterator for &'a Element<T> {
    type Item = &'a Element<T>;
    type IntoIter = iter::Once<&'a Element<T>>;
    fn into_iter(self) -> <Self as IntoIterator>::IntoIter {
        iter::Once::new(self)
    }
}

/*impl<T> Iterable<Element<T>> for Element<T> {
    type Iterator = std::slice::Iter<Element<T>>;
    fn iter<>(&self) -> std::slice::Iter<Element<T>> {
        iter::Once::new(self)
    }
}*/


impl<T> Walkable<AtomicFirstSet<T>> for Element<T> where T: Eq + PartialEq + Copy + Hash {
    fn action<'a>(&'a self, element: &'a Element<T>) -> walk::Action<AtomicFirstSet<T>> {
        match *element {
            Element::Atom(atom) => Action::new(Some(atom), false, action::TERMINATE as u8),
            Element::Pattern(..) => Action::new(None, true, action::TERMINATE as u8)
        }
    }
}

impl<T> Nullable for Element<T> where T: Copy + Hash{
    fn is_nullable(&self) -> bool {
        match *self {
            Element::Empty => true,
            Element::Atom(..) => false,
            Element::Pattern(ref pat) => pat.is_nullable(),
        }
    }
}



/* **************************************************************** */
/// A sequence of atoms or subpatterns.
#[derive(Debug,Clone)]
pub struct Sequence<T> where T: Copy + Clone + Debug + Hash {
    elements: Vec<Element<T>>
}

impl<T: Copy + Hash> Hashable for Sequence<T> where T: Copy + Clone + Debug + Hash {
    fn hash(&self, state: &mut Hasher) {
        for ref elt in &(self.elements) {
            <Element<T> as Hashable>::hash(elt, state);
        }
    }
}

impl<T: Copy + Hash> Hash for Sequence<T> where T: Copy + Clone + Debug + Hash  {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let x: &[Element<T>] = self.elements.as_ref();
        x.hash(state)
    }
}

impl<T> Sequence<T> where T: Copy + Clone + Debug + Hash  {
    pub fn empty() -> Self {
        Sequence{elements: Vec::new()}
    }
    pub fn new(elements: Vec<Element<T>>) -> Sequence<T> {
        Sequence{elements: elements}
    }
}

/*struct SequenceFirstSet<T> where T: 'a {
    elements: &'a Vec<Element<T>>,
    sub_iter: Option<Box<Iterator<Item=T>>>
}

impl<T> Iterator for SequenceFirstSet<T> {
    type Item = T;
    #[inline(always)]
    fn next(&mut self) -> Option<T> {
        loop {
        match self.sub_iter {
            Some(boxed) => {
                match boxed.next() {
                    Some(T) => T,
                    None => {
                        self.sub_iter = None;
                    }
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
    }
}*/



impl<T> Pattern<T> for Sequence<T>
where T: Eq + PartialEq + Hash + Hashable + Copy + Debug {
    fn clone_to_box(&self) -> Box<Pattern<T>> {
        Box::new(Sequence::from_iter(self.elements.collect()));
    }


    fn first_set<'a,'b>(&'a self, buf: &'b mut FlexBox) -> Ref<'b,Iterator<Item=T>> where 'a: 'b {
        buf.store(self.walk::<AtomicFirstSet<T>>())
    }

    fn is_fixed(&self) -> bool {
        self.elements.iter().all(|ref elt| elt.is_fixed() )
    }

    fn input_length_bounds(&self) -> (usize, Option<usize>) {
        let mut totals = (0usize, Some(0usize));
        for ref elt in &(self.elements) {
            let b = elt.input_length_bounds();
            totals.0 += b.0;
            if b.1.is_some() && totals.1.is_some() {
                totals.1 = Some(b.1.unwrap() + totals.1.unwrap())
            } else {
                totals.1 = None
            }
        }
        totals
    }
}


impl<'a,T> IntoIterator for &'a Sequence<T> {
    type Item = &'a Element<T>;
    type IntoIter = std::slice::Iter<'a,Element<T>>;

    fn into_iter(self) -> <Self as IntoIterator>::IntoIter {
        self.elements.iter()
    }
}

impl<T> Walkable<AtomicFirstSet<T>> for Sequence<T> where T: Eq + PartialEq + Copy + Hash {
    fn action<'a>(&'a self, element: &'a Element<T>) -> walk::Action<AtomicFirstSet<T>> {
        match *element {
            Element::Atom(atom) =>
                Action::new(Some(atom), false, action::TERMINATE as u8),
            Element::Pattern(ref pat) =>
                Action::new(None, true, if pat.is_nullable() { 0 } else { action::TERMINATE as u8 })
        }
    }
}


/*impl<'a,T> Walkable<'a,FirstSet<'a,T>> for Sequence<T> where T: Copy + Hash + Eq + PartialEq {
    #[inline(always)]
    fn action(&'a self, element: &'a <FirstSet<'a,T> as WalkType>::Item) -> walk::Action<FirstSet<'a,T>> {
        match *element {
            Element::Atom(..) =>
                Action::new(Some(element), false, action::TERMINATE as u8),
            Element::Pattern(ref pat) =>
                Action::new(Some(element), true, if pat.is_nullable() { 0 } else { action::TERMINATE as u8 })
        }
    }
}*/

impl<T> FromIterator<T> for Sequence<T> where T: Copy + Debug + Eq + PartialEq + Hash {
    fn from_iter<I: IntoIterator<Item=T>>(v: I) -> Sequence<T> {
        Sequence::new(v.map_in_place(|c| Element::Atom(c)))
    }
}

impl<T> FromIterator<Element<T>> for Sequence<T> where T: Copy + Debug + Eq + PartialEq + Hash {
    fn from_iter<I: IntoIterator<Item=Element<T>>>(v: I) -> Sequence<T> {
        Sequence::new(v.into_iter().collect())
    }
}


impl<T> Nullable for Sequence<T> where T: Copy + Debug + Eq + PartialEq + Hash {
    fn is_nullable(&self) -> bool {
        if self.elements.is_empty() { true }
        else {
            for elt in self.elements.iter() {
                match *elt {
                    Element::Atom(..) => return false,
                    Element::Pattern(ref pat) => if ! pat.is_nullable() { return false; }
                }
            }
            true
        }
    }
}


/*/* **************************************************************** */
/// A set of alternatives.
#[derive(Debug,Clone)]
pub struct Union<T: Copy + Eq + Hash> {
    elements: HashSet<Element<T>>,
}


impl<T: Copy + Eq + Hash> Union<T> {
    pub fn new(elements: HashSet<Element<T>>) -> Union<T> {
        Union{elements: elements}
    }
}

impl<T> Pattern<T> for Union<T>
where T: Eq + PartialEq + Hash + Copy + std::fmt::Debug {
    fn is_fixed(&self) -> bool {
        if self.elements.is_empty() { true }
        else {
            match self.elements.len() {
                1 => self.elements.iter().next().unwrap().is_fixed(),
                _ => false
            }
        }
    }

    fn input_length_bounds(&self) -> (usize, Option<usize>) {
        let mut result = (0usize, Some(0usize));
        for elt in self.elements.iter() {
            let b = elt.input_length_bounds();
            if b.0 < result.0 { result.0 = b.0 }

            if b.1.is_some() && result.1.is_some() {
                let Some(u) = b.1;
                let Some(v) = result.1;
                if u > v { result.1 = Some(u) }
           } else {
               result.1 = None
           }

        }
        result
    }
}

impl<T: Copy + Eq + Hash> Hashable for Union<T> {
    fn hash(&self, state: &mut Hasher) {
        for ref elt in &(self.elements) {
            <Element<T> as Hashable>::hash(elt, state);
        }
    }
}

impl<T> Walkable<'a,AtomicFirstSet<T>> for Union<T> where T: Eq + PartialEq + Copy + Hash {
    #[inline(always)]
    fn iter<'a>(&'a self) -> Option<WalkIterator<AtomicFirstSet<'a,T>>> {
        Some(Box::new(self.elements.iter()))
    }

    fn action<'a>(&'a self, element: &'a <AtomicFirstSet<'a,T> as WalkType>::Item) -> walk::Action<AtomicFirstSet<'a,T>> {
        match *element {
            Element::Atom(atom) =>
                Action::new(Some(atom), false, 0),
            Element::Pattern(..) =>
                Action::new(None, true, 0)
        }
    }
}


impl<T> Walkable<'a,FirstSet<T>> for Union<T> where T: Copy + Hash + Eq + PartialEq {
    #[inline(always)]
    fn iter(&'a self) -> Option<WalkIterator<FirstSet<T>>> {
        Some(Box::new(self.elements.iter()))
    }

    #[inline(always)]
    fn action(&'a self, element: &'a <FirstSet<T> as WalkType>::Item) -> walk::Action<FirstSet<T>> {
        match *element {
            Element::Atom(..) =>
                Action::new(Some(element), false, 0),
            Element::Pattern(..) =>
                Action::new(Some(element), true, 0)
        }
    }
}


impl<T> FromIterator<T> for Union<T>
    where T: Copy + Eq + PartialEq + Hash {
    fn from_iter<I: IntoIterator<Item=T>>(v: I) -> Union<T> {
        Union::new(v.into_iter().map(|val| Element::Atom(val)).collect())
    }
}


impl<T> Nullable for Union<T> where T: Copy + Hash + Eq {
    fn is_nullable(&self) -> bool {
        if self.elements.is_empty() { true }
        else { self.elements.iter().any(|elt: &'a Element<T>| (*elt).is_nullable()) }
   }
}

/* **************************************************************** */

/* **************************************************************** */

/// Anchor patterns, including absolute-position, look-ahead,
/// look-behind types.
pub mod anchor {
    use std;
    use std::marker::*;
    use std::fmt::Debug;
    use std::hash::{Hash,Hasher};

    use super::*;
    use super::walk::*;
    use super::walk::Iterator as WalkIterator;
    use super::super::util::hash::Hashable;

    /// Absolute-position anchor pattern.
    #[derive(Debug,Copy,Clone)]
    #[repr(u8)]
    #[allow(non_camel_case_types)]
    pub enum Position {
        BUFFER_START,
        BUFFER_END
    }

    impl<T> Pattern<T> for Position
        where T: Copy + Eq + PartialEq + Hash + Debug {
        fn is_fixed(&self) -> bool { true }
        fn input_length_bounds(&self) -> (usize, Option<usize>) {
            (0, Some(0))
        }
    }

    impl Hashable for Position {
        fn hash(&self, state: &mut Hasher) {
            use std::any::TypeId;
            state.write_u64(*unsafe { std::mem::transmute::<_,&u64>(&TypeId::of::<Self>()) });
            state.write_u8(*self as u8);
        }
    }


    impl<T> Walkable<'a,AtomicFirstSet<T>> for Position  where T: Copy + Eq + PartialEq + Hash + Debug {
        #[inline(always)]
        fn iter(&'a self) -> Option<WalkIterator<AtomicFirstSet<T>>> { None }
        fn action(&'a self, _: &'a <AtomicFirstSet<T> as WalkType>::Item) -> walk::Action<AtomicFirstSet<T>> {
            Action::new(None, false, 0)
        }
    }

    impl<T> Walkable<'a,FirstSet<T>> for Position where T: Copy + Eq + PartialEq + Hash + Debug {
        #[inline(always)]
        fn iter(&'a self) -> Option<WalkIterator<FirstSet<T>>> { None }
        fn action(&'a self, _: &'a <FirstSet<T> as WalkType>::Item) -> walk::Action<FirstSet<T>> {
            Action::new(None, false, 0)
        }
    }

    impl Nullable for Position {
        #[inline(always)]
        fn is_nullable(&self) -> bool { true }
    }

    /* **************************************************************** */

    /// Direction in which a pattern anchor should attempt to match
    /// its pattern.
    #[derive(Debug,Copy,Clone)]
    pub enum Direction {
        Backward,
        Forward
    }

    /// Look-ahead or look-behind pattern anchor.
    #[derive(Debug,Clone)]
    pub struct Look<T> {
        direction: Direction,
        pattern: Box<Pattern<T>>
    }

    impl<T> Pattern<T> for Look<T> where T: Copy + Eq + Hash + std::fmt::Debug + Reflect {
        fn is_fixed(&self) -> bool { self.pattern.is_fixed() }
        fn input_length_bounds(&self) -> (usize, Option<usize>) {
            self.pattern.input_length_bounds()
        }
    }


    /// Remove a reference from a type.
    ///
    /// Used internally by some other cripes code.
    #[doc(hidden)]
    trait RemoveRef { type Type; }
    impl<T> RemoveRef for &'a T { type Type = T; }


    /// Horrific hack to clone a reference.  This should be used **only** until
    /// I figure out why the borrow-checker doesn't like
    /// pass-through functions.
    macro_rules! clone_ref {
        ($r: ident : $t: ty) => { {use std::mem::transmute;
                                   let o: $t = transmute(transmute::<_,*const <$t as RemoveRef>::Type>($r));
                                   o} };
    }

    impl<'b, 'a: 'b, T> Walkable<'a,FirstSet<T>> for Look<T> where T: Copy + Eq + PartialEq + Hash + Debug {
        #[inline(always)]
        fn iter(&'a self) -> Option<WalkIterator<FirstSet<T>>> {
            <Pattern<T> as Walkable<'a,FirstSet<T>>>::iter(&*self.pattern)
        }
        fn action(&'a self, item: &'a <FirstSet<T> as WalkType>::Item) -> walk::Action<FirstSet<T>> {
            self.pattern.action(unsafe { clone_ref!(item: &'a <FirstSet<T> as WalkType>::Item) })
        }
    }

    impl<T> Walkable<'a,AtomicFirstSet<T>> for Look<T>  where T: Copy + Eq + PartialEq + Hash + Debug {
        #[inline(always)]
        fn iter(&'a self) -> Option<WalkIterator<AtomicFirstSet<T>>> {
            <Pattern<T> as Walkable<'a,AtomicFirstSet<T>>>::iter(&(*self.pattern))
        }
        fn action(&'a self, item: &'a <AtomicFirstSet<T> as WalkType>::Item) -> walk::Action<AtomicFirstSet<T>> {
            self.pattern.action(unsafe { clone_ref!(item: &'a <FirstSet<T> as WalkType>::Item) })
        }
    }

    impl<T> Nullable for Look<T> {
        #[inline(always)]
        fn is_nullable(&self) -> bool { self.pattern.is_nullable() }
    }

    impl<T> Hashable for Look<T> where T: Reflect {
        #[inline(always)]
        fn hash(&self, state: &mut Hasher) {
            state.write_u64(*unsafe { std::mem::transmute::<_,&u64>(&std::any::TypeId::of::<Look<'static,T>>()) });
            <Pattern<T> as Hashable>::hash(&*self.pattern, state);
        }
    }
}
*/
