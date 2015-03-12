//! Support for patterns of arbitrary element types.
use std;
use std::marker::{PhantomData};

pub mod walk;
pub mod iter;

use self::walk::*;
use self::iter::*;

/// Interface for descriptions of syntactic patterns.
pub trait Pattern<'a,T>:
    Walkable<'a,FirstSet<T>> +
    Walkable<'a,AtomicFirstSet<T>> +
    Nullable + std::fmt::Debug
where T: Copy + std::fmt::Debug {}

/// A pattern's first set is the set of elements that may begin a sequence
/// matching that pattern.
pub struct FirstSet<T> { _marker: PhantomData<T> }

impl<'a,T> WalkType for FirstSet<T> {
    type Yield = &'a Element<'a,T>;
    type Item = Element<'a,T>;
}

/// The atomic first set is like the first set above, but yields only the
/// atomic values within the set.
pub struct AtomicFirstSet<T> { _marker: PhantomData<T>}
impl<'a,T> WalkType for AtomicFirstSet<T>  {
    type Yield = T;
    type Item = Element<'a,T>;
}


/// Provides a method to determine whether a pattern is "nullable", i.e., can
/// match an empty sequence.
pub trait Nullable {
    /// Check if the token is nullable.
    fn is_nullable(&self) -> bool { false }
}


/* **************************************************************** */
/// A component in a pattern.  Each element is either an atomic value, or
/// a Pattern trait object.
#[derive(Debug)]
pub enum Element<'a,T> {
    Atom(T),
    Pattern(Box<Pattern<'a,T>>)
}

impl<'a,T> WalkableExt for Element<'a,T> {}

impl<'a,T> Pattern<'a,T> for Element<'a,T> where T: 'a + Copy + std::fmt::Debug {}

//impl<'a,T> Iterable<'a, IterBox<'a,&'a <FirstSet<T> as WalkType>::Item>> for Element<'a,T>
impl<'a,T> Iterable<'a, Box<Iterator<Item=&'a <FirstSet<T> as WalkType>::Item>>> for Element<'a,T>
where T: 'a {
    #[inline(always)]
    fn iter(&'a self) -> Box<Iterator<Item=&'a Element<'a,T>> + 'a> {//Box<Iterator<Item=<FirstSet<T> as WalkType>::Item> + 'a> /*IterBox<'a,&'a <FirstSet<T> as WalkType>::Item>*/ {
        Box::new(Once::new(self))
    }
}

impl<'a,T> Walkable<'a,FirstSet<T>> for Element<'a,T>
where T: 'a {
    fn action(&'a self, element: &'a <FirstSet<T> as WalkType>::Item) -> walk::Action<FirstSet<T>> {
        match *element {
            Element::Atom(..) => Action::new(Some(element), false, action::TERMINATE as u8),
            Element::Pattern(..) => Action::new(Some(element), true, action::TERMINATE as u8)
        }
    }
}

impl<'a,T> Walkable<'a,AtomicFirstSet<T>> for Element<'a,T> where T: 'a + Copy {
    fn action(&'a self, element: &'a <AtomicFirstSet<T> as WalkType>::Item) -> walk::Action<AtomicFirstSet<T>> {
        match *element {
            Element::Atom(atom) => Action::new(Some(atom), false, action::TERMINATE as u8),
            Element::Pattern(..) => Action::new(None, true, action::TERMINATE as u8)
        }
    }
}

impl<'a,T> Nullable for Element<'a,T> {
    fn is_nullable(&self) -> bool {
        match *self {
            Element::Atom(..) => false,
            Element::Pattern(ref pat) => pat.is_nullable(),
        }
    }
}



/* **************************************************************** */
/// A sequence of atoms or subpatterns.
#[derive(Debug)]
pub struct Sequence<'a,T> {
    elements: Vec<Element<'a,T>>
}

impl<'a,T> Sequence<'a,T> {
    pub fn new(elements: Vec<Element<'a,T>>) -> Sequence<'a,T> {
        Sequence{elements: elements}
    }
}

impl<'a,T> Pattern<'a,T> for Sequence<'a,T> where T: 'a + Copy + std::fmt::Debug {}


//impl<'a,T> Iterable<'a,IterBox<'a,&'a <FirstSet<T> as WalkType>::Item>> for Sequence<'a,T>
impl<'a,T> Iterable<'a,Box<Iterator<Item=&'a <FirstSet<T> as WalkType>::Item>>> for Sequence<'a,T>
where T: 'a {
    #[inline(always)]
    fn iter(&'a self) -> Box<Iterator<Item=&'a <FirstSet<T> as WalkType>::Item> + 'a> {
        Box::new(self.elements.iter())
    }
}

// TODO: impl<'a,T> Sequence<'a,T>

impl<'a,T> Walkable<'a,AtomicFirstSet<T>> for Sequence<'a,T> where T: 'a + Copy {
    fn action(&'a self, element: &'a <AtomicFirstSet<T> as WalkType>::Item) -> walk::Action<AtomicFirstSet<T>> {
        match *element {
            Element::Atom(atom) =>
                Action::new(Some(atom), false, action::TERMINATE as u8),
            Element::Pattern(ref pat) =>
                Action::new(None, true, if pat.is_nullable() { 0 } else { action::TERMINATE as u8 })
        }
    }
}


impl<'a,T> Walkable<'a,FirstSet<T>> for Sequence<'a,T>
where T: 'a {
    #[inline(always)]
    fn action(&'a self, element: &'a <AtomicFirstSet<T> as WalkType>::Item) -> walk::Action<FirstSet<T>> {
        match *element {
            Element::Atom(..) =>
                Action::new(Some(element), false, action::TERMINATE as u8),
            Element::Pattern(ref pat) =>
                Action::new(Some(element), true, if pat.is_nullable() { 0 } else { action::TERMINATE as u8 })
        }
    }
}

impl<'a,T> Nullable for Sequence<'a,T> {
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
