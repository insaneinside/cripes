//! Support for patterns of arbitrary element types.
use std;

mod walk;
use self::walk::{action,Action,Walkable,Walker};

/// An iterator that produces its input item exactly once.
struct Once<'a,T: 'a>(Option<&'a T>);

impl<'a,T> Once<'a,T> {
    fn new(obj: &'a T) -> Once<'a,T> {
        Once(Some(obj))
    }
}

impl<'a,T> Iterator for Once<'a,T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<&'a T> {
        let out = self.0;
        self.0 = None;
        out
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self.0 {
            Some(..) => (1, Some(1)),
            None => (0, Some(0))
        }
    }
}


use self::PatternWalk::*;
enum PatternWalk {
    /// A pattern's first set is the set of elements that may begin a sequence
    /// matching that pattern.
    FirstSet,

    /// The atomic first set is like the first set above, but yields only the
    /// atomic values within the set.
    AtomicFirstSet
}

pub type ElementIterator<'a,T> = Box<Iterator<Item=&'a Element<T>> + 'a>;
pub type AtomicIterator<'a,T> = Box<Iterator<Item=&'a T> + 'a>;


/// Provides a method to determine whether a pattern is "nullable", i.e., can
/// match an empty sequence.
pub trait Nullable {
    /// Check if the token is nullable.
    fn is_nullable(&self) -> bool { false }
}



/// A component in a pattern.  Each element is either an atomic value, or
/// a Pattern trait object.
pub enum Element<T> {
    Atom(T),
    Pattern(Box<Pattern<T>>)
}


/// Interface for descriptions of syntactic patterns.
pub trait Pattern<T>: Walkable<PatternWalk::FirstSet,Element> + Walkable<PatternWalk::AtomicFirstSet,T> + Nullable {}

impl<'a,T> Nullable for Element<T> {
    fn is_nullable(&self) -> bool {
        match *self {
            Element::Atom(..) => false,
            Element::Pattern(ref pat) => pat.is_nullable(),
        }
    }
}

            


/// A sequence of atoms or subpatterns.
struct Sequence<T> {
    elements: Vec<Element<T>>
}


impl<'a,T: 'a> Walkable<AtomicFirstSet,&T> for Sequence<T> {
    type BaseIterator = std::slice::Iter<'a,Element<T>>;

    #[inline(always)]
    fn iter(&self) -> <Self as Walkable<AtomicFirstSet,&T>>::BaseIterator { self.elements.iter() }

    fn action<'a>(&self, element: &'a Element<T>) -> (Option<&T>, u8) {
        match *element {
            Element::Atom(..) =>
                Action{yield_value: Some(element),
                       recurse_on: None,
                       flags: action::TERMINATE},
            Element::Pattern(ref pat) =>
                Action{yield_value: None,
                       recurse_on: Some(element),
                       flags: if pat.is_nullable() { 0 } else { action::TERMINATE }}
        }
    }
}


impl<'a,T: 'a> Walkable<FirstSet,&Element<T>> for Sequence<T> {
    type BaseIterator = std::slice::Iter<'a,Element<T>>;

    #[inline(always)]
    fn iter(&self) -> <Self as Walkable<FirstSet,&Element<T>>>::BaseIterator { self.elements.iter() }

    #[inline(always)]
    fn action<'a>(&self, element: &'a Element<T>) -> (Option<&Element<T>>, u8) {
        match *element {
            Element::Atom(..) =>
                Action{yield_value: Some(element),
                       recurse_on: None,
                       flags: action::TERMINATE},
            Element::Pattern(ref pat) =>
                Action{yield_value: Some(element),
                       recurse_on: Some(element),
                       flags: if pat.is_nullable() { 0 } else { action::TERMINATE }}
        }
    }
}

impl<T> Nullable for Sequence<T> {
    fn is_nullable(&self) -> bool {
        if self.elements.is_empty() { true }
        else {
            for elt in self.elements.iter() {
                match *elt {
                    Element::Atom(..) => return false,
                    Element::Pattern(pat) => if ! pat.is_nullable() { return false; }
                }
            }
            true
        }
    }
}
