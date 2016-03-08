//! Iterators that support the `pattern` module.
use std::marker::PhantomData;
use std::iter::ExactSizeIterator;

/* **************************************************************** */
/// An iterator that produces its input item exactly once.
///
/// ```rust
/// #[macro_use] extern crate cripes;
/// use cripes::util;
/// use cripes::util::iter;
/// # fn main() {
/// let one_four_two = iter::Once::new(42);
/// panic_unless!(one_four_two.count() == 1, "`Once` iterator should be valid for exactly one iteration.");
/// # }
/// ```
///
///
///
#[derive(Debug)]
pub struct Once<T>(Option<T>);

impl<T> Once<T> {
    /// Create a `Once` iterator instance that will produce the given object.
    #[inline(always)]
    pub fn new(obj: T) -> Once<T> {
        Once(Some(obj))
    }
}

impl<T> ExactSizeIterator for Once<T> where T: Copy {
    #[inline(always)]
    fn len(&self) -> usize { 1 }
}

impl<T> Iterator for Once<T> where T: Copy {
    type Item = T;

    #[inline(always)]
    fn next(&mut self) -> Option<T> {
        if let Once(Some(val)) = *self {
            self.0 = None;
            Some(val) }
        else { None }
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self.0 {
            Some(..) => (1, Some(1)),
            None => (0, Some(0))
        }
    }
}
/* **************************************************************** */
/// An empty iterator.
#[derive(Debug)]
pub struct Empty<T> { _marker: PhantomData<T> }

impl<T> Empty<T> {
    /// Create an empty iterator.
    #[inline(always)]
    pub fn new() -> Empty<T> {
        Empty{_marker: PhantomData}
    }
}

impl<T> ExactSizeIterator for Empty<T> {
    #[inline(always)]
    fn len(&self) -> usize { 0 }
}

impl<T=()> Iterator for Empty<T> {
    type Item = T;

    #[inline(always)]
    fn next(&mut self) -> Option<T> { None }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) { (0, Some(0)) }
}

/* **************************************************************** */
// Interface for types that provide some form of iterator.
// pub trait Iterable<'t,Iter> where Iter: 't {
//     fn iter(&'t self) -> Iter;
// }
