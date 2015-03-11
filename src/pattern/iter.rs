//! Iterators that support the `pattern` module.
use std;
use std::mem::*;

/* **************************************************************** */
/// An iterator that produces its input item exactly once.
#[derive(Debug)]
pub struct Once<T>(Option<T>);

impl<T> Once<T> {
    #[inline(always)]
    pub fn new(obj: T) -> Once<T> {
        Once(Some(obj))
    }
}

impl<T> Iterator for Once<T> where T: Copy {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        if let Once(Some(val)) = *self {
            self.0 = None;
            Some(val) }
        else {
            None }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self.0 {
            Some(..) => (1, Some(1)),
            None => (0, Some(0))
        }
    }
}


/* **************************************************************** */
/// Interface for types that provide some form of iterator.
pub trait Iterable<'t,Iter> where Iter: 't {
    fn iter(&'t self) -> Iter;
}
