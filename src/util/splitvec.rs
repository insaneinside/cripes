//! Types for Vec-based storage with stack-like pushable states.


use std::fmt;
use std::mem;
use std::ptr;
use std::slice;
use std::convert::From;
use std::default::Default;
use std::ops::{Deref, DerefMut, Index, Range, RangeFrom, RangeFull, RangeTo};

/// Vec-like type with support for saving and restoring states via stack-like
/// operations
///
/// 
#[derive(Clone)]
pub struct SplitVec<T> {
    data: Vec<T>,
    splits: Vec<usize>
}

impl<T> SplitVec<T> {

    /// Create a new, empty SplitVec instance.
    #[inline]
    pub fn new() -> Self {
        SplitVec{data: Vec::new(), splits: Vec::new()}
    }

    /// Create a new, empty SplitVec instance with space for the specified
    /// number of entries.
    #[inline]
    pub fn with_capacity(data_capacity: usize, splits_capacity: usize) -> Self {
        SplitVec{data: Vec::with_capacity(data_capacity), splits: Vec::with_capacity(splits_capacity)}
    }

    /// Determine the offset of the first item in the current state.
    #[inline]
    fn offset(&self) -> usize {
        self.splits.last().cloned().unwrap_or_else(|| 0)
    }

    /// Mark a split above the current working state.
    ///
    /// This creates a new, empty state at the top of the `SplitVec`'s stack;
    /// you can use `pop_state` to return to the previous state or
    /// `pop_and_merge_states` to merge the current state with the
    /// previous one.
    #[inline]
    pub fn push_state(&mut self) {
        self.splits.push(self.data.len());
    }

    /// Mark a split above the current working state, then copy the contents of
    /// that state into the new working area.
    pub fn push_and_copy_state(&mut self) where T: Copy {
        let ofs = self.offset();
        let count = self.len();
        self.push_state();
        self.data.reserve(count);
        unsafe {
            let src = self.data.as_ptr().offset(ofs as isize);
            let dst = self.data.as_mut_ptr().offset((ofs + count) as isize);
            ptr::copy_nonoverlapping(src,
                                     dst,
                                     count);
            self.data.set_len(ofs + 2 * count);
        }
    }

    /// Restore the most-recently-pushed state, discarding all data added to
    /// the working vector since the last call to `push_state`.
    #[inline]
    pub fn pop_state(&mut self) {
        self.data.truncate(self.splits.pop().expect("Call to `pop_state` without prior matching `push_state` call"));
    }

    /// Remove the split between the current and previous states; this has the
    /// effect of merging their contents into the previous state.
    ///
    /// Calling this method will have no effect if no prior state exists.
    ///
    /// # Examples
    ///    
    /// ```rust
    /// # extern crate cripes;
    /// # use cripes::util::splitvec::SplitVec;
    /// # fn main() {
    /// let mut s = SplitVec::from(vec![1, 2, 3]);
    /// s.push_state();
    ///
    /// s.extend_from_slice(&[4, 5, 6]);
    /// assert_eq!(&*s, &[4, 5, 6]);
    ///
    /// s.pop_and_merge_states();
    /// assert_eq!(&*s, &[1, 2, 3, 4, 5, 6]);
    /// # }
    /// ```
    #[inline]
    pub fn pop_and_merge_states(&mut self) {
        self.splits.pop();
    }

    /// Get the number of items in the working state.
    #[inline]
    pub fn len(&self) -> usize {
        self.data.len() - self.offset()
    }

    /// Truncate the working state to at most a given number of elements.
    #[inline]
    pub fn truncate(&mut self, len: usize) {
        let ofs = self.offset();
        self.data.truncate(ofs + len)
    }

    /// Push an item onto the end of the working state.
    pub fn push(&mut self, value: T) {
        self.data.push(value)
    }


    /// Remove the top-most item from the working state and return it.
    ///
    /// # Examples
    ///    
    /// ```rust
    /// # extern crate cripes;
    /// # use cripes::util::splitvec::SplitVec;
    /// # fn main() {
    /// let mut s = SplitVec::new();
    /// assert_eq!(None, s.pop());
    ///
    /// s.extend_from_slice(&[1, 2, 3]);
    /// assert_eq!(Some(3), s.pop());
    /// # }
    /// ```
    ///
    /// `pop` works only within the current state, and will return `None` if the
    /// current state is empty even if a prior state is not:
    /// 
    /// ```rust
    /// # extern crate cripes;
    /// # use cripes::util::splitvec::SplitVec;
    /// # fn main() {
    /// let mut s = SplitVec::from(vec![1, 2, 3]);
    /// s.push_state();
    ///
    /// assert_eq!(None, s.pop());
    /// # }
    /// ```
    pub fn pop(&mut self) -> Option<T> {
        if self.len() > 0 { self.data.pop() }
        else { None }
    }

    /// Extend the current working state from a slice.
    #[inline]
    pub fn extend_from_slice<'a>(&mut self, slice: &'a [T]) where T: Copy {
        self.data.extend_from_slice(slice)
    }

    /// Get a reference to the current working state as a slice.
    #[inline]
    pub fn as_slice(&self) -> &[T] {
        self
    }

    /// Remove adjacent duplicates in the working state.
    pub fn dedup(&mut self) where T: PartialEq {
        unsafe {
            let ofs = self.offset();
            let ln = self.data.len();
            if ln <= ofs {
                return;
            }

            let p = self.data.as_mut_ptr();
            let mut r: usize = 1 + ofs;
            let mut w: usize = 1 + ofs;

            while r < ln {
                let p_r = p.offset(r as isize);
                let p_wm1 = p.offset((w - 1) as isize);
                if *p_r != *p_wm1 {
                    if r != w {
                        let p_w = p_wm1.offset(1);
                        mem::swap(&mut *p_r, &mut *p_w);
                    }
                    w += 1;
                }
                r += 1;
            }

            self.truncate(w - ofs);
        }
    }

    /// Iterate over all states contained within the SplitVec.
    ///
    /// The final state is guaranteed to be the current working state.
    pub fn iter_states<'a>(&'a self) -> StateIter<'a, T> {
        StateIter{splits_iter: self.splits.iter(), data: &self.data[..], last_split: Some(0)}
    }
}

/// Iterator over the states of a SplitVec as slices.
///
/// Instances of `StateIter` are obtained by calling `iter_states` on
/// a [`SplitVec`](struct.SplitVec.html).
pub struct StateIter<'a,T> where T: 'a {
    splits_iter: slice::Iter<'a,usize>,
    data: &'a [T],
    last_split: Option<usize>
}

impl<'a, T: 'a> Iterator for StateIter<'a, T> {
    type Item = &'a [T];
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(start) = self.last_split {
            if let Some(end) = self.splits_iter.next() {
                self.last_split = Some(*end);
                Some(&self.data[start..*end])
            } else {
                self.last_split = None;
                Some(&self.data[start..])
            }
        } else { None }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.splits_iter.size_hint()
    }
}

use itertools::Itertools;
impl<T> fmt::Debug for SplitVec<T> where T: fmt::Debug {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]",
               self.iter_states().map(|s| s.iter().map(|ref elt| format!("{:?}", elt)).join(", "))
               .join(" | "))
    }
}

impl<T> AsRef<SplitVec<T>> for SplitVec<T> {
    fn as_ref(&self) -> &Self {
        self
    }
}
impl<T> AsMut<SplitVec<T>> for SplitVec<T> {
    fn as_mut(&mut self) -> &mut Self {
        self
    }
}

impl<T> AsRef<[T]> for SplitVec<T> {
    fn as_ref(&self) -> &[T] {
        self
    }
}
impl<T> AsMut<[T]> for SplitVec<T> {
    fn as_mut(&mut self) -> &mut [T] {
        self
    }
}


impl<T> From<Vec<T>> for SplitVec<T> {
    fn from(v: Vec<T>) -> Self {
        SplitVec{data: v, splits: Vec::new()}
    }
}

impl<'a,T> From<&'a [T]> for SplitVec<T> where T: Clone {
    fn from(s: &'a [T]) -> Self {
        SplitVec{data: s.into(), splits: Vec::new()}
    }
}


impl<T> Default for SplitVec<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}


impl<T> Extend<T> for SplitVec<T> {
    fn extend<I>(&mut self, iter: I) where I: IntoIterator<Item=T> {
        self.data.extend(iter);
    }
}


impl<T> Deref for SplitVec<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        let o = self.offset();
        &self.data[o..]
    }
}

impl<T> DerefMut for SplitVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let o = self.offset();
        &mut self.data[o..]
    }
}

impl<T> Index<usize> for SplitVec<T> {
    type Output = T;
    #[inline]
    fn index<'a>(&'a self, idx: usize) -> &'a T {
        let o = self.offset();
        &self.data[idx + o]
    }
}

impl<T> Index<Range<usize>> for SplitVec<T> {
    type Output = [T];
    #[inline]
    fn index<'a>(&'a self, r: Range<usize>) -> &'a [T] {
        let o = self.offset();
        &self.data[(o + r.start)..(o + r.end)]
    }
}
impl<T> Index<RangeFrom<usize>> for SplitVec<T> {
    type Output = [T];
    #[inline]
    fn index<'a>(&'a self, r: RangeFrom<usize>) -> &'a [T] {
        let o = self.offset();
        &self.data[(o + r.start)..]
    }
}
impl<T> Index<RangeTo<usize>> for SplitVec<T> {
    type Output = [T];
    #[inline]
    fn index<'a>(&'a self, r: RangeTo<usize>) -> &'a [T] {
        let o = self.offset();
        &self.data[..(o + r.end)]
    }
}
impl<T> Index<RangeFull> for SplitVec<T> {
    type Output = [T];
    #[inline(always)]
    fn index<'a>(&'a self, _: RangeFull) -> &'a [T] {
        &self.data[..]
    }
}
