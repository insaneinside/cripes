//! Utilities for generalized iterators over trees of arbitrary types.

use std;
use std::iter::IntoIterator;

use util::flex_box::*;
pub use self::action::Action;

/// Descriptions of actions that may be taken on a given node when traversing
/// a pattern tree.
pub mod action {
    use super::WalkType;

    enum Flag {
        /// Terminate iteration after performing other specified actions.
        TERMINATE = 1,
    }

    pub use self::Flag::*;

    pub struct Action<Type> where Type: WalkType {
        pub yield_value: Option<<Type as WalkType>::Yield>,
        pub recurse: bool,
        pub flags: u8
    }

    /*impl<'a,Type> Action<Type> where Type: WalkType, <Type as WalkType>::Item: super::Walkable<Type> {
        #[inline]
        pub fn new(y: Option<<Type as WalkType>::Yield>, r: bool, f: u8) -> Action<Type> {
            Action{yield_value: y, recurse: r, flags: f}
        }
    }*/

}


/// Defines a particular kind of walk.
pub trait WalkType {
    /// Type yielded by iterators for this trait.
    type Yield;
}

/// Interface for patterns whose elements may be visited in
/// a runtime-specified manner.
///
/// @tparam Type Walk type this trait works with.
pub trait Walkable<Walk: WalkType>
{
    type Item: Walkable<WalkType<Yield=<Walk as WalkType>::Yield>,
                        Item=Self::Item>;

    /// Determine the action to take for a given node in the implementor's
    /// element tree.
    ///
    /// @param element Pattern element to process.
    ///
    /// @return Structure describing the action to be performed for the
    ///     given element.
    fn action<'a>(&'a self, element: &'a Self::Item) -> Action<Walk>
        where &'a Self: IntoIterator;

}

/// Provides additional helper methods for implementers of a particular walk type.
/*pub trait WalkableExt {
    #[inline(always)]
    fn walk<'a,'b,W>(&'a self) -> Walker<'b,W,Self>
    where 'a: 'b,
          Self: 'b + Sized + Walkable<W>,
          W: 'b + WalkType,
          <W as WalkType>::Yield: 'b,
          <W as WalkType>::Item: 'b
          //,<Self as Iterable<<W as WalkType>::Item>>::Iterator: 'b
{
        Walker::new(self)
    }
}*/


/// Iterator used to perform a particular walk on a particular type.
///
/// @tparam Walk Walk type
/// @tparam T Type of the value being walked.
pub struct Walker<'a, Walk, T>
    where Walk: 'a + WalkType,
          <Walk as WalkType>::Yield: 'a,
          <T as Walkable<Walk>>::Item: 'a,
          T: 'a + Walkable<Walk>,
          &'a T: IntoIterator,
          <&'a T as IntoIterator>::Item: 'a,
          <&'a T as IntoIterator>::IntoIter: 'a
{
    walkable: &'a T,
    base_iterator: Option<Ref<'a,Iterator<Item=<T as Walkable<Walk>>::Item>>>,
    sub_iterator_box: FlexBox,
    sub_iterator: Option<Ref<'a,Walker<'a,Walk,T>>>
}


impl<'a,Walk,T> Walker<'a,Walk,T>
    where Walk: 'a + WalkType,
          <Walk as WalkType>::Yield: 'a,
          <T as Walkable<Walk>>::Item: 'a,
          T: 'a + Walkable<Walk>,
          &'a T: IntoIterator,
          <&'a T as IntoIterator>::Item: 'a
          //,<T as Iterable<<Walk as WalkType>::Item>>::Iterator: 'a

{
    /// Create a new walker iterator from the given base iterator.
    pub fn new(base: &'a T) -> Walker<'a,Walk,T> {
        Walker{walkable: base,
               base_iterator: Some(base.into_iter()),
               sub_iterator_box: FlexBox::new(),
               sub_iterator: None}
    }
}

impl<'a,Type,T> std::iter::Iterator for Walker<'a,Type,T>
where Type: 'a + WalkType,
      <Type as WalkType>::Yield: 'a,
      <T as Walkable<Type>>::Item: 'a + Walkable<Type>,
      &'a T: IntoIterator,
      <&'a T as IntoIterator>::Item: 'a,
      T: 'a + Walkable<Type>,
{
    type Item = <Type as WalkType>::Yield;

    fn next(&mut self) -> Option<<Type as WalkType>::Yield> {
        let mut terminate: bool = false;
        loop {
            if terminate { self.base_iterator = None;
                           break; }

            // If the sub-iterator exists and isn't exhausted, return its
            // yielded value...
            if let Some(ref mut iter) = self.sub_iterator {
                let yielded_opt = iter.next();
                if yielded_opt.is_some() {
                    return yielded_opt }
            }
            // ...otherwise clear it and check if the base iterator exists
            self.sub_iterator = None;
            if let Some(ref mut iter) = self.base_iterator {
                if let Some(ref element) = iter.next() {
                    let action = self.walkable.action(element);

                    // FIXME: need to provide a way to specify order in which
                    // to recurse vs. yield.  The solution should allow for
                    // selection of different traversal methods -- need to come
                    // up with an API for that.
                    if action.recurse {
                        self.sub_iterator = Some(self.sub_iterator_box.store(element.walk::<Type>()));

                        // Recurse immediately if we have no value to yield.
                        if ! action.yield_value.is_some() {
                            continue;
                        }
                    }

                    // For now we'll just do depth-first, since it's simplest.
                    if action.yield_value.is_some() {
                        return action.yield_value;
                    }

                    // Clear the base iterator if we were asked to terminate
                    // the walk.  Note that any recursive walks on
                    // `self.sub_iterator` will continue until that iterator
                    // is exhausted.
                    if (action.flags & action::TERMINATE as u8) != 0 {
                        terminate = true; }
                } else {
                    // Base iterator is exhausted; clear it.
                    terminate = true;
                }
            }
        }
        if terminate { self.base_iterator = None; }
        return None;
    }

    /// We implement `size_hint` as the sum of the hints for the current base-
    /// and sub-iterators, if they're not `None`.  We return an upper bound
    /// only if both iterators are `None`.
    fn size_hint(&self) -> (usize, Option<usize>) {
        let mut min = 0;
        if let Some(ref iter) = self.sub_iterator { min += iter.size_hint().0 }
        if let Some(ref iter) = self.base_iterator { min += iter.size_hint().0 }

        (min, if self.sub_iterator.is_none() && self.base_iterator.is_none() { Some(0) } else { None })
    }
}
