//! Utilities for generalized iterators over trees of arbitrary types.

use super::iter::*;
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

    impl<Type> Action<Type> where Type: WalkType {
        pub fn new(y: Option<<Type as WalkType>::Yield>, r: bool, f: u8) -> Action<Type> {
            Action{yield_value: y, recurse: r, flags: f}
        }
    }

}


/// Defines a particular kind of walk.
pub trait WalkType {
    /// Type yielded by iterators for this trait.
    type Yield;

    /// Value produced by base iterators for the container being walked.
    type Item;
}



/// Interface for patterns whose elements may be visited in
/// a runtime-specified manner.
///
/// @tparam Type Marker type used to discriminate the "type" of walk being
///     implemented.  A particular type may be walked for different purposes,
///     such as to visit *all* elements, or only certain ones.
///
/// @tparam 
pub trait Walkable<'a,Type>: Iterable<'a,Box<Iterator<Item=<Type as WalkType>::Item>>> /*Iterable<'a,IterBox<'a,&'a <Type as WalkType>::Item>>*/
where Type: WalkType {
    /// Determine the action to take for a given node in the implementor's
    /// element tree.
    ///
    /// @param element Pattern element to process.
    ///
    /// @return Tuple containing the optionally-yielded element, and a set of
    ///     flags indicating how to proceed.  Action::RECURSE is only valid
    ///     when `Item` implements Walkable<Tag>.
    fn action(&'a self, element: <Type as WalkType>::Item)
                  -> Action<Type>;
}

/// Iterator used to perform a particular walk on a particular type.
///
/// @tparam Tag Walk-type tag.
///
/// @tparam Yield `Item` type for the iterator implementation of
///     the structure.
///
/// @tparam T Top-level Walkable type for which this structure is an iterator.
pub struct Walker<'a,Type>
    where Type: WalkType,
          <Type as WalkType>::Item: 'a {

    walkable: &'a Walkable<'a,Type>,
    base_iterator: Option<Box<Iterator<Item=<Type as WalkType>::Item> + 'a>>,
    //base_iterator: Option<IterBox<'a,&'a <Type as WalkType>::Item>>,
    sub_iterator: Option<Box<Iterator<Item=<Type as WalkType>::Yield> + 'a>>
    //sub_iterator: Option<IterBox<'a,<Type as WalkType>::Yield>>
}

impl<'a,Type> Walker<'a,Type>
    where Type: WalkType,
          <Type as WalkType>::Item: 'a {
    /// Create a new walker iterator from the given base iterator.
    pub fn new(base: &'a Walkable<'a,Type>) -> Walker<'a,Type> {
        Walker{walkable: base,
               base_iterator: Some(base.iter()),
               sub_iterator: None}
    }
}

impl<'a,Type> Iterator for Walker<'a,Type>
where Type: 'a + WalkType,
      <Type as WalkType>::Yield: 'a,
      <Type as WalkType>::Item: 'a + Iterable<'a,Box<Iterator<Item=<Type as WalkType>::Yield>+'a>> + Walkable<'a,Type>,/*Iterable<'a,IterBox<'a,<Type as WalkType>::Yield>> + Walkable<'a,Type>*/
{
    type Item = <Type as WalkType>::Yield;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        let mut terminate: bool = false;
        loop {
            if terminate { self.base_iterator = None;
                           break; }

            // If the sub-iterator exists and isn't exhausted, return its
            // yielded value...
            if let Some(ref mut iter) = self.sub_iterator {
                let yielded_opt = (*iter).next();
                if yielded_opt.is_some() {
                    return yielded_opt }
            }
            // ...otherwise clear it and check if the base iterator exists
            self.sub_iterator = None;
            if let Some(ref mut iter) = self.base_iterator {
                if let Some(ref element) = (*iter).next() {
                    let action = self.walkable.action(*element);

                    // FIXME: need to provide a way to specify order in which
                    // to recurse vs. yield.  The solution should allow for
                    // selection of different traversal methods -- need to come
                    // up with an API for that.
                    if action.recurse {
                        self.sub_iterator = Some(Box::new(Walker::new(element as &Walkable<'a,Type>)));

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
