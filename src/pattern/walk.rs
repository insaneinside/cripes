//! Utilities for generalized iterators over trees of arbitrary types.

use std;

/// Descriptions of actions that may be taken on a given node when traversing
/// a pattern tree.
pub mod action {
    enum Flag {
        /// Terminate iteration after performing other specified actions.
        TERMINATE = 1,
    }

    pub use self::Flag::*;

    pub struct Action<U,V> {
        yield_value: Option<U>,
        recurse_on: Option<V>,
        flags: u8
    }
}

pub use self::action::Action;

/// Interface for patterns whose elements may be visited in
/// a runtime-specified manner.
///
/// @tparam Tag Marker type used to discriminate the "type" of walk being
///     implemented.  A particular type may be walked for different purposes,
///     such as to visit *all* elements, or only certain ones.
///
/// @tparam Yielded Type of item the walkable should implement an iterator for.
trait Walkable<Tag,Yielded>: std::marker::PhantomFn<Tag> {
    /// Iterator type for the implementer's underlying container.
    type BaseIterator;
    type Item = <Self::BaseIterator as Iterator>::Item;
    type Action = Action<Yielded,Self::Item>;

    /// Fetch the initial iterator to use for a walk.
    fn iter(&self) -> Self::BaseIterator;

    /// Determine the action to take for a given node in the implementor's
    /// element tree.
    ///
    /// @param element Pattern element to process.
    ///
    /// @return Tuple containing the optionally-yielded element, and a set of
    ///     flags indicating how to proceed.  Action::RECURSE is only valid
    ///     when `Item` implements Walkable<Yielded>.
    fn action<'a>(element: &'a Self::Item) -> Self::Action
        where Self::Item: Walkable<Tag,Yielded>;
}

// FIXME: should the order of `T` and `Yielded` be reversed?
/// Iterator used to perform a particular walk on a particular type.
///
/// @tparam Tag Walk-type tag.
///
/// @tparam Yielded `Item` type for the iterator implementation of
///     the structure.
///
/// @tparam T Top-level Walkable type for which this structure is an iterator.
struct Walker<Tag,Yielded,T> where T: Walkable<Tag,Yielded> {
    base_iterator: Option<<T as Walkable<Tag,Yielded>>::BaseIterator>,
    sub_iterator: Option<Walker<Tag,Yielded,Yielded>>,
}

impl<Tag,Yielded,T> Walker<Tag,Yielded,T>
where T: Walkable<Tag,Yielded> {
    /// Create a new walker iterator from the given base iterator.
    pub fn new(base: <T as Walkable<Tag,Yielded>>::BaseIterator) -> Walker<Tag,Yielded,T> {
        Walker{base_iterator: base, sub_iterator: None}
    }
}

impl<'a,Tag,Yielded,T> Iterator for Walker<Tag,Yielded,T>
where T: Walkable<Tag,Yielded>, Yielded: Walkable<Tag,Yielded> {
    type Item = &'a Yielded;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        // "can't use type parameter from outer function" Why not?  And it's not a function.
        //type Walk = Walkable<Tag,Yielded>;
        // "huh?  why's there a less-than sign in here?"  Because I put it there.
        //use self::Walkable<Tag,Yielded> as Walk;

        if let Some(ref iter) = self.sub_iterator {
            if let Some(yielded) = (*iter).next() {
                return yielded; }
            else {
                // Sub-iterator is empty; clear it and fall through to the
                // base iterator.
                self.sub_iterator = None;
            } }
        if let Some(ref iter) = self.base_iterator {
            if let Some(ref element) = (*iter).next() {
                let action = <T as Walkable<Tag,Yielded>>::action(element);

                // FIXME: need to provide a way to specify order in which
                // to recurse vs. yield.  The solution should allow for
                // selection of different traversal methods -- need to come
                // up with an API for that.
                if let Some(recurse_item) = action.recurse_on {
                    self.sub_iterator = Some(Walker::new(<Yielded as Walkable<Tag,Yielded>>::iter(recurse_item)));

                    // Recurse immediately if we have no value to yield.
                    if ! action.yield_value.is_some() {
                        self.next()
                    }
                }

                // For now we'll just do depth-first, since it's simplest.
                if let Some(yielded) = action.yield_value {
                    return yielded;
                }

                // Clear the base iterator if we were asked to terminate
                // the walk.  Note that any recursive walks on
                // `self.sub_iterator` will continue until that iterator
                // is exhausted.
                if action.flags & action::TERMINATE {
                    self.base_iterator = None; }
            } else {
                // Base iterator is exhausted; clear it.
                self.base_iterator = None;
            }
        }
        return None;
    }

    /// We implement `size_hint` as the sum of the hints for the current base-
    /// and sub-iterators, if they're not `None`.  We return an upper bound
    /// only if both iterators are `None`.
    fn size_hint(&self) -> (usize, Option<usize>) {
        let mut min = 0;
        if let Some(iter) = self.sub_iterator { min += iter.size_hint().0 }
        if let Some(iter) = self.base_iterator { min += iter.size_hint().0 }

        (min, if self.sub_iterator.is_none() && self.base_iterator.is_none() { Some(0) } else { None })
    }
}
