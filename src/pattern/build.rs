//! Traits for builder-style pattern construction.

use super::{RepeatCount, Repetition, Sequence, Union};

/// Sequence-building trait.
pub trait Then<T> {
    /// Result of calling `then` on the implementor.
    type Output;

    /// Append the given object to the automaton under construction.
    fn then(self, t: T) -> Self::Output;
}

/// Alternative-building trait.
pub trait Or<T> {
    /// Result of calling `or` on the implementor.
    type Output;

    /// Create an automaton that matches either the current value in `self` or
    /// the given object.
    fn or(self, t: T) -> Self::Output;
}


/// Repetition-building trait.
pub trait Repeat<R>: Sized {
    /// Repeat the receiver the specified number of times.
    fn repeat(self, count: R) -> Repetition<Self>;
}



impl<T> Then<T> for T where T: Copy {
    type Output = Sequence<T>;
    fn then(self, t: T) -> Self::Output { 
        Sequence::new(vec![self, t])
    }
}

impl<T> Or<T> for T where T: Copy {
    type Output = Union<T>;
    fn or(self, t: T) -> Self::Output {
        Union::new(vec![self, t])
    }
}

impl<T, R> Repeat<R> for T
    where R: Into<RepeatCount>
{
    /// Repeat the receiver the given number of times.
    fn repeat(self, count: R) -> Repetition<Self> {
        Repetition::new(self, count.into())
    }
}

