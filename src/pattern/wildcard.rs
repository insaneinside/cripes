//! Stand-alone wildcard pattern element.

use std::marker::PhantomData;

use super::{Atom, Element, Sequence, Union, Repetition, Class, Polarity};
use util::set::{self, Contains, IsSubsetOf};


/// Explicit wildcard pattern.
pub struct Wildcard<T: Atom>(PhantomData<T>);

pub type Constructor<T> = fn(PhantomData<T>) -> Wildcard<T>;

impl<T: Atom> set::IsSubsetOf<Constructor<T>> for Constructor<T> {
    #[inline(always)]
    fn is_subset_of(&self, _: &Self) -> bool {
        true
    }
}

impl<T: Atom> set::IsSubsetOf<Class<T>> for Constructor<T> {
    #[inline]
    fn is_subset_of(&self, class: &Class<T>) -> bool {
        class.is_empty() && class.polarity() == Polarity::INVERTED
    }
}

impl<T: Atom> set::IsSubsetOf<Union<T>> for Constructor<T> {
    #[inline]
    fn is_subset_of(&self, union: &Union<T>) -> bool {
        union.iter().any(|m| self.is_subset_of(m))
    }
}

impl<T: Atom> set::IsSubsetOf<Repetition<T>> for Constructor<T> {
    fn is_subset_of(&self, rep: &Repetition<T>) -> bool {
        self.is_subset_of(rep.element()) && rep.count().contains(1)
    }
}

impl<T: Atom> set::IsSubsetOf<Element<T>> for Constructor<T> {
    fn is_subset_of(&self, elt: &Element<T>) -> bool {
        match elt {
            &Element::Wildcard => self.is_subset_of(&Wildcard),
            &Element::Atom(atom) => self.is_subset_of(&atom),
            &Element::Class(ref class) => self.is_subset_of(class),
            &Element::Union(ref u) => self.is_subset_of(u),
            &Element::Sequence(ref seq) => self.is_subset_of(seq),
            &Element::Repeat(ref rep) => self.is_subset_of(rep),
            &Element::Tagged{ref element, ..} => self.is_subset_of(&**element),
            &Element::Anchor(_) => false,
        }
    }
}

macro_rules! not_subset_of {
    (<$T: ident> $Ty:ty, $($rest:ty),+) => {
        not_subset_of!(<$T> $Ty);
        not_subset_of!(<$T> $($rest),+);
    };

    (<$T: ident> $Ty:ty) => {
        impl<$T: Atom> set::IsSubsetOf<$Ty> for Constructor<T> {
            #[inline(always)]
            fn is_subset_of(&self, _: &$Ty) -> bool {
                false
            }
        }
    };
}

not_subset_of!(<T> T, Sequence<T>);
