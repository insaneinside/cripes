//! Adaptors for object-safe hashing.
//!
//! `std::hash::Hash` is not object-safe because trait function `hash` takes
//! a type parameter that specifies the concrete hash implementation used.
//!
//! This module defines a trait [`Hashable`](trait.Hashable.html), which can be
//! implemented identically to `std::hash::Hash` but accepts the hash-state
//! object as a trait object (`state: &Hasher`) and thus requires no type
//! parameters on function `hash`.
//!
//! For this reason, `Hashable` is entirely agnostic to the hash implementation being
//! used; any trait object that specifies `Hashable` as a supertrait, and
//! implements `Hash` in terms of Hashable, can be used may be safely used as
//! a (boxed) type parameter to containers like `HashSet`.
//!
//! TL;DR => if you want to hash instances of a type using dynamic dispatch
//! through a trait it implements, you must:
//!
//!
//!   1. ensure that the trait lists `Hashable` as a supertrait, and implements
//!      `std::hash::Hash` in terms of `Hashable`;
//!
//!      ```rust,ignore
//!      trait MyTrait: Hashable {
//!          // ...
//!      }
//!
//!      impl std::hash::Hash for MyTrait {
//!          fn hash<H: Hasher>(&self, state: &mut H) {
//!              <Self as Hashable>::hash(self, state);
//!          }
//!      }
//!      ```
//!
//!      and
//!
//!   2. implement `Hashable` for each concrete type that implements the trait.
//!
//!      ```rust,ignore
//!      impl Hashable for MyType {
//!          fn hash(&self, state: &mut std::hash::Hasher) {
//!              // function body should be identical to that of
//!              // an implementation of `Hash::hash`.
//!          }
//!      }
//!      ```
//!
//!
//! ## Complete Example
//!
//! ```rust
//! #![feature(hash)]
//! use std::cmp::{Eq,PartialEq};
//! use std::hash::{Hash,Hasher,SipHasher};
//! use std::collections::HashSet;
//!
//! use cripes::util::hash::{hash,Hashable};
//!
//! trait MyInterface: Hashable {}
//!
//! impl Eq for MyInterface {}
//! impl PartialEq<MyInterface> for MyInterface {
//!     fn eq(&self, other: &MyInterface) -> bool {
//!         hash::<_,SipHasher>(self) == hash::<_,SipHasher>(other)
//!     }
//! }
//!
//! impl std::hash::Hash for MyInterface {
//!     fn hash<H: Hasher>(&self, state: &mut H) {
//!         <Self as Hashable>::hash(self, state)
//!     }
//! }
//!
//!
//! #[derive(Eq,PartialEq)]
//! struct MyType(u64, &'static str);
//!
//! impl MyInterface for MyType {}
//! impl Hashable for MyType {
//!     fn hash(&self, state: &mut Hasher) {
//!         state.write_u64(self.0);
//!         state.write(self.1.as_bytes());
//!     }
//! }
//!
//! fn main() {
//!     use std::collections::HashSet;
//!
//!     let mut hs: HashSet<Box<MyInterface>> = HashSet::new();
//!     hs.insert(Box::new(MyType(123, "abc")));
//!
//!     // ...
//!
//!     for x in &hs {
//!         println!("{}", std::hash::hash::<_,SipHasher>(x));
//!     }
//! }
//!
//! ```
//!
use std::default::Default;
use std::hash::{Hash,Hasher};

/// Object-safe hash adaptor.  See the [module-level documentation](index.html)
/// for details.
pub trait Hashable {
    fn hash(&self, state: &mut Hasher);
}


/// Hash `char` as a `u32`.
impl Hashable for char {
    #[inline(always)]
    fn hash(&self, state: &mut Hasher) {
        state.write_u32(*self as u32);
    }
}

/// Hash an object by using the Hashable interface directly.
///
/// 
pub fn hash<T: ?Sized, H>(obj: &T) -> u64 where H: Hasher + Default, T: Hashable {
    let mut state: H = Default::default();
    obj.hash(&mut state);
    state.finish()
}
