//! Adaptors for object-safe hashing.
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
//! ```rust
//! #![feature(hash)]
//! use std::cmp::{Eq,PartialEq};
//! use std::hash::{Hash,Hasher,SipHasher};
//! use std::collections::HashSet;
//! use cripes::util::hash::{hash,Hashable};
//!
//! trait MyInterface: Hashable {}
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
//!
//! fn main() {
//!     use std::collections::HashSet;
//!     static value_instance: MyType = MyType(123, "abc");
//!     let mut hs: HashSet<&'static MyInterface> = HashSet::new();
//!     hs.insert(&value_instance);
//! }
//!
//! ```
//!
//! `std::hash::Hash` is not object-safe because trait function `hash` takes
//! a type parameter that specifies the concrete hash implementation used.
use std::default::Default;
use std::hash::{Hash,Hasher};

/// Object-safe hash adaptor.  See the [module-level documentation](index.html)
/// for details.
pub trait Hashable {
    fn hash(&self, state: &mut Hasher);
}

/// Hash an object using the Hashable interface.
///
/// 
pub fn hash<T: ?Sized, H>(obj: &T) -> u64 where H: Hasher + Default, T: Hashable {
    let mut state: H = Default::default();
    obj.hash(&mut state);
    state.finish()
}
