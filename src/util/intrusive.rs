///! Selected intrusive types, including intrusive references.
///! 
///! Use of intrusively-reference-counted objects allows easy creation of sharable reference
///! allows creation of references to e.g. self:
///! 
///! ```rust
///! use relemon::util::intrusive
///! struct Quaternion { w: f64, x: f64, y: f64, z: f64 }
///! impl MyType {
///! fn new_on_heap(w: f64, x: f64, y: f64, z: f64) -> Ref<Quaternion>
///! }
///! ```
///!

extern crate core;
use std;
use std::fmt;
use std::mem;
use std::ptr;
use std::ptr::PtrExt;
use std::ops::{Deref, DerefMut};
use std::hash::{Hash, Hasher};

/// Interface for types that can be used with intrusive references. 
pub trait RefCounted {
    // Create a reference to the current object.
    fn ref_to_self<T: RefCounted + ?Sized>(&self) -> Ref<T> {
        ref_to_raw(unsafe{std::mem::transmute::<_,*const T>(self)})
    }

    /// Fetch the object's current reference count. 
    fn get_refcount(&self) -> usize;

    /// Increment the object's reference count, returning the new value. 
    fn add_ref(&mut self) -> usize;
   
    /// Decrement the object's reference count, returning the new value. 
    fn remove_ref(&mut self) -> usize;
}

/// Interface required of unsized types used with intrusive references.
pub trait ExplicitlySized {
    /// Get the underlying type's allocation size.
    fn get_type_size(&self) -> usize;

    /// Get the underlying type's allocation alignment.
    fn get_type_align(&self) -> usize;
}

/// Create a simple implementation of util::intrusive::RefCounted for a type.
#[macro_export]
macro_rules! default_refcounted_impl {
    ($target:ident, $($mbr:ident).*) => (
        impl RefCounted for $target {
            fn get_refcount(&self) -> usize { self.$($mbr).* }
            fn add_ref(&mut self) -> usize { self.$($mbr).* += 1; self.$($mbr).* }
            fn remove_ref(&mut self) -> usize { self.$($mbr).* -= 1; self.$($mbr).* }
        });
    ($target:ident<'a>, $($mbr:ident).*) => (
        impl<'a> RefCounted for $target<'a> {
            fn get_refcount(&self) -> usize { self.$($mbr).* }
            fn add_ref(&mut self) -> usize { self.$($mbr).* += 1; self.$($mbr).* }
            fn remove_ref(&mut self) -> usize { self.$($mbr).* -= 1; self.$($mbr).* }
        });
    (<$typ:ident>, $($mbr:ident).*) => (
        impl<$typ> RefCounted for $typ {
            fn get_refcount(&self) -> usize { self.$($mbr).* }
            fn add_ref(&mut self) -> usize { self.$($mbr).* += 1; self.$($mbr).* }
            fn remove_ref(&mut self) -> usize { self.$($mbr).* -= 1; self.$($mbr).* }
        });
}



/// Immutable reference to an intrusively-refcounted object. 
pub struct Ref<T: RefCounted + ?Sized>(*const T);



/// Create an intrusive reference via raw pointer. 
/// 
/// This method is all *kinds* of unsafe, and should **not** be used from
/// user code!
fn ref_to_raw<T: RefCounted + ExplicitlySized>(ptr: *mut T) -> Ref<T> {
    // Transmute into a mutable reference so we can increment the refcount.
    let mut_ref: &mut T = unsafe { &mut *ptr };
    panic_unless!(1 == mut_ref.add_ref(), "unexpected nonzero refcount on unowned raw pointer");
    Ref(ptr)
}

/// Move the given value into the heap, and return a Ref to it.  The value
/// should have a refcount of zero prior to this function.
/// 
/// # Examples
/// 
/// ```rust
/// let x = ref_to_new(42);
/// println!("{:?}", x);
/// ```
pub fn ref_to_new<U: RefCounted + ExplicitlySized, T: ?Sized + RefCounted + ExplicitlySized>(obj: U) -> Ref<T> {
    let mem = unsafe { mem::transmute::<_,*mut T>(std::boxed::into_raw(Box::new(obj))) };
    ref_to_raw(mem)
}


/* **************************************************************** */
impl<T: RefCounted + ExplicitlySized + ?Sized> Ref<T> {
    /// Create a null Ref.  This is probably not very safe.
    pub fn null() -> Ref<T> {
        Ref(std::mem::transmute::<_,*const T>(0))
    }

    /// Check if the ref's internal pointer is NULL.
    pub fn is_null(&self) -> bool { match *self { Ref(x) => (x == 0 as *const T) } }

    /// Get the reference count from the underlying object.
    pub fn get_refcount(&self) -> usize {
        self.deref().get_refcount()
    }

    /// Check if this refers to the same object as another Ref. 
    pub fn is_same(&self, other: &Ref<T>) -> bool {
        match *self { Ref(x) => match *other { Ref(y) => (x as usize) == (y as usize) } }
    }
}

/* ================================================================
 * impl std::borrow::Borrow
 */
impl<T: RefCounted + ?Sized>  std::borrow::Borrow<T> for Ref<T> {
    fn borrow(&self) -> &T {
        self.deref()
    }
}

/* ================================================================
 * impl fmt::Display
 */
impl<T: RefCounted + fmt::Display + ?Sized> fmt::Display for Ref<T>  {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Ref(p) => { assert!(! (p == 0 as *const T));
                        self.deref().fmt(f) } }
    }
}

/* ================================================================
 * impl fmt::Debug
 */
impl<T: RefCounted + fmt::Debug + ?Sized> fmt::Debug for Ref<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Ref(value) => {
                write!(f, "Ref {{ 0x{:X}=>", (value as usize));
                self.deref().fmt(f) }
        }
    }
}

/* ================================================================
 * impl Clone
 */
impl<T: RefCounted + ?Sized> Clone for Ref<T> {
    fn clone(&self) -> Self {
        ref_to_raw(self.deref())
    }
}


/* ================================================================
 * impl Deref
 */
impl<T: RefCounted + ?Sized> Deref for Ref<T> {
    type Target = T;
    fn deref(&self) -> &T {
        if self.is_null() { panic!("tried to deref a null pointer") }
        match *self { Ref(p) =>  { &*p }
        }                      
    }
}

/// **FIXME:** this is NOT in ANY WAY thread-safe!
/// 
/// We really should have a separate MutRef<T: AtomicallyRefCounted> for types
/// that are reliably mutable in a thread-safe fashion.

// impl<T: RefCounted> DerefMut for Ref<T> {
//     fn deref_mut(&mut self) -> &'a mut T {
//         match *self {
//             Ref(None) => panic!("attempted to deref NULL reference"),
//             Ref(Some(x)) =>  {
//                 assert!(! x.is_null());
//                 unsafe { &mut *x }
//             }
//         }
//     }
// }


/* ================================================================
 * impl Drop
 */
#[unsafe_destructor]
impl<T: RefCounted + ExplicitlySized + ?Sized> Drop for Ref<T> {
    fn drop(&mut self) {
        match *self {
            Ref(x) => { assert!((x as usize) != 0);
                        let _ref: &mut T = unsafe { &mut *x };
                        if _ref.remove_ref() == 0 {
                            let boxed = unsafe { Box::from_raw(mem::transmute::<_,*mut T>(x)) };
                            drop(boxed); } }
        }
    }
}

/* ================================================================
 * impl PartialEq<T>, Eq<T>
 */
impl<T: Eq + RefCounted + ExplicitlySized + ?Sized> Eq for Ref<T> {}

impl<T: PartialEq<T> + RefCounted + ExplicitlySized + ?Sized> PartialEq<Ref<T>> for Ref<T> {
    fn eq(&self, other: &Ref<T>) -> bool {
        *(*self) == *(*other)
    }
}
impl<T: PartialEq<T> + RefCounted + ExplicitlySized + ?Sized> PartialEq<T> for Ref<T> {
    fn eq(&self, other: &T) -> bool {
        *self == *other
    }
}

/* ================================================================
 * impl Hash<H>
 */
impl<T: ?Sized + ExplicitlySized + RefCounted + Hash> Hash for Ref<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self { Ref(..) => self.deref().hash(state) }
    }
}
