//! Intrusive reference types.
//!
//! Use of intrusively-reference-counted objects with matching reference types
//! over containers like `std::rc::Rc` or `std::sync::Arc` trades some
//! compile-time safety checks (and likely performance benefits) for greater
//! flexibility in how those objects are allocated and used.
//!
//! `Rc` and `Arc` are themselves references to underlying *containers*; any
//! object passed to either's `::new` will be moved to a newly-allocated block
//! on the heap.  To support weak references the data's reference count is
//! stored in another heap-allocated block, which is freed *only when the last
//! reference, weak *or* strong, is dropped*.  Because these types have
//! a reference count stored separately from the data, it is impossible to
//! create a new `Rc`, `Arc`, or `Weak` pointer thereto without one of
//! the same.
//!
//! In contrast, the types defined here forgo support for weak references and
//! strong guarantees of lifetime-management and memory-safety to allow the
//! developer more flexibility in data storage and manipulation.
//!
//! Since reference counts are stored alongside the data (in types
//! implementing [`RefCountHolder`](trait.RefCountHolder.html)),
//! a stack-allocated value can safely provide references to itself as long as
//! its refcount is non-zero prior to its doing so.  they support references
//! to stack-allocated values and values in memory not managed directly the by
//! [`Reference`](trait.Reference.html) implementation.
use std;
use std::fmt;
use std::mem;
use std::ops::{Deref,DerefMut};
use std::ptr::PtrExt;
use std::clone::Clone;
use std::hash::{Hash, Hasher};
use std::borrow::Borrow;

/// Abstract interface for references to intrusively-refcounted values.
pub trait Reference<T: ?Sized> where T: RefCounted, Self: Sized {
    /// Create a reference from a raw pointer.  The reference-type
    /// implementation should do no management of the referand's refcount for
    /// this operation.
    fn from_obj<U>(obj: &U) -> Self;

    /// Create an intrusive reference from a trait object.  The reference-type
    /// implementation should do no management of the referand's refcount for
    /// this operation.
    fn from_trait_obj(obj: &T) -> Self;


    /// Move the given value into the heap, and return a reference to it.  The value
    /// should have a refcount of zero prior to this function.
    ///
    /// # Examples
    ///
    /// ```rust
    /// #[macro_use]
    /// use cripes::util::intrusive;
    /// use cripes::util::intrusive::{Ref,Reference};
    ///
    /// #[derive(Debug)]
    /// struct Dog { refcount: usize }
    /// default_refcounted_impl!(Dog);
    ///
    /// let x: Ref<_> = Reference::new_on_heap(Dog{refcount: 0});   // x is an intrusive reference to the heap
    ///
    /// println!("{:?}", x);
    /// drop(x);                  // dropping the sole reference to that location
    ///                           // will free the memory.
    /// ```
    fn new_on_heap<U>(mut obj: U) -> Self where Self: Sized, U: Sized + RefCounted {
        {
            let rc = obj.as_refcounted_mut();
            // Zero the reference count: we're "taking ownership" of the object.
            while rc.get_refcount() != 0 { rc.remove_ref(); }

            rc.add_ref();
        }

        // Move the object onto the heap using Box, then discard the box in
        // favor of a hot, sticky, unsafe mess.
        let heaped: *const () = unsafe { std::boxed::into_raw(Box::new(obj)) as *const () };

        if std::mem::size_of::<&T>() == std::mem::size_of::<usize>() {
            // standard pointer
            unsafe { Reference::from_obj(&*(heaped as *const U)) }
        } else {
            // trait object
            unsafe { Reference::from_trait_obj(&*(heaped as *const T)) }
        }
    }
}

/// Describes a type that holds a reference count.
pub trait RefCountHolder {
    /// Fetch the object's current reference count.
    fn get_refcount(&self) -> usize;

    /// Increment the object's reference count, returning the new value.
    fn add_ref(&mut self) -> usize;

    /// Decrement the object's reference count, returning the new value.
    fn remove_ref(&mut self) -> usize;
}

pub trait RefCounted {
    // /// Create a reference to the current object.
    // fn ref_to_self<T: ?Sized>(&self) -> Ref<T> where T: RefCounted, Self: T {
    //     Reference::from_raw(unsafe { std::mem::transmute::<_,*const Self>(self) })
    // }

    fn as_refcounted(&self) -> &RefCountHolder;
    fn as_refcounted_mut(&mut self) -> &mut RefCountHolder;
}

impl<T> RefCounted for T where T: RefCountHolder {
    fn as_refcounted(&self) -> &RefCountHolder { self as &RefCountHolder }
    fn as_refcounted_mut(&mut self) -> &mut RefCountHolder { self as &mut RefCountHolder }
}

/// Create a simple implementation of util::intrusive::RefCountHolder for a type.
#[macro_export]
macro_rules! default_refcounted_impl {
    ($target:ident) => (default_refcounted_impl!($target, refcount););

    ($target:ident, $($mbr:ident).*) => (
        impl ::cripes::util::intrusive::RefCountHolder for $target {
            fn get_refcount(&self) -> usize { self.$($mbr).* }
            fn add_ref(&mut self) -> usize { self.$($mbr).* += 1; self.$($mbr).* }
            fn remove_ref(&mut self) -> usize { self.$($mbr).* -= 1; self.$($mbr).* }
        });
}


/* ****************************************************************
 * MutRef<T>
 */
// This is disabled for now -- we'd need a refcounted `RwLock` type or
// something to make this properly intrusive.  Until that happens, we
// (!!unsafely) implement MutDeref for Ref<T>.

/* /// Mutable, RwLock-protected reference to an intrusively-refcounted object.
pub struct MutRef<T: ?Sized + RefCounted + Send + Sync>(std::sync::RwLock<*mut T>);

impl<T> Reference<T> for MutRef<T>
where T: RefCounted + Send + Sync {
    fn new(ptr: *mut T) -> MutRef<T> { MutRef(std::sync::RwLock::new(ptr)) }
}

impl<T> Deref for MutRef<T>
    where T: RefCounted + Send + Sync {
    type Target = T;
    fn deref(&self) -> &T {
        if (*self).is_null() { panic!("tried to deref a null pointer") }
        match *self { Ref(p) =>  { let val_ref: &T = unsafe { &*p };
                                   val_ref }
        }
    }
}
impl<T: RefCounted> DerefMut for Ref<T> {
    fn deref_mut(&mut self) -> &mut T {
        if (*self).is_null() { panic!("tried to deref a null pointer") }
        match *self { Ref(p) =>  unsafe { std::mem::transmute(&*p) }
        }
    }
}*/


/* ****************************************************************
 * Ref<T>
 */
/// Immutable reference to an intrusively-refcounted object.
pub struct Ref<T: ?Sized>(RefImpl<T>) where T: RefCounted;

/// Private data type for Ref<T>.
enum RefImpl<T: ?Sized> where T: RefCounted {
    /// Invalid reference.
    Empty,

    /// Simple pointer to a concrete type.
    Pointer(*const T),

    /// A fat pointer/trait object.
    TraitObject(std::raw::TraitObject)
}


impl<T: ?Sized> Reference<T> for Ref<T> where T: RefCounted {
    fn from_trait_obj(obj: &T) -> Self {
        Ref(RefImpl::TraitObject(std::mem::transmute(obj)))
    }


    fn from_obj<U>(obj: &U) -> Ref<T> {
        let ptr: *const T =  unsafe { std::mem::transmute(obj) };
        if ptr == (0 as *const T) { Ref(RefImpl::Empty) }
        else { Ref(RefImpl::Pointer(ptr)) }
    }
}

// impl<T: ?Sized, U> Reference<U> for Ref<T> where T: RefCounted {
//     fn from_trait_obj(obj: &T) -> Self {
//         Ref(RefImpl::TraitObject(std::mem::transmute(obj)))
//     }


//     fn from_obj<U>(obj: &U) -> Ref<T> {
//         let ptr: *const T =  unsafe { std::mem::transmute(obj) };
//         if ptr == (0 as *const T) { Ref(RefImpl::Empty) }
//         else { Ref(RefImpl::Pointer(ptr)) }
//     }
// }


impl<T: ?Sized> Ref<T> where T: RefCounted {
    /// Fetch the reference's data pointer as a `usize`.  This is intended
    /// primarily for implementing hashes over trait objects (the standard
    /// library's Hash is not object-safe) by simply hashing the instance's
    /// data pointer.
    pub fn data_id(&self) -> usize {
        match self.0 {
            RefImpl::Empty => 0,
            RefImpl::Pointer(p) => p as usize,
            RefImpl::TraitObject(ref to) => to.data as usize
        }
    }

    /// Create a null Ref.  This is probably not very safe.
    pub fn null() -> Ref<T> {
        Ref(RefImpl::Empty)
   }

    /// Check if the ref's internal pointer is NULL.
    pub fn is_null(&self) -> bool {
        match self.0 {
            RefImpl::Empty => true,
            _ => false
        }
    }

    /// Get the reference count from the underlying object.
    pub fn get_refcount(&self) -> usize {
        match self.0 {
            RefImpl::Empty => 0,
            _ => self.deref().as_refcounted().get_refcount()
        }
    }

    /// Check if this refers to the same object as another Ref.
    pub fn is_same(&self, other: &Ref<T>) -> bool {
        match self.0 {
            RefImpl::Empty => match other.0 { RefImpl::Empty => true, _ => false },
            RefImpl::Pointer(x) => match other.0 { RefImpl::Pointer(y) => x == y, _ => false },
            RefImpl::TraitObject(ref x) => match other.0 {
                RefImpl::TraitObject(ref y) => { x.data == y.data && x.vtable == y.vtable },
                _ => false },
        }
    }
}

/* ================================================================
 * impl Deref
 */
impl<T: ?Sized> Deref for Ref<T>
where T: RefCounted {
    type Target = T;
    fn deref(&self) -> &T {
        match self.0 {
            RefImpl::Empty => panic!("tried to deref from an empty reference"),
            RefImpl::Pointer(ptr) => unsafe { &*ptr },
            RefImpl::TraitObject(ref obj) => unsafe { std::mem::transmute(obj) }
        }
    }
}

impl<T: ?Sized> DerefMut for Ref<T>
where T: RefCounted {
    fn deref_mut(&mut self) -> &mut T {
        match self.0 {
            RefImpl::Empty => panic!("tried to mutably deref from an empty reference"),
            RefImpl::Pointer(ptr) => unsafe { &mut *std::mem::transmute::<_,*mut T>(ptr) },
            RefImpl::TraitObject(ref obj) => unsafe { std::mem::transmute(obj) }
        }
    }
}

/* ================================================================
 * impl std::borrow::Borrow
 */
impl<T: ?Sized>  Borrow<T> for ::cripes::util::intrusive::Ref<T>
where T: RefCounted {
    fn borrow(&self) -> &T {
        self.deref()
    }
}

// impl<T: ?Sized>  Borrow<::cripes::util::intrusive::Ref<T>> for ::cripes::util::intrusive::Ref<T>
// where T: RefCounted {
//     fn borrow(&self) -> &T {
//         self
//     }
// }


/* ================================================================
 * impl fmt::Display
 */
impl<T: ?Sized> fmt::Display for Ref<T>
where T: RefCounted + fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            RefImpl::Pointer(p) => { assert!(! (p == 0 as *const T));
                                     let val: &T = (*self).deref();
                                     val.fmt(f) } }
    }
}

/* ================================================================
 * impl fmt::Debug
 */
impl<T: ?Sized> fmt::Debug for Ref<T>
where T:  RefCounted + fmt::Debug {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ptr = match self.0 {
            RefImpl::Empty => { return write!(f, "Ref{{Empty}}"); },
            RefImpl::Pointer(p) => p,
            RefImpl::TraitObject(ref obj) => unsafe { std::mem::transmute(obj.data) }};
        let val = (*self).deref();
        try!(write!(f, "Ref {{ 0x{:X}=>", ptr as usize));
        try!(val.fmt(f));
        write!(f, " }}")

    }
}

/* ================================================================
 * impl Clone
 */
impl<T: ?Sized> Clone for Ref<T>
where T: RefCounted {
    fn clone(&self) -> Self {
        Ref(self.0)
    }
}



/* ================================================================
 * impl Drop
 */
#[unsafe_destructor]
impl<T: ?Sized> Drop for Ref<T>
where T: RefCounted {
    fn drop(&mut self) {
        match self.0 {
            RefImpl::Pointer(x) => { assert!((x as usize) != 0);
                                     unsafe {
                                         let _ref =  self.deref_mut().as_refcounted_mut();
                                         if _ref.remove_ref() == 0 {
                                             let boxed = Box::from_raw(mem::transmute::<_,*mut T>(x));
                                             drop(boxed); } } },
            _ => {}
        }
    }
}

/* ================================================================
 * impl Eq, PartialEq<T>, PartialEq<Ref<T>>
 */
impl<T: ?Sized> Eq for Ref<T>
where T: RefCounted + Eq {}

impl<T: ?Sized> PartialEq<T> for Ref<T>
where T: RefCounted + PartialEq<T> {
    fn eq(&self, other: &T) -> bool {
        self.deref() == other
    }
}

impl<T: ?Sized> PartialEq<Ref<T>> for Ref<T>
where T: RefCounted + PartialEq<T> {
    fn eq(&self, other: &Ref<T>) -> bool {
        *self == *other
    }
}

/* ================================================================
 * impl Hash<H>
 */
impl<T: ?Sized> Hash for Ref<T>
where T: RefCounted + Hash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (*self).deref().hash(state)
    }
}
