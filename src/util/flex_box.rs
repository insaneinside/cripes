//! # Flexibly-sized reusable boxes
//!
//! `FlexBox` provides a reusable `Box`-like storage mechanism.  Like `Box` it
//! allows dynamic dispatch by wrapping of trait objects; however, it also
//! allows the allocated memory to be used multiple times.
//!
//! ```rust
//! use cripes::util::flex_box::{FlexBox,Ref};
//!
//! fn some_iter<'a>(dest: &'a mut FlexBox) -> Ref<'a,Iterator<Item=u8>> {
//!     dest.store(std::iter::once(42))
//! }
//!
//! fn main() {
//!     let mut flex_box = FlexBox::new();
//!     let iter = some_iter(&mut flex_box);
//!     println!("{:?}", iter.collect::<Vec<u8>>());
//! }
//! ```
use std;
use std::fmt;
use std::ptr;
use std::iter::Iterator;
use std::mem::{align_of,size_of};
use std::intrinsics::move_val_init;
use std::ops::Drop;
extern crate alloc;
use self::alloc::heap::{allocate,deallocate,reallocate};

/// A flexibly-sized, reusable `Box`-like container.
pub struct FlexBox {
    buf: *mut u8,
    capacity: usize,
    align: usize
}

impl FlexBox {
    /// Create a new FlexBox with zero capacity.
    pub fn new() -> Self {
        FlexBox{buf: ptr::null_mut(), capacity: 0, align: 0}
    }

    /// Create a new FlexBox with capacity and alignment sufficient to hold
    /// a value of the given type.
    pub fn with_capacity_for<T>() -> Self {
        let mut out = Self::new();
        if size_of::<T>() > 0 { out.ensure_capacity::<T>(); }

        out
    }

    /// Ensure that the buffer has sufficient capacity and alignment to hold
    /// a value of the specified type.
    fn ensure_capacity<T>(&mut self) {
        let size = size_of::<T>();
        let align = align_of::<T>();
        if size > 0 {
            unsafe {
                let new = if self.buf.is_null() {
                    allocate(size, align)
                } else if self.capacity < size || self.align < align {
                    reallocate(self.buf, self.capacity, size, align)
                } else {
                    ptr::null_mut()
                };

                if !new.is_null() {
                    self.buf = new;
                    self.capacity = size;
                    self.align = align;
                }
            }
        }
    }

    /// Move a value into the FlexBox, and return a reference to it.
    /// The stored value will be dropped at the same time as the returned
    /// reference is dropped.
    pub fn store<'a,T>(&'a mut self, obj: T) -> Ref<'a,T> {
        self.ensure_capacity::<T>();

        assert!(self.capacity >= size_of::<T>());
        let dest = unsafe { &mut *(self.buf as *mut T) };
        unsafe { move_val_init(dest, obj) };
        Ref(dest)
    }
}

impl Drop for FlexBox {
    fn drop(&mut self) {
        if ! self.buf.is_null() {
            unsafe { deallocate(self.buf, self.capacity, self.align); }
            self.buf = ptr::null_mut();
            self.capacity = 0;
            self.align = 0; }
    }
}

/// Reference to a live value stored in a `FlexBox`.  `Ref` directly manages
/// the lifetime of the stored value: the value is dropped at the same time as
/// the `Ref` that refers to it.
pub struct Ref<'a,T: 'a + ?Sized>(&'a mut T);

impl<'a,U: 'a + ?Sized, T: 'a + ?Sized + std::marker::Unsize<U>>
    std::ops::CoerceUnsized<Ref<'a,U>>
    for Ref<'a,T>
    {}


impl<'a,T: 'a + ?Sized> Drop for Ref<'a,T> {
    fn drop(&mut self) {
        unsafe { std::intrinsics::drop_in_place(self.0); }
    }
}

impl<'a,T: 'a + ?Sized> std::ops::Deref for Ref<'a,T> {
    type Target = T;
    fn deref<'b>(&'b self) -> &'b T {
        &*self.0
    }
}

impl<'a,T: 'a + ?Sized> std::ops::DerefMut for Ref<'a,T> {
    fn deref_mut<'b>(&'b mut self) -> &'b mut T {
        &mut *self.0
    }
}


/* **************************************************************** */
// Utility implementations follow.

// I'll admit that this macro is a bit excessive, but it's adapted from
// something similar I had lying around anyway.
macro_rules! passthrough {
    /* (&self) */
    ($implemented: path, $method:ident(&self))
        => (fn $method(&self) { T::$method(&*self) });

    /* (&self, ...) */
    ($implemented: path, $method: ident (&self, $($arg:ident: $typ:ty),+))
        => (fn $method(&self, $($arg: $typ),+) -> $ret { T::$method(&*self, $($arg),+) });

    /* (&self) -> ... */
    ($implemented: path, $method:ident(&self) -> $ret:ty)
        => (fn $method(&self) -> $ret { T::$method(&*self) });

    /* (&self, ...) -> ... */
    ($implemented: path, $method: ident (&self, $($arg:ident: $typ:ty),+) -> $ret:ty)
        => (fn $method(&self, $($arg: $typ),+) -> $ret { T::$method(&*self, $($arg),+) });


    /* (&mut self) */
    ($implemented: path, $method:ident(&mut self))
        => (fn $method(&mut self) { T::$method(&mut *self) });

    /* (&mut self, ...) */
    ($implemented: path, $method: ident (&mut self, $($arg:ident: $typ:ty),+))
        => (fn $method(&mut self, $($arg: $typ),+) -> $ret { T::$method(&mut *self, $($arg),+) });

    /* (&mut self) -> ... */
    ($implemented: path, $method:ident(&mut self) -> $ret:ty)
        => (fn $method(&mut self) -> $ret { T::$method(&mut *self) });

    /* (&mut self, ...) -> ... */
    ($implemented: path, $method: ident (&mut self, $($arg:ident: $typ:ty),+) -> $ret:ty)
        => (fn $method(&mut self, $($arg: $typ),+) -> $ret { T::$method(&mut *self, $($arg),+) });
}

impl<'a,T: 'a + ?Sized + fmt::Debug> fmt::Debug for Ref<'a,T> {
    passthrough!(fmt::Debug, fmt(&self, f: &mut fmt::Formatter) -> fmt::Result);
}

impl<'a,T: 'a + ?Sized + fmt::Display> fmt::Display for Ref<'a,T> {
    passthrough!(fmt::Display, fmt(&self, f: &mut fmt::Formatter) -> fmt::Result);
}

impl<'a,U,T: ?Sized> Iterator for Ref<'a,T> where T: 'a + Iterator<Item=U> {
    type Item = U;

    passthrough!(Iterator, next(&mut self) -> Option<U>);
    passthrough!(Iterator, size_hint(&self) -> (usize, Option<usize>));
}
