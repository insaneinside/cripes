#![feature(unstable)]
#![crate_type = "rlib"]
#![crate_name = "cripes"]

#![feature(alloc)]


/// Utility wrapper for `std::ptr::copy` that retains C's `memcpy`
/// argument-order semantics.
#[inline(always)]
fn memcpy(dest: &mut [u8], src: &[u8]) {
    unsafe { std::ptr::copy(src.as_ptr(), dest.as_mut_ptr(), std::cmp::min(src.len(), dest.len())) }
}

#[macro_use]
pub mod util;

#[macro_use]
pub mod symbol;
pub mod pattern;
