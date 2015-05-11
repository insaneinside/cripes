#![crate_type = "rlib"]
#![crate_name = "cripes"]

#![feature(core)]
#![feature(hash)]
#![feature(convert)]
#![feature(unsafe_destructor)]

use std::slice::bytes;

/// Utility wrapper for `std::slice::bytes::copy_memory` that retains C's
/// `memcpy` argument-order semantics.
#[inline(always)]
fn memcpy(dest: &mut [u8], src: &[u8]) {
    bytes::copy_memory(src, dest)
}

#[macro_use]
pub mod util;

#[macro_use]
pub mod symbol;
pub mod pattern;
