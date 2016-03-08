//! Utilities not directly related to parsing, lexing, or
//! language-manipulation.

use std;

/// Panic with a given message unless an expression evaluates to true.
///
/// ## Examples
/// ```rust
/// # #[macro_use] extern crate cripes;
/// # fn main() {
/// panic_unless!(1 + 1 == 2, "Math is broken.");
/// # }
/// ```
///
/// ```rust,should_panic
/// # #[macro_use] extern crate cripes;
/// # fn main() {
/// panic_unless!(true == false, "I'm broken.");
/// # }
/// ```
#[macro_export]
macro_rules! panic_unless {
    ($condition:expr, $($rest:expr),+) => ({ if ! $condition { panic!($($rest),+); } });
}

/// Utility wrapper for `std::ptr::copy` that retains C's `memcpy`
/// argument-order semantics.
#[inline(always)]
pub unsafe fn memcpy(dest: &mut [u8], src: &[u8]) {
    std::ptr::copy(src.as_ptr(), dest.as_mut_ptr(), std::cmp::min(src.len(), dest.len()))
}


pub mod hash;
pub mod iter;
pub mod flex_box;
