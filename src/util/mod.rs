//! Utilities not directly related to parsing, lexing, or
//! language-manipulation.

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


pub mod hash;
pub mod flex_box;
pub mod graph;
