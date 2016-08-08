#![crate_type = "rlib"]
#![crate_name = "cripes"]
#![warn(missing_docs)]

// These features (`alloc`, `heap_api`, `unsize`, `coerce_unsized`, and
// `core_intrinsics`) are used in the implementation of
// `util::flex_box::FlexBox`, a reusable `Box`-like container.
#![feature(alloc)]
#![feature(heap_api)]
#![feature(unsize)]
#![feature(coerce_unsized)]
#![feature(core_intrinsics)]

// `range_contains` and `inclusive_range_syntax` are required in the `pattern`
// module, where we use them for checking whether particular atoms are members
// of a range in an atom class.
#![feature(range_contains)]
#![feature(inclusive_range_syntax)]

// `stmt_expr_attributes` is used to enable debug output crate-wide using crate
// feature "debug".
#![feature(stmt_expr_attributes)]


extern crate num_traits;
extern crate bit_set;
extern crate smallvec;
extern crate arrayvec;
extern crate itertools;

#[cfg(feature="regex")]
extern crate regex_syntax;

#[macro_use]
extern crate bitflags;

#[macro_use]
pub mod util;

#[macro_use]
pub mod symbol;
pub mod pattern;
pub mod automaton;
