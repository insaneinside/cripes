#![crate_type = "rlib"]
#![crate_name = "cripes"]
#![warn(missing_docs)]

#![feature(alloc)]
#![feature(heap_api)]

#![feature(unsize)]
#![feature(coerce_unsized)]
#![feature(core_intrinsics)]

#![feature(range_contains)]
#![feature(inclusive_range_syntax)]
#![feature(stmt_expr_attributes)]


extern crate num_traits;
extern crate bit_vec;
extern crate bit_set;
extern crate smallvec;
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
