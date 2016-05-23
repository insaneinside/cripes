#![crate_type = "rlib"]
#![crate_name = "cripes"]
#![warn(missing_docs)]

#![feature(alloc)]

#![feature(core_intrinsics)]
#![feature(heap_api)]
#![feature(unsize)]
#![feature(coerce_unsized)]


extern crate dot;
extern crate num_traits;
extern crate anymap;
extern crate bit_vec;
extern crate smallvec;
extern crate regex_syntax;

#[macro_use]
pub mod util;

#[macro_use]
pub mod symbol;
pub mod pattern;
