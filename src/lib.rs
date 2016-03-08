#![crate_type = "rlib"]
#![crate_name = "cripes"]
#![warn(missing_docs)]

#![feature(alloc)]

#![feature(core_intrinsics)]
#![feature(heap_api)]
#![feature(unsize)]
#![feature(coerce_unsized)]
#![feature(drop_in_place)]


#[macro_use]
pub mod util;

#[macro_use]
pub mod symbol;
pub mod pattern;
