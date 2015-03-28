#![crate_type = "rlib"]
#![crate_name = "cripes"]

#![feature(core)]
#![feature(hash)]
#![feature(convert)]
#![feature(unsafe_destructor)]


#[macro_use]
pub mod util;

#[macro_use]
pub mod symbol;
pub mod pattern;
