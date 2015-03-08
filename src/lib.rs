#![crate_type = "rlib"]
#![crate_name = "cripes"]

#![feature(collections)]
#![feature(core)]
#![feature(alloc)]
#![feature(hash)]
#![feature(unsafe_destructor)]


#[macro_use]
pub mod util;

#[macro_use]
pub mod symbol;
pub mod pattern;
//pub mod grammar;
pub mod ordered;

//#[doc(hidden)]
//mod cripes { pub use super::util; }
