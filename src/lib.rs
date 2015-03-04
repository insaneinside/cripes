#![crate_type = "staticlib"]
#![crate_name = "cripes"]

#![feature(alloc)]
#![feature(hash)]
#![feature(unsafe_destructor)]


#[macro_use]
pub mod util;

#[macro_use]
/** Support for named symbols and symbol tables. */
pub mod symbol;
pub mod grammar;
pub mod ordered;


#[doc(hidden)]
mod cripes {
    pub use super::util;
}
