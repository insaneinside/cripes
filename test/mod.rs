#![feature(inclusive_range_syntax)]
#![feature(try_from)]

#[cfg(feature = "regex")]
extern crate regex_syntax;

extern crate char_iter;

#[macro_use]
extern crate cripes;

mod test_util;
mod test_pattern;
