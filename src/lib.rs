//! Cripes: Grammar Manipulation Toolkit
// Copyright (C) 2015-2016 Collin J. Sutton
#![crate_type = "rlib"]
#![crate_name = "cripes"]
#![warn(missing_docs)]

// `unicode` is used for encoding `char`s to UTF-8 in `pattern/codegen.rs`.
#![feature(unicode)]

// These features (`alloc`, `heap_api`, `unsize`, `coerce_unsized`, and
// `core_intrinsics`) are used in the implementation of
// `util::flex_box::FlexBox`, a reusable `Box`-like container.
#![feature(alloc)]
#![feature(heap_api)]
#![feature(unsize)]
#![feature(coerce_unsized)]
#![feature(core_intrinsics)]

// `range_contains`, `inclusive_range`, and `inclusive_range_syntax` are
// required in the `pattern` module, where we use them for checking whether
// particular atoms are members of a range in an atom class.
// `pattern::RepeatCount` implements `From<RangeToInclusive<usize>>`.
#![feature(range_contains)]
#![feature(inclusive_range_syntax)]
#![feature(inclusive_range)]

// `conservative_impl_trait` is used to provide iterators from various types
// without resorting to unwieldy workarounds or viral proliferation of
// lifetime parameters.
#![feature(conservative_impl_trait)]

// `stmt_expr_attributes` is used to enable debug output crate-wide using crate
// feature "debug".
#![feature(stmt_expr_attributes)]

#[macro_use] extern crate cripes_macros;

extern crate num_traits;
extern crate bit_set;
extern crate smallvec;
extern crate arrayvec;
extern crate itertools;
#[macro_use] extern crate bitflags;

pub mod util;
pub mod pattern;
pub mod automaton;
//pub mod grammar;
