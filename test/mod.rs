#[macro_use]
extern crate cripes;

macro_rules! assert_ne {
    ($a:expr, $b:expr) => { assert!(($a != $b) && ($b != $a)) };
}

mod test_symbol;
