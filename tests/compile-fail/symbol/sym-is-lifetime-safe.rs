extern crate cripes;
use cripes::symbol::{Sym, Pool};

/// Return a Sym from a temporary Pool object.  This causes a compile error
/// because the pool is dropped at the end of the function.
fn make_sym<'a>(s: &'a str) -> Sym<'a> {
    Pool::new().sym(s) //~ ERROR borrowed value does not live long enough
}

fn main() {
    let s = make_sym("he who smelt it, dealt it");
    println!("s = {}", s);
}
