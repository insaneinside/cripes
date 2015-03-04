use cripes::symbol::*;
use cripes::grammar::*;


#[test]
fn test_grammar() {
    let mut g = Grammar::new();
    let idx = g.index_mut();
    let foo = idx.token_by_name("foo");
    let FOO = idx.token_by_name("FOO");
    println!("{:?}", foo);
    println!("{:?}", FOO);
}
