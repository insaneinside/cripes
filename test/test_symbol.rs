use cripes::symbol::{Pool,Type,Unpacked,Inline,PackFormat};
use std::string::String;

macro_rules! inline_test_strings {
    () => ((0u8..16u8).map(|i| (0u8..i).map(|j| (j  + 'a' as u8) as char).collect::<String>()));
}


#[test]
fn inline_as_slice() {
    for s in inline_test_strings!() {
        let sym = Inline::new(s.as_slice());
        assert_eq!(sym.as_slice(), s.as_slice());
    }
}

#[test]
fn inline_packed_as_slice() {
    for s in inline_test_strings!() {
        let inl = Inline::new(s.as_slice());
        assert_eq!(inl.as_slice(), s.as_slice());
        assert_eq!(inl.pack().as_slice(), s.as_slice());
    }
}


#[test]
fn inline_pack_unpack() {
    let foo = Inline::new("foo");
    assert_eq!(foo.as_slice(), "foo");

    let bar = foo.pack();

    println!("{:?}", foo);
    println!("{:?}", bar);


    panic_unless!(foo == foo, "each symbol::Inline instance should be equal to itself");
    panic_unless!(bar == bar, "each symbol::Symbol instance should be equal to itself");

    panic_unless!(Unpacked::Inline(foo) == bar.unpack(),
                  "pack-unpack cycle on a symbol::Inline should yield the same symbol");
}



#[test]
fn pooled_pack_unpack() {
    let mut pool = Pool::new();
    let a_str = "it was a very nice day";
    let b_str = "and everyone was happy";

    let a = pool.symbol(a_str);
    let b = pool.symbol(b_str);
    assert_eq!(a.type_of(), Type::POOLED);
    assert_eq!(b.type_of(), Type::POOLED);

   println!("{:?} => {:?}", a, a.as_slice());
    println!("{:?}", a.unpack());

    assert_eq!(a, a);
    assert_ne!(a, b);
    assert_eq!(a.as_slice(), a_str);
    assert_eq!(b.as_slice(), b_str);
}
