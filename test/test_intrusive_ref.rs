use cripes::util::intrusive::*;

#[derive(Debug)]
struct Foo { refcount: usize, foo: &'static str }
default_refcounted_impl!(Foo, refcount);


trait Shower {
    fn show(&self);
}

impl Shower for Foo {
    fn show(&self) {
        println!("{}", self.foo);
    }
    fn bar(x: Ref<Foo>) {
        x.show();
    }
}

#[test]
fn ref_to_stack() {
    let x = Foo{refcount: 1, foo: "all i want for christmas is my two front teeth"};
    assert_eq!(1, x.refcount);
    assert_eq!("all i want for christmas is my two front teeth", x.foo);
    // println!("{:?} @ {:p}", x, &x);

    let y: Ref<Foo> = x.ref_to_self();
    assert!(y.foo == x.foo );
    // println!("{:?}", y);

    y.show();
    y.bar();

    let z: Ref<Shower> = x.ref_to_self();
    z.show();

    panic_unless!(x.refcount == 2, "invalid refcount on stack-allocated object with external reference");
    drop(y);

    panic_unless!(x.refcount == 1, "invalid refcount on stack-allocated object with dropped external reference");
}


#[test]
fn ref_to_new() {
    let r: Ref<_> = Reference::new_on_heap(Foo{refcount: 99, foo: "problems"});
    panic_unless!(r.refcount == 1, "invalid refcount on heap-allocated object");
    assert_eq!("problems", r.foo);
}
