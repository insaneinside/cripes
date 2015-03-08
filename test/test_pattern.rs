use cripes::pattern::*;

/// Test the `Once` iterator.
#[test]
fn test_once() {
    let mut n = 0;

    let y = "foo";
    for something in Once::new(y) { n += 1; println!("{:?}", something); }
    assert_eq!(1, n);

    let things = vec!['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'];
    n = 0;
    for something in Once::new(things) { n += 1; println!("{:?}", something); }

    n = 0;
    for something in things.iter() {
        for thing in Once::new(something) { n += 1 }
    }
    assert_eq!(n, 26);
}
