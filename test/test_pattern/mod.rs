use std::iter::FromIterator;

use cripes::pattern::*;

mod test_iter;

/// Test the basic Element<T> values.
#[test]
fn test_element() {
    let x = ['f','(','x',')',':','Ν','→','ℜ'];
    let seq = Sequence::from_iter(x.iter());
    println!("{:?}", seq);
    let elt = Box::new(seq);
    println!("{:?}", elt);
}



#[test]
fn test_first_set() {
    let u: Union<char> = Union::from_iter(vec!['a', 'b', 'c'].into_iter());
    println!("{:?}", u);
}
