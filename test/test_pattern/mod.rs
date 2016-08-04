use std::char;
use std::iter::FromIterator;

use char_iter;
use cripes::pattern::Class;

#[test]
fn test_class() {
    let cls = Class::from_iter(['a', 'b', 'c'].into_iter().cloned());
    //assert_eq!(3, cls.len());

    // contains
    assert!(cls.contains('a'));
    assert!(cls.contains('b'));
    assert!(cls.contains('c'));
    for c in char_iter::new(0 as char, char::MAX) {
        let c = c as char;
        if c == 'a' || c == 'b' || c == 'c' {
            assert!(cls.contains(c));
        } else {
            assert!(!cls.contains(c))
        }
    }
}
