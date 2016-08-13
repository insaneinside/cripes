mod wildcard {
    use cripes::pattern::{Class, Element, Polarity, Wildcard};
    use cripes::util::set::{Contains, IsSubsetOf, IsSupersetOf};

    #[test]
    fn test_is_subset_of() {
        assert!(Element::Wildcard.is_superset_of(&Element::Atom('a')));
        assert!(!Element::Wildcard.is_subset_of(&Element::Atom('a')));
        assert!(Wildcard.is_subset_of(&Element::Wildcard));
    }
}

mod class {
    use std::char;
    use std::iter::FromIterator;

    use char_iter;
    use cripes::pattern::{Class, Element, Polarity};
    use cripes::util::set::{Contains, IsSubsetOf, IsSupersetOf};

    #[test]
    fn test_contains() {
        let cls = Class::new(Polarity::NORMAL, ['a', 'b', 'c'].into_iter().cloned());
        //assert_eq!(3, cls.len());

        // contains
        assert!(cls.contains('a'));
        assert!(cls.contains('b'));
        assert!(cls.contains('c'));
        for c in char_iter::new(0 as char, char::MAX) {
            if c == 'a' || c == 'b' || c == 'c' {
                assert!(cls.contains(c));
            } else {
                assert!(!cls.contains(c))
            }
        }
    }

    #[test]
    fn test_is_subset_of() {
        let cls1: Class<char> = FromIterator::from_iter(['x', 'y', 'z'].into_iter().cloned());
        assert!(Element::Wildcard.is_superset_of(&cls1));

        let cls2: Class<char> = FromIterator::from_iter(['x', 'y'].into_iter().cloned());
        assert!(cls2.is_subset_of(&cls1));
    }
}
