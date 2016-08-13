mod wildcard {
    use std::{char, u8};
    use std::iter::FromIterator;
    use char_iter;
    use cripes::pattern::{ByteOrChar, Class, Element, Wildcard};
    use cripes::util::set::{Contains, IsSubsetOf, IsSupersetOf};

    #[test]
    fn test_is_subset_of() {
        let wild = Wildcard::<char>;
        assert!(wild.is_superset_of(&'a'));
        assert!(wild.is_superset_of(&Element::Atom('a')));
        assert!(!wild.is_subset_of(&Element::Atom('a')));
        assert!(wild.is_subset_of(&Wildcard));

        assert!(ByteOrChar::Byte(b'H').is_subset_of(&Wildcard));

        let cls = Class::from_iter("xyz".chars());
        assert!(wild.is_superset_of(&cls));
    }

    #[test]
    fn test_contains() {
        let wildc = Wildcard::<char>;
        for c in char_iter::new(0 as char, char::MAX) {
            assert!(wildc.contains(c));
        }

        let wildbc = Wildcard::<ByteOrChar>;
        for c in char_iter::new(0 as char, char::MAX) {
            assert!(wildbc.contains(c.into()));
        }
        for b in 0...u8::MAX {
            assert!(wildbc.contains(b.into()));
        }
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
