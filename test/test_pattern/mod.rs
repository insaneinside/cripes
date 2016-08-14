mod sequence {
    use std::iter::FromIterator;
    use cripes::pattern::{Class, Element, Sequence, Union};
    use cripes::util::set::IsSubsetOf;

    #[test]
    fn test_is_subset_of() {
        let unions = (&["abc", "def", "ghi"]).iter().map(|s| Union::<char>::from_iter(s.chars().map(|c| c.into()))).collect::<Vec<_>>();
        let classes = (&["abc", "def", "ghi"]).iter().map(|s| Class::<char>::from_iter(s.chars())).collect::<Vec<_>>();
        let seqs = (&["adg", "beh", "cfi"]).iter().map(|s| Sequence::<char>::from_iter(s.chars().map(|c| c.into()))).collect::<Vec<_>>();
        let useq = Sequence::from_iter(unions.iter().map(|u| Element::Union(u.clone())));
        let cseq = Sequence::from_iter(classes.iter().map(|c| Element::Class(c.clone())));
        assert!(useq.is_subset_of(&cseq));
        assert!(cseq.is_subset_of(&useq));

        for seq in seqs.iter() {
            assert!(seq.is_subset_of(&cseq));
            assert!(seq.is_subset_of(&useq));
        }
    }
}

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
    use cripes::pattern::{Class, ClassMember, Element, Polarity, Wildcard};
    use cripes::util::set::{Contains, IsSubsetOf, IsSupersetOf};

    #[test]
    fn test_contains() {
        let range = ClassMember::Range('a', 'z');
        panic_unless_eq!(26, range.len());
        assert!(range.contains('a'));
        assert!(range.contains('z'));
        assert!(! range.contains('A'));
        assert!(! range.contains('Z'));

        let atom = ClassMember::Atom('X');
        panic_unless_eq!(1, atom.len());
        assert!(atom.contains('X'));
        assert!(! atom.contains('x'));

        let class_from_members = Class::from_members([range, atom].into_iter().cloned());
        assert!(class_from_members.contains(&range));
        assert!(class_from_members.contains(&atom));
        panic_unless_eq!(27, class_from_members.len());
        for c in char_iter::new('a', 'z') {
            assert!(class_from_members.contains(c));
        }
        for c in char_iter::new('A', 'W') {
            assert!(! class_from_members.contains(c));
        }
        assert!(class_from_members.contains('X'));
        for c in char_iter::new('Y', 'Z') {
            assert!(! class_from_members.contains(c));
        }




        let cls = Class::from_iter(['a', 'b', 'c'].into_iter().cloned());
        assert_eq!(3, cls.len());

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
        assert!(Wildcard.is_superset_of(&cls1));

        let cls2: Class<char> = FromIterator::from_iter(['x', 'y'].into_iter().cloned());
        assert!(cls2.is_subset_of(&cls1));
    }
}
