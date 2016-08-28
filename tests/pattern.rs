use std::fmt::Debug;
use std::convert::TryFrom;
use regex_syntax;
use cripes::pattern::{Atom, Element};

fn from_regex<T: Atom>(s: &str) -> Element<T>
    where Element<T>: TryFrom<regex_syntax::Expr>,
          <Element<T> as TryFrom<regex_syntax::Expr>>::Err: Debug
{
    TryFrom::try_from(regex_syntax::Expr::parse(s).unwrap()).unwrap()
}

macro_rules! extract_variant {
    ((ref $input: expr) -> $variant: ident) => ( match $input {
        &Element::$variant(ref x) => x,
        _ => unreachable!() });

    (($input: expr) -> $variant: ident) => ( match $input {
        Element::$variant(x) => x,
        _ => unreachable!() });
}

/// Helper for parsing a regex and returning the inner pattern type.
macro_rules! from_regex {
    ($s: tt -> Element<$T: ty>) => ( from_regex::<$T>($s) );
    ($s: tt -> $which: ident < $T: ty>) => (extract_variant!((from_regex::<$T>($s)) -> $which));
}

mod sequence {
    use std::iter::{self, FromIterator};
    use cripes::pattern::{Class, Element, Sequence, Union};
    use cripes::pattern::Reduce;
    use cripes::util::set::IsSubsetOf;

    #[test]
    fn test_reduce() {
        let seq_of_seqs = Sequence::from_iter(["abc", "def", "ghi"].iter().map(|s| Sequence::from_iter(s.chars().map(|c| c.into())).into()));
        let flat_seq = Sequence::<char>::from_iter("abcdefghi".chars().map(|c| c.into()));

        assert!(seq_of_seqs != flat_seq);
        assert_eq!(Some(Element::Sequence(flat_seq)), seq_of_seqs.reduce());

        // Empty sequences should reduce to `None`
        assert_eq!(None, Sequence::<char>::from_iter(iter::empty()).reduce());

        // Unit-length sequences should reduce to their only element.
        assert_eq!(Some(Element::Wildcard), Sequence::<char>::from_iter(vec![Element::Wildcard]).reduce());
    }

    #[test]
    fn test_is_subset_of() {
        let unions = (&["abc", "def", "ghi"]).iter().map(|s| Union::<char>::from_iter(s.chars().map(|c| c.into()))).collect::<Vec<_>>();
        let classes = (&["abc", "def", "ghi"]).iter().map(|s| Class::<char>::from_iter(s.chars())).collect::<Vec<_>>();
        let seqs = (&["adg", "beh", "cfi"]).iter().map(|s| Sequence::<char>::from_iter(s.chars().map(|c| c.into()))).collect::<Vec<_>>();
        let useq = Sequence::from_iter(unions.iter().map(|u| Element::Union(u.clone())));
        let cseq = Sequence::from_iter(classes.iter().map(|c| Element::Class(c.clone())));

        // Every sequence should be a subset of itself.
        assert!(useq.is_subset_of(&useq));
        assert!(cseq.is_subset_of(&cseq));

        // `useq` and `cseq` should be subsets of each other.
        assert!(useq.is_subset_of(&cseq));
        assert!(cseq.is_subset_of(&useq));

        // each sequence in `seqs` should be a subset of both `useq` and
        // `cseq`.
        for seq in seqs.iter() {
            for i in 0..3 {
                assert!(seq[i].is_subset_of(&cseq[i]));
                assert!(seq[i].is_subset_of(&useq[i]));
            }

            assert!(seq.is_subset_of(&cseq));
            assert!(seq.is_subset_of(&useq));
        }
    }
}

mod union {
    use super::from_regex;
    use std::iter::FromIterator;
    use cripes::pattern::{Element, Union, Wildcard};
    use cripes::util::set::{Contains, IsSubsetOf};

    #[test]
    fn test_contains() {
        let u = Union::from_iter("abc".chars().map(|c| c.into()));
        assert!(u.contains('a'));
        assert!(u.contains('b'));
        assert!(u.contains('c'));
    }

    #[test]
    fn test_is_subset_of_union() {
        let u1 = Union::<char>::from_iter("abc".chars().map(|c| c.into()));
        let u2 = Union::from_iter("abcdef".chars().map(|c| c.into()));
        assert!(u1.is_subset_of(&u2));
        assert!(! u2.is_subset_of(&u1));

        assert!(u1.is_subset_of(&Wildcard));
        assert!(u2.is_subset_of(&Wildcard));
    }
    #[test]
    fn test_is_subset_of_sequence() {
        let seq_of_classes = from_regex!("[abc][def][ghi]" -> Sequence<char>);
        let seq_of_unions = from_regex!("(a|b|c)(d|e|f)(g|h|i)" -> Sequence<char>);

        let union_of_seqs = from_regex!("adg|beh|cfi" -> Union<char>);
        println!("{:?}", union_of_seqs);
        assert_eq!(3, union_of_seqs.len());

        // check that the union would match each contained sequence.
        for seq in union_of_seqs.iter() {
            println!("{:?}", seq);
            assert!(seq.is_subset_of(&union_of_seqs));

            for i in 0..3 {
                let s = extract_variant!((ref seq) -> Sequence);
                println!("{:?} <=> {:?}", s[i], seq_of_unions[i]);
                assert!(s[i].is_subset_of(&seq_of_unions[i]));
                assert!(s[i].is_subset_of(&seq_of_classes[i]));
            }

            assert!(seq.is_subset_of(&seq_of_classes));
            assert!(seq.is_subset_of(&seq_of_unions));
        }

        assert!(union_of_seqs.is_subset_of(&seq_of_classes));
        assert!(union_of_seqs.is_subset_of(&seq_of_unions));

        assert!(! seq_of_classes.is_subset_of(&union_of_seqs));
        assert!(! seq_of_unions.is_subset_of(&union_of_seqs));

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
    use cripes::pattern::{Class, ClassMember, Wildcard};
    use cripes::util::set::{Contains, IsSubsetOf};

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
        assert!(cls1.is_subset_of(&Wildcard));

        let cls2: Class<char> = FromIterator::from_iter(['x', 'y'].into_iter().cloned());
        assert!(cls2.is_subset_of(&cls1));
        assert!(! cls1.is_subset_of(&cls2));
    }
}
