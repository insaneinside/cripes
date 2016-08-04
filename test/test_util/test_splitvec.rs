//! Test module for SplitVec

use cripes::util::splitvec::SplitVec;


#[test]
fn test_push_and_copy_state() {
    let mut v = SplitVec::from(vec![1, 2, 3]);
    v.push_and_copy_state();

    panic_unless_eq!(&[1, 2, 3], &*v);
    v.pop();
    panic_unless_eq!(&[1, 2], &*v);
    v.pop_state();
    panic_unless_eq!(&[1, 2, 3], &*v);
}

/// Check that `pop_and_merge_states` acts as advertised -- merging the current
/// state with the first one on the stack.
#[test]
fn test_pop_and_merge_states() {
    let mut v = SplitVec::from(vec![1, 2, 3]);

    v.push_state();
    v.extend_from_slice(&[4, 5, 6]);

    panic_unless_eq!(&[4, 5, 6], &*v);
    v.pop_and_merge_states();

    panic_unless_eq!(&[1, 2, 3, 4, 5, 6], &*v);
}


#[test]
fn test_dedup() {
    {
        let mut v = SplitVec::from(vec![1, 2, 2, 3]);
        v.dedup();
        panic_unless_eq!(&[1, 2, 3], &*v);
    }
    {
        let mut v = SplitVec::from(vec![1, 1, 2, 2, 3, 3]);
        v.dedup();
        panic_unless_eq!(&[1, 2, 3], &*v);
    }

    {
        let mut v = SplitVec::from(vec![1, 1, 2, 2, 3, 3]);

        v.push_state();

        v.dedup();
        panic_unless_eq!(&[], &*v);

        v.extend_from_slice(&[4, 5, 5, 6]);
        v.dedup();
        panic_unless_eq!(&[4, 5, 6], &*v);

        v.pop_state();

        panic_unless_eq!(&[1, 1, 2, 2, 3, 3], &*v);
        v.dedup();
        panic_unless_eq!(&[1, 2, 3], &*v);
    }
}


#[test]
fn test_fmt_debug() {
    {
        let mut v = SplitVec::from(vec![1, 2, 3]);
        panic_unless_eq!("[1, 2, 3]", format!("{:?}", v));

        v.push_state();
        panic_unless_eq!("[1, 2, 3 | ]", format!("{:?}", v));

        v.extend_from_slice(&[4, 5, 6]);
        panic_unless_eq!("[1, 2, 3 | 4, 5, 6]", format!("{:?}", v));
    }
}


#[test]
fn test_index() {
    let mut v = SplitVec::from(&[1, 2, 3]);
    panic_unless_eq!(&[1, 2, 3], &v[..]);

    v.push_state();
    panic_unless_eq!(&[], &v[..]);
}
