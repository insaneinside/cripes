//! Test module for `cripes::util::flex_box`, which provides flexibly-sized
//! reusable boxes.

use cripes::util::flex_box::*;

#[test]
/// Test that FlexBox works for simple value types.
fn test_simple_value() {
    let mut b = FlexBox::new();

    {
        let x = b.store(3u8);
        println!("{:?}", x);
    }
    {
        let x = b.store(vec![0x55usize;32]);
        println!("{:?}", x);        
    }

    {
        let y = b.store("foo");
        println!("{:?}", y);
    }
}


#[test]
/// Test that FlexBox works for values with Drop implementations.
fn test_with_drop() {
    use std::rc::Rc;
    use std::ops::Drop;
    use std::mem::drop;
    use std::cell::RefCell;
    use std::sync::atomic::{AtomicUsize,Ordering};

    let ran_drop = Rc::new(RefCell::new(AtomicUsize::new(0)));

    struct HasDrop(Rc<RefCell<AtomicUsize>>);
    impl Drop for HasDrop {
        fn drop(&mut self) {
            println!("Goodbye, World!");
            self.0.borrow_mut().fetch_add(1, Ordering::SeqCst);
        }
    }

    let mut b = FlexBox::new();
    {
        let x = b.store(HasDrop(ran_drop.clone()));
        assert!( 0 == ran_drop.borrow().load(Ordering::SeqCst) );
        drop(x);
        assert!( 1 == ran_drop.borrow().load(Ordering::SeqCst) );
    }

    ran_drop.borrow().store(0, Ordering::SeqCst);
    { 
        let x: Ref<Drop> = b.store(HasDrop(ran_drop.clone()));
        assert!( 0 == ran_drop.borrow().load(Ordering::SeqCst) );
        drop(x);
        assert!( 1 == ran_drop.borrow().load(Ordering::SeqCst) );

    }
}


#[test]
/// Ensure that trait objects work.
fn test_with_trait_object() {
    use std;
    let mut b = FlexBox::new();

    {
        let y: Ref<std::fmt::Debug> = b.store("eeny meeny meiny moe");
        println!("{:?}", y);
    }
}


#[test]
/// Check that the iterator implementations on flex_box::Ref work as expected.
/// This is mostly a compile-time check.
fn test_with_iterator() {
    let mut b = FlexBox::new();

    {
        let u = vec![1, 2, 3, 4];
        let it: Ref<Iterator<Item=_>> = b.store(u.iter());
         for x in it {
            println!("{:?} ", x);
        }
    }
}


#[test]
/// Check that we can store items in a FlexBox indirectly.  This is mostly
/// a compile-time check.
fn test_store_indirect() {
    let mut b = FlexBox::new();

    for x in store_and_return_iterator::<()>(&mut b) {
        println!("{:?}", x);
    }
}

fn store_and_return_iterator<'a,I: 'a>(b: &'a mut FlexBox) -> Ref<'a,Iterator<Item=I>> {
    use cripes::util::iter;
    b.store(iter::Empty::new())
}


#[test]
/// Check that we can implement traits that use FlexBox for storage,
/// potentially via chained calls to subobjects.  This is mostly
/// a compile-time check.
fn test_trait_store_iterator_chained() {
    use cripes::util::iter;

    trait StoresIterator<T> {
        fn give_me_an_iterator<'b>(&self, b: &'b mut FlexBox) -> Ref<'b, Iterator<Item=T>>
            where T: 'b;
    }

    struct LikesIterators<T: Copy> {
        // ...but doesn't own them directly.
        holder: HasIterator<T>
    }
    impl<T: Copy> StoresIterator<T> for LikesIterators<T> {
        fn give_me_an_iterator<'b>(&self, b: &'b mut FlexBox) -> Ref<'b,Iterator<Item=T>>
        where T: 'b {
            self.holder.give_me_an_iterator(b)
        }
    }

    struct HasIterator<T: Copy> { value: T }

    impl<T: Copy> StoresIterator<T> for HasIterator<T> {
        fn give_me_an_iterator<'b>(&self, b: &'b mut FlexBox) -> Ref<'b,Iterator<Item=T>>
        where T: 'b {
            b.store(iter::Once::new(self.value))
        }
    }


    let mut b = FlexBox::new();
    let hi = HasIterator{value: 2};
    let likes_iterators = LikesIterators{holder: HasIterator{value: 2}};

    for u in hi.give_me_an_iterator(&mut b) {
        println!("{}", u);
    }

    for v in likes_iterators.give_me_an_iterator(&mut b) {
        println!("{}", v);
    }
}    
