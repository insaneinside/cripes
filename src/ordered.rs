use std::fmt;
use std::collections::VecMap;
use std::collections::vec_map::Entry;
use std::sync::atomic::{AtomicUsize,Ordering};

#[allow(non_camel_case_types)]
#[derive(Copy)]
pub enum Category
{ TOKEN = 0,
  RULE  = 1,
  STATE = 2 }

pub trait Ordered {
    fn get_order(&self) -> usize;
}

pub struct Manager {
    next_order: VecMap<AtomicUsize>
}

impl Manager {
    pub fn new() -> Self {
        Manager{next_order: VecMap::with_capacity(3)}
    }
    pub fn next_order(&mut self, c: Category) -> usize
    {
        match self.next_order.entry(c as usize) {
            Entry::Vacant(entry) => { entry.insert(AtomicUsize::new(1));
                                      0 },
            Entry::Occupied(mut entry) => { let val = entry.get_mut();
                                            val.fetch_add(1, Ordering::SeqCst) }
        }
    }
}

impl fmt::Debug for Manager {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ordered::Manager{{}}")
    }
}
