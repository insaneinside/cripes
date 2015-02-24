use std::sync::atomic::{AtomicUsize,Ordering};

#[allow(non_camel_case_types)]
#[derive(Copy)]
enum Category
{ CATEGORY_TOKEN = 0,
  CATEGORY_RULE  = 1,
  CATEGORY_STATE = 2 }

pub struct Manager
{
    next_order: [AtomicUsize; 3]
}

impl Manager
{
    fn next_order(&self, c: Category) -> usize
    {
        self.next_order[c as usize].fetch_add(1, Ordering::SeqCst)
    }
}
