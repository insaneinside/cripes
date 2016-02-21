//! Parsing routines for patterns.

use std;
use std::fmt::Debug;
use std::hash::Hash;
use std::boxed::Box;
use std::cmp::{Eq,PartialEq};
use std::collections::HashSet;
use std::iter::{Peekable,FromIterator};

use super::*;
use util::hash::Hashable;

/// Parse interface for concrete pattern types.
///
/// Note that this is *not* object-safe!
trait Parse<T> {
    fn parse(input: &str) -> Self;
}

#[derive(Copy,Clone,Eq,PartialEq)]
#[repr(u8)]
enum PatternType {
    EMPTY,
    SEQUENCE,
    UNION
}

/// Precedence for patterns.  Higher values indicate a higher priority here.
const PATTERN_PRIORITY: [u8; 3]
    = [ 0xFF, 0xFE, 0xFD ];


/// Part of a pattern held in a Builder instance.
#[derive(Clone,Eq,PartialEq)]
enum Partial<T> where T: 'static + Copy + Debug + Hash + Eq + PartialEq<T>{
    Empty,
    Element(Element<T>),
    Builder(Builder<T>)
}

impl<T> Partial<T> where T: 'static + Copy + Debug + Eq + PartialEq + Hash + Hashable {
    pub fn finish(self) -> Element<T> {
        match self {
            Partial::Empty => Element::Empty,
            Partial::Element(elt) => elt,
            Partial::Builder(builder) => builder.finish()
        }
    }
}


/// Concrete syntax tree builder.
#[derive(Clone,Eq,PartialEq)]
struct Builder<T> where T: 'static + Copy + Debug + Hash + Eq + PartialEq<T> {
    pattern_type: PatternType,
    parts: Vec<Partial<T>>,
    push_index: Option<usize>
}
    
impl<T> Builder<T> where T: Copy + Debug + Hash + Eq + PartialEq<T> {
    pub fn new() -> Builder<T> {
        Builder{pattern_type: PatternType::EMPTY,
                parts: Vec::new(),
                push_index: None}
    }

    pub fn ensure_type(&self, ptype: PatternType) -> Self {
        if let Some(idx) = self.push_index {
            match self.parts[idx] {
                // Cannot set type of a completed pattern!  Clear the push
                // index and fall through.
                Partial::Element(_) => self.push_index = None,
                Partial::Builder(ref b) => { b.ensure_type(ptype);
                                             return *self; }
            }
        }
                                      
        if self.pattern_type == ptype {
            *self
        } else if self.pattern_type == PatternType::EMPTY {
            self.pattern_type = ptype;
            *self
        } else {
            // Create a new builder object.
            let mut o = Builder::new();
            o.pattern_type = ptype;

            if PATTERN_PRIORITY[ptype as usize] > PATTERN_PRIORITY[self.pattern_type as usize] {
                // Push the current builder in as a component of the new one.
                o.push(Partial::Builder(*self));
                o
            } else {
                // Append the new builder to the current one, and set the push index to the new index.
                self.push(Partial::Builder(o));
                self.push_index = Some(self.parts.len() - 1);
                *self
            }
        }
    }

    pub fn push(&self, part: Partial<T>) {
        // If we're adding an element to a non-empty builder marked as empty,
        // it's clearly *not* empty -- it's probably a sequence.
        if ! self.parts.is_empty() && self.pattern_type == PatternType::EMPTY {
            self.pattern_type = PatternType::SEQUENCE;
            self.push_index = None;
        }

        match self.push_index {
            Some(idx) =>
            { match self.parts[idx] {
                // Can't push to a completed element.
                Partial::Element(..)
                    => { self.push_index = None;
                         self.parts.push(part); },
                Partial::Builder(ref b)
                    => b.push(part) } },
            None => self.parts.push(part)
        }
    }

    /// Evaluate the builder's contents and produce a pattern.
    pub fn finish(self) -> Element<T> where T: 'static + Copy + Debug + Eq + PartialEq + Hash + Hashable {
        match self.pattern_type.clone() {
            PatternType::EMPTY => {
                
                match self.parts.len() {
                    0 => Element::Empty,
                    1 => self.parts[0].finish(),
                    _ => panic!("Pattern type MUST be set for non-unit-length Builder instance!")
                } },
            PatternType::SEQUENCE => {
                Element::Pattern(Box::new(Sequence::new(self.parts.map_in_place(move |p| p.finish())))) }
            PatternType::UNION =>
                // Let's figure out these other errors later.
                unimplemented!() // {
            //     let mut s = HashSet::<Element<T>>::new();
            //     for p in self.parts { s.insert(p.finish()) }
            //     Element::Pattern(Box::new(Union::new(s)))
            // }
            
        }
    }
        
}

pub struct Parser<'a> {
    input: &'a str,
    iter: Peekable<std::str::Chars<'a>>
}


impl<'a> Parser<'a> {
    pub fn parse(input: &'a str) -> Element<char> {
        Parser::new(input).run()
    }

    pub fn new(input: &'a str) -> Self {
        Parser{input: input, iter: input.chars().peekable()}
    }

    fn next_char(&self) -> char {
        if self.iter.is_empty() { panic!("`next_char()` called with empty buffer") }
        else { self.iter.next().unwrap() }
    }

    pub fn run(&self) -> Element<char> {
        let escaped = false;
        let mut b = Builder::<char>::new();
        let len = self.input.len();

        for c in self.input.chars() {
            if escaped { b.push(Partial::Element(Element::Atom(c))); }
            else {
                match c {
                    '\\' => escaped = true,
                    '|' => b = b.ensure_type(PatternType::UNION),
                    '(' => { let mut s = String::with_capacity(len);
                             
                    },
                    _ => {b.push(Partial::Element(Element::Atom(c)));}
                }
            }
        }
        b.finish()
    }
}

/*impl<'a> Parse<&'a str> for Sequence<char> {
    fn parse(input: &str) -> Sequence<char> {
        Sequence::from_iter(input.chars())
    }
}*/
