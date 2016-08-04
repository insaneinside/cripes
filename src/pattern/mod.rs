//! Structural representations of regular patterns.
//!
//! Patterns are parameterized by the _kind_ of thing &mdash; bounded by the
//! [`Atom`](trait.Atom.html) trait &mdash; those patterns describe; regular
//! expressions, for example, are usually patterns of characters.  Like their
//! real-world counterparts, `Atom`s are indivisible: they represent the
//! smallest unit that will be analyzed by any algorithm.
//!
//! Up one level from `Atom` is the [`Transition`](enum.Transition.html) enum,
//! which has variants for simple sequences and unions (classes) of atoms in
//! addition to single-atom values.  (At the time of writing, is is unclear
//! whether this type will be useful.)
//!
//!
//! 
//! This module provides two distinct ways to represent a pattern:
//!
//!   * [`Tagged<Element<T>>`](enum.Element.html) contains pattern elements
//!     as nested (and, as appropriate, boxed) fields.  This type is best for
//!     when the pattern will be modified or you need high-level access to
//!     its elements.
//!

//! A tagged-element instance can be converted to a `GraphRepr` using `.into()`.
//! 

use std::ops::{BitXor, Deref, DerefMut, Range};
use std::iter::{FromIterator,IntoIterator};
use std::fmt::{self,Display,Debug};

#[cfg(feature="regex")]
use regex_syntax::{self, Expr, Repeater};

use num_traits::{NumCast,ToPrimitive};

pub mod codegen;

/// Trait-bounds requirements for atomic values in a pattern.
pub trait Atom: Debug + Copy + Clone + Eq + Ord {}
impl<T> Atom for T where T: Debug + Copy + Clone + Eq + Ord {}


/// An action to be performed immediately before or after any pattern element
/// is consumed.
#[derive(Clone,Debug,PartialEq)]
pub enum Action {
    /// Mark the start of a captured portion of input at the current cursor
    /// location
    BeginSubmatch(String),
    /// Mark the end of a captured portion of input at the current cursor
    /// location
    EndSubmatch(String)
}


// ================================================================
// Patterns

/// Top-level pattern type
pub type Pattern<T> = Element<T>;


// ----------------------------------------------------------------
// Atomic classes

// ----------------------------------------------------------------
// Transition

/// Description of a potential transition between parser states _due to .
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum Transition<T: Atom> {
    /// atomic literal value
    Atom(T),

    /// Concatenation of multiple atoms
    ///
    /// `Literal` is the atom-level equivalent of `Sequence`, which
    /// concatenates arbitrary transitions.
    Literal(Vec<T>),

    /// Any one of a set of atoms
    ///
    /// `Class` is the atom-level equivalent of `Structure::Union`, which matches any one
    /// of a set of arbitrary transitions.
    Class(Class<T>),

    /// Any single atom.
    Wildcard,

    /// Condition that must match the current input for pattern-matching to
    /// continue
    Anchor(Anchor/*<T>*/),
}

impl<T: Atom> Transition<T> {
    /// Convert the transition to a different atom type.
    pub fn map<U, F>(self, f: F) -> Transition<U>
        where F: Fn(T) -> U,
              U: Atom
    {
        match self {
            Transition::Atom(a) => Transition::Atom(f(a)),
            Transition::Literal(v) => Transition::Literal(v.into_iter().map(f).collect()),
            Transition::Class(c) => Transition::Class(c.map(f)),
            Transition::Wildcard => Transition::Wildcard,
            Transition::Anchor(a) => Transition::Anchor(a)
        }
    }
}

impl Display for Transition<char> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Transition::Atom(ref x) => {
                if x.is_whitespace() || x.is_control() {
                    write!(f, "'{}'", x.escape_default().collect::<String>()) }
                else { write!(f, "{:?}", x) } },
            Transition::Literal(ref x) => write!(f, "'{}'", String::from_iter(x.iter().cloned())),
            Transition::Wildcard => f.write_str("(any)"),
            _ => <Self as Debug>::fmt(self, f)
        }

    }
}


impl<T: Atom> From<T> for Transition<T> {
    fn from(atom: T) -> Self {
        Transition::Atom(atom)
    }
}

impl<'a,T: Atom> From<&'a [T]> for Transition<T> {
    fn from(lit: &'a [T]) -> Self {
        Transition::Literal(lit.into())
    }
}


#[cfg(feature="regex")]
impl From<Expr> for Transition<char> {
    fn from(expr: Expr) -> Self {
        match expr {
            Expr::Empty => unimplemented!(),
            Expr::LiteralBytes{..} => unimplemented!(),
            Expr::Literal{chars, ..} => {
                if chars.len() > 1 { panic!("Literals are handled at the Element level") }
                else { Transition::Atom(chars[0]) } },
            Expr::AnyChar => Transition::Wildcard,
            Expr::AnyCharNoNL =>  Transition::Class(Class::new(Polarity::INVERTED, [ClassMember::Atom('\n')].iter().cloned())),
            Expr::AnyByte => unimplemented!(),
            Expr::AnyByteNoNL => unimplemented!(),
            Expr::Class(c) => {
                let first = c.iter().cloned().nth(0).unwrap();
                if c.len() > 1 || first.start != first.end { Transition::Class(c.into()) }
                else { Transition::Atom(first.start) } },
            Expr::ClassBytes(..) => unimplemented!(),
            Expr::StartLine => unimplemented!(),
            Expr::EndLine => unimplemented!(),
            Expr::StartText => Transition::Anchor(Anchor::StartOfInput),
            Expr::EndText => Transition::Anchor(Anchor::EndOfInput),
            Expr::WordBoundary => unimplemented!(),
            Expr::NotWordBoundary => unimplemented!(),
            Expr::WordBoundaryAscii => unimplemented!(),
            Expr::NotWordBoundaryAscii => unimplemented!(),
            Expr::Group{..} => panic!("Groups are not valid transition items"),
            Expr::Repeat{..} => panic!("Repeat expressions are handled at the Element level"),
            Expr::Alternate(_) => panic!("Alternation expressions are handled at the Element level"),
            Expr::Concat(_) => panic!("Concatenation expressions are handled at the Element level"),

        }
    }
}
// ----------------------------------------------------------------
// Class

/// Any member of a class of atoms
#[derive(Clone,PartialEq,PartialOrd, Eq, Ord)]
pub enum ClassMember<T: Atom> {
    /// A single atom
    Atom(T),

    /// A range of atoms
    Range(T, T)
}

impl<T: Atom> Copy for ClassMember<T> {}

impl<T: Atom> ClassMember<T> {
    /// Map the class-member's atoms to a different type
    pub fn map<U, F>(self, f: F) -> ClassMember<U>
        where F: Fn(T) -> U,
              U: Atom
    {
        match self {
            ClassMember::Atom(a) => ClassMember::Atom(f(a)),
            ClassMember::Range(a, b) => ClassMember::Range(f(a), f(b))
        }
    }

    /// Check if the member is or contains a particular atom.
    pub fn contains(&self, x: T) -> bool {
        match self {
            &ClassMember::Atom(a) => a == x,
            &ClassMember::Range(a, b) => (a...b).contains(x),
        }
    }

    /// Get the number of individual atoms that this class member represents.
    pub fn len(&self) -> usize
    where T: ToPrimitive {
        match self {
            &ClassMember::Atom(_) => 1,
            &ClassMember::Range(first, last) => (<usize as NumCast>::from(last).unwrap() - <usize as NumCast>::from(first).unwrap()) + 1,
        }
    }

    
}

    
impl<T: Atom> Debug for ClassMember<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ClassMember::Atom(a) => write!(f, "{:?}", a),
            ClassMember::Range(first, last) => write!(f, "{:?}-{:?}", first, last)
        }
    }
}

impl<T: Atom> From<T> for ClassMember<T> {
    fn from(a: T) -> Self { ClassMember::Atom(a) }
}

impl From<regex_syntax::ClassRange> for ClassMember<char> {
    fn from(cr: regex_syntax::ClassRange) -> Self {
        ClassMember::Range(cr.start, cr.end)
    }
}


use std::ops::Sub;
impl<T: Atom> From<Range<T>> for ClassMember<T>
    where T: Sub<usize,Output=T> + Sub<T,Output=usize>
{
    fn from(r: Range<T>) -> Self {
        if r.end - r.start > 1 { ClassMember::Range(r.start, r.end - 1usize) }
        else { ClassMember::Atom(r.start) }
    }
}

/// A set of atoms and/or ranges of atoms.
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct Class<T: Atom> {
    polarity: Polarity,
    members: Vec<ClassMember<T>>
}

impl<T: Atom> Class<T> {

    /// Create a new Class instance with specified polarity and members taken
    /// from the given iterator.
    pub fn new<I,U>(p: Polarity, items: I) -> Self
        where I: IntoIterator<Item=U>, 
              ClassMember<T>: From<U> {
        Class{polarity: p, members: Vec::from_iter(items.into_iter().map(|x| x.into()))}
    }

    /// Map the atoms in the class to a different atom type.
    pub fn map<U, F>(self, f: F) -> Class<U>
        where F: Fn(T) -> U,
              U: Atom
    {
        Class::new(self.polarity, self.members.into_iter().map(|m| m.map(&f)))
    }

    /// Check whether the class would match a particular atom.
    pub fn matches(&self, x: T) -> bool {
        self.contains(x).bitxor(self.polarity == Polarity::NORMAL)
    }

    /// Check whether a particular atom is a member of this class.
    /// Unlike `matches`, this method does not take the class's polarity
    /// into account.
    pub fn contains(&self, x: T) -> bool {
        self.members.iter().any(|&range| range.contains(x))
    }

    /// Get the number of members (atoms) in the class.
    ///
    /// Ranges of atoms count as the number of atoms in each range.
    pub fn len(&self) -> usize
        where T: ToPrimitive
    {
        self.members.iter().map(|m| m.len()).sum()
    }
}

impl<T: Atom> Debug for Class<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(if self.polarity == Polarity::NORMAL { f.write_str("[") } else { f.write_str("[^") });
        for m in self.members.iter() { try!(write!(f, "{:?}", m)) }
        f.write_str("]")
    }
}

impl<T: Atom, U> FromIterator<U> for Class<T>
where ClassMember<T>: From<U> {
    fn from_iter<I>(iter: I) -> Self
        where I: IntoIterator<Item=U> {
        Class{polarity: Polarity::NORMAL, members: Vec::from_iter(iter.into_iter().map(|x| x.into()))}
    }
}

impl From<regex_syntax::CharClass> for Class<char> {
    fn from(c: regex_syntax::CharClass) -> Self {
        Self::from(&c)
    }
}

impl<'a> From<&'a regex_syntax::CharClass> for Class<char> {
    fn from(c: &'a regex_syntax::CharClass) -> Self {
        Self::new(Polarity::NORMAL,
                  c.into_iter().map(|cr| if cr.end != cr.start { ClassMember::Range(cr.start, cr.end) }
                                    else { ClassMember::Atom(cr.start) }))
    }
}


// ----------------------------------------------------------------

/** Flag type used to differentiate between normal and inverting matches on
 * pattern elements. */
#[derive(Debug,Copy,Clone,PartialEq, PartialOrd, Eq, Ord)]
pub enum Polarity {
    /// Normal match behavior.
    NORMAL,
    /// Invert the match result.
    INVERTED
}

// ----------------------------------------------------------------
// Anchor

/// Non-consuming pattern matchers.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum Anchor/*<T: Atom>*/ {
    /// Beginning of the input buffer.
    StartOfInput,
    /// End of the input buffer.
    EndOfInput,
    /*/// Match the *FA with entry at the given node, ahead of the
    /// current position.
    LookAhead(Box<Transition<T>>),
    /// Match the *FA with entry at the given node, behind the
    /// current position.
    LookBehind(Box<Transition<T>>)*/
}

// ----------------------------------------------------------------

/// Metadata-storage wrapper for pattern elements.
#[derive(Clone,Debug)]
pub struct Tagged<T>
    where T: Clone + Debug {
    value: T,

    /// Actions that should be executed before the contained value is processed
    pub preactions: Vec<Action>,

    /// Actions that should be executed after the contained value is processed
    pub postactions: Vec<Action>
}

impl<T: Clone + Debug> Tagged<T> {
    /// Create a new Tagged instance with the given value.
    pub fn new(value: T) -> Self {
        Tagged{value: value,
               preactions: Vec::new(),
               postactions: Vec::new()}
    }

    /// Deconstruct the Tagged instance into its components
    pub fn unwrap(self) -> (T, Vec<Action>, Vec<Action>) {
        (self.value, self.preactions, self.postactions)
    }
}

impl<T: Clone + Debug> Deref for Tagged<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T: Clone + Debug> DerefMut for Tagged<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}
    
impl<T: Clone + Debug> From<T> for Tagged<T> {
    fn from(thing: T) -> Self {
        Tagged::new(thing)
    }
}

impl<'a,T: Clone + Debug> From<&'a T> for Tagged<T> {
    fn from(thing: &'a T) -> Self {
        Tagged::new(thing.clone())
    }
}


// ----------------------------------------------------------------
// Element
/// Specification of the number of times a repeated element is to be repeated
#[derive(Copy, Clone, PartialEq)]
pub enum RepeatCount {
    /// Element occurs as many times as it occurs (if it does indeed occur)
    Any,

    /// Element occurs *exactly* N times
    Exact(usize),

    /// Element occurs *at least* N times (N..)
    AtLeast(usize),

    /// Element occurs *at most* N times (..N)
    AtMost(usize),

    /// Element occurs between M and N times inclusive (M..(N + 1))
    Between(usize, usize)
}


impl From<Repeater> for RepeatCount {
    fn from(r: Repeater) -> Self {
        match r {
            Repeater::ZeroOrOne => RepeatCount::AtMost(1),
            Repeater::ZeroOrMore => RepeatCount::Any,
            Repeater::OneOrMore => RepeatCount::AtLeast(1),
            Repeater::Range{min, max} => {
                if min > 0 {
                    if let Some(max) = max {
                        if max != min { RepeatCount::Between(min as usize, max as usize) }
                        else { RepeatCount::Exact(min as usize) }
                    } else {
                        RepeatCount::AtLeast(min as usize)
                    }
                } else if let Some(max) = max {
                    RepeatCount::AtMost(max as usize)
                } else {
                    RepeatCount::Any
                }
            }
        }
    }
}



impl Debug for RepeatCount {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &RepeatCount::Any => write!(f, "any number of times"),
            &RepeatCount::Exact(n) => write!(f, "{} {}", n, if n == 1 { "time" } else { "times" }),
            &RepeatCount::AtLeast(n) => write!(f, "at least {} {}", n, if n == 1 { "time" } else { "times" }),
            &RepeatCount::AtMost(n) => write!(f, "at most {} {}", n, if n == 1 { "time" } else { "times" }),
            &RepeatCount::Between(m, n)=> write!(f, "{}-{} times", m, n)
        }
    }
}



/// High-level description of a matchable element in a pattern's structure
#[derive(Clone, Debug, PartialEq)]
pub enum Element<T: Atom> {
    /// Simple transition
    Atomic(Transition<T>),

    /// Sequence or concatenation of elements.
    ///
    /// See `Transition::Literal` for the atom-only equivalent.
    Sequence(Vec<Element<T>>),
    //Sequence(Vec<Tagged<Element<T>>>),

    /// Alternation (union) of elements.
    ///
    /// See `Transition::Class` for the atom-only equivalent.
    Union(Vec<Element<T>>),

    /// Repeated element.
    ///
    /// No provision is made for non-greedy repetition; instead we label
    /// defective those patterns for which it would be required.
    Repeat {
        /// Element being repeated
        element: Box<Element<T>>,

        /// Number of times the element should be repeated
        count: RepeatCount
    },

    /// Captured element
    Tagged {
        /// Actual element
        element: Box<Element<T>>,

        /// Name used for the capture
        name: String
    }

}

impl<T: Atom> Element<T> {
    /// Map the element to one of a different atom type.
    pub fn map<U, F>(self, f: F) -> Element<U>
        where F: Fn(T) -> U,
              U: Atom
    {
        match self {
            Element::Atomic(a) => a.map(f).into(),
            Element::Sequence(v) => Element::Sequence(v.into_iter().map(|elt| elt.map(&f)).collect()),
            Element::Union(v) => Element::Union(v.into_iter().map(|elt| elt.map(&f)).collect()),
            Element::Repeat{element, count} => Element::Repeat{element: Box::new(element.map(f)), count: count},
            Element::Tagged{element, name} => Element::Tagged{element: Box::new(element.map(f)), name: name},
        }
    }
}


impl<T: Atom> From<Transition<T>> for Element<T> {
    fn from(transition: Transition<T>) -> Self {
        Element::Atomic(transition.into())
    }
}

impl<T: Atom> From<T> for Element<T> {
    fn from(atom: T) -> Self {
        Element::Atomic(atom.into())
    }
}

#[cfg(feature="regex")]
impl From<Expr> for Element<char> {
    fn from(expr: Expr) -> Self {
        match expr {
            Expr::Literal{chars, casei} => {
                // FIXME: for case-insensitive matches, should we add some sort
                // of normalization preprocessing action?
                if casei { panic!("Case-insensitive matching is not yet supported") }
                if chars.len() > 1 { Element::Sequence(chars.into_iter().map(|c| c.into()).collect()) }
                else { Element::Atomic(Transition::Atom(chars[0])) } },
            Expr::Group{e, name, ..}
            => if let Some(name) = name { Element::Tagged{element: Box::new((*e).into()), name: name} }
               else { (*e).into() },
            Expr::Repeat{e, r, greedy} => {
                if ! greedy { panic!("Non-greedy repetition is not supported"); }
                Element::Repeat{element: Box::new((*e).into()), count: r.into()} },
            Expr::Concat(exprs) => Element::Sequence(exprs.into_iter().map(|e| e.into()).collect()),//Tagged::new(e.into())).collect()),
            Expr::Alternate(exprs) => Element::Union(exprs.into_iter().map(|e| e.into()).collect()),
            _ => Element::Atomic(expr.into())
        }
    }
}

/*
#[cfg(feature="regex")]
impl From<Expr> for Tagged<Element<char>> {
    fn from(expr: Expr) -> Self {
        match expr {
            Expr::Group{e, i, name} => {
                if i.is_some() || name.is_some() {
                    Tagged{value: (*e).into(),
                           preactions: vec![Action::BeginSubmatch(i, name.clone())],
                           postactions: vec![Action::EndSubmatch(i, name)]}
                } else {
                    Tagged{value: (*e).into(), preactions: Vec::new(), postactions: Vec::new()}
                } },
            _ => Tagged{value: expr.into(), preactions: Vec::new(), postactions: Vec::new()}
        }
    }
}*/

impl Display for Element<char> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Element::Repeat{ref element, count}
            => write!(f, "{:?} ({:?})", element, count),
            _ => <Self as Debug>::fmt(self, f)
        }
    }
}


// ----------------------------------------------------------------
// Structure

/*/// Structural representation of a pattern and its metadata.
#[derive(Clone,Debug)]
pub struct Structure<T: Atom> {
    /// Pattern element associated with the structure
    pub element: Element<T>,

    /// Actions that should be executed before the structure's element is
    /// processed
    pub preactions: Vec<Action>,

    /// Actions that should be executed after the structure's element is
    /// processed
    pub postactions: Vec<Action>
}

impl<T: Atom> Default for Structure<T> {
    fn default() -> Self {
        Structure{element: Element::Atomic(Transition::Wildcard),
                  preactions: Vec::new(), postactions: Vec::new()}
    }
}

impl<T: Atom> From<Element<T>> for Structure<T> {
    fn from(elt: Element<T>) -> Self {
        Structure{element: elt, preactions: Vec::new(), postactions: Vec::new()}//.. Default::default()}
    }
}

impl<T: Atom> From<Transition<T>> for Structure<T> {
    fn from(transition: Transition<T>) -> Self {
        Structure{element: transition.into(), preactions: Vec::new(), postactions: Vec::new()}//.. Default::default()}
    }
}

impl From<Expr> for Structure<char> {
    fn from(expr: Expr) -> Self {
        match expr {
            Expr::Group{e, i, name} => {
                if i.is_some() || name.is_some() {
                    Structure{element: (*e).into(),
                              preactions: vec![Action::BeginSubmatch(i, name.clone())],
                              postactions: vec![Action::EndSubmatch(i, name)]}
                } else {
                    Structure{element: (*e).into(), .. Default::default()}
                } },
            _ => Structure{element: expr.into(), .. Default::default()}
        }
    }
}*/


