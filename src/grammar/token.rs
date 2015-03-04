//! Token traits, variants, and related data structures.
use std;
use std::fmt;
use std::any::Any;
use std::raw::TraitObject;
//use std::ops::Range;
use std::default::Default;
use std::hash::{hash,Hash, Hasher,SipHasher};
use std::collections::{LinkedList,HashSet};

use symbol;
use ordered;
use symbol::{PackFormat,Symbol};
use grammar::Rule;
use grammar::TokenIndex;
use util::intrusive;
use util::intrusive::{Ref,Reference,RefCountHolder};

/// Data used to quickly find information related to a particular token.
#[derive(Debug)]
pub struct IndexData {
    /// Rules in which the token appears on the left-hand side.  These rules
    /// define the token.
    // FIXME: do we need this?  It may make more sense to store rules in their
    //     parent LHS nonterminal or synthetic, since terminals (and
    //     potentially other token types we dream up) don't have rules.
    pub rules_for: Option<Box<LinkedList<Rule>>>,

    /// (rule,vec(position)) pairs for rules that contain the token on their
    /// right-hand sides.  Rules in this list are used-by relations.
    pub rules_containing: LinkedList<(Ref<Rule>,Vec<usize>)>
}

impl Default for IndexData {
    fn default() -> IndexData {
        IndexData{rules_for: None,
                  rules_containing: LinkedList::new()}
    }
}

/// Trait providing access to a token's index data.
pub trait Indexed {
    fn index_data(&self) -> &IndexData;
}


/* ****************************************************************
 * Tokens, and their Exquisite Varieties
 */
/// Data-type used solely to identify the underlying concrete token type.
pub enum Type {
    /// [Terminal](struct.Terminal.html) token.
    Terminal,
    /// [Nonterminal](struct.Nonterminal.html) token.
    Nonterminal,
    /// [Synthetic](struct.Synthetic.html) token.
    Synthetic,

    /// [Labeled](struct.LabeledToken.html) token.
    Labeled
}

static TOKEN_SCOPE_SEPARATOR: char = ':';

pub trait Typed {
    fn token_type(&self) -> Type;
}


/// Provides methods to calculate a token's first set.
///
/// A compound token's first set is defined as the union of the first sets for
/// all rules with that token on the left-hand side; a terminal token is its
/// own first set.
///
/// A rule's first set is the union of first sets for leading tokens on its
/// right-hand side, up to and including the first
/// non-[nullable](trait.Nullable.html) token.
pub trait HasFirstSet {
    /// Find the token's first set, and store it into the given vector.
    fn first_set_into(&self, out: &mut Vec<Ref<Token>>);

    /// Calculate and return the token's first set.
    fn first_set(&self) -> Vec<Ref<Token>> {
        let mut out = Vec::new();
        self.first_set_into(&mut out);
        out
    }
}
        
/// Provides a method to determine whether a token is "nullable", i.e., has any
/// rules that may reduce to an empty sequence.
pub trait Nullable {
    /// Check if the token is nullable.
    fn is_nullable(&self) -> bool { false }
}

/// A token that may be repeated as a Synthetic.
pub trait Repeatable: intrusive::RefCounted {
    /// Fetch the repetition range for this token.  
    fn get_repetitions(&self) -> std::ops::Range<usize> { 1..2 } /* default 1 */
}


/* ****************************************************************
 * Token
 */
/// Abstract interface to all Token variants.
pub trait Token:
    fmt::Debug + 
    ordered::Ordered +
    symbol::Nameable +
    intrusive::RefCounted +
    Indexed +
    HasFirstSet +
    Repeatable +
    Nullable {
    /// Get the underlying token's type.
    fn token_type(&self) -> Type;
}


impl Eq for Token {}

/// We Consider two tokens to be identical if they live at the same address.
/// This is a complete hack, but will have to do since neither Hash nor
/// PartialEq are object-safe.
impl PartialEq<Token> for Token {
    fn eq(&self, other: &Token) -> bool {
        use std::mem::transmute;
        unsafe { transmute::<_,TraitObject>(self).data ==
                 transmute::<_,TraitObject>(other).data }
    }
}


/* ****************************************************************
 * Spec
 */
#[derive(Copy,Debug)]
pub struct Range<T>{start: T, end: T}
impl<T> Range<T> {
    pub fn from_range(r: std::ops::Range<T>) -> Range<T> {
        Range{start: r.start, end: r.end}
    }

    pub fn as_range(&self) -> std::ops::Range<T> where T: Copy {
        std::ops::Range{start: self.start, end: self.end}
    }
}

impl<T> Eq for Range<T> where T: Eq {}
impl<T> PartialEq<Range<T>> for Range<T> where T: Eq {
    fn eq(&self, other: &Range<T>) -> bool {
        self.start == other.start && self.end == other.end
    }
}

/* ****************************************************************
 * Spec
 */
/// Summary of a token's parameters relevant to its use within a grammar.
/// Spec is used to find and create tokens in
/// a [TokenIndex](struct.TokenIndex.html), which also uses it internally as
/// the key type in mappings to Tokens.
#[derive(Copy)]
pub struct Spec {
    pub name: Symbol,

    /// Number of occurrences the token should match.  Specifying this value as
    /// anything other than `1..2` (one repetition) will create
    /// a Synthetic token.
    pub repetitions: Range<usize>,

    /// Label for the token.  If not specified, the token will be an
    /// unlabeled/standard token.
    pub label: Option<Symbol>,

    // /// Reference to base token to use for synthetic and labeled specs.
    // /// If this is not specified, one will be automatically selected
    // /// and/or created.
    // pub base: Option<Ref<Token>>
    // parent_scope: Ref<Namespace>
}

impl Spec {
    /// Determine whether the spec calls for a synthetic (repeated) token.
    pub fn is_synthetic(&self) -> bool {
        self.repetitions.as_range() != (1..2)
    }

    /// Determine whether the spec calls for a labeled token instance.
    pub fn is_labeled(&self) -> bool {
        match self.label {
            Some(..) => true,
            _ => false }
    }


    /// Determine whether the spec implies the existence of some base token.
    pub fn requires_base(&self) -> bool {
        self.is_synthetic() || self.is_labeled()
    }

    /// Create a copy of this Spec with the topmost modifier (label or
    /// repetitions) removed.
    pub fn base_spec(&self) -> Spec {
        if self.is_labeled()
        { Spec{name: self.name,
               repetitions: self.repetitions,
               ..Default::default()} }
        else if self.is_synthetic()
        { Spec{name: self.name,
               ..Default::default()} }
        else { *self }
    }
}

impl Default for Spec {
    fn default() -> Spec {
        Spec{name: symbol::Inline::new("").pack(),
             repetitions: Range::from_range(1..2),
             label: None}
    }
}

impl Eq for Spec {}

impl PartialEq<Spec> for Spec {
    fn eq(&self, other: &Spec) -> bool {
        self.name == other.name &&
            self.repetitions == other.repetitions &&
            self.label == other.label
    }
}

impl Hash for Spec {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.repetitions.start.hash(state);
        self.repetitions.end.hash(state);
        self.label.hash(state);
    }
}

/* ****************************************************************
 * BaseToken -- common base for concrete token variants.
 */

/// Common data shared by all concrete token types. 
#[derive(Debug)]
pub struct BaseToken {
    pub name: Symbol,
    pub order: usize,
    pub index: IndexData
}

impl BaseToken {
    pub fn new(name: Symbol, order: usize) -> BaseToken {
        BaseToken{name: name, order: order, index: <IndexData as Default>::default()}
    }
}

impl ordered::Ordered for BaseToken {
    fn get_order(&self) -> usize { self.order }
}

impl Hash for BaseToken {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

impl Eq for BaseToken {}
impl PartialEq<BaseToken> for BaseToken
{
    fn eq(&self, other: &BaseToken) -> bool {
        self.name == other.name
    }
}

default_nameable_impl!(BaseToken, name);
// default_has_root_scope_impl!(BaseToken, Grammar, parent_scope);
// default_has_parent_scope_impl!(BaseToken, Namespace, parent_scope);


/* ****************************************************************
 * LabeledToken
 */
#[derive(Debug)]
pub struct LabeledToken {
    refcount: usize,
    label: Symbol,
    token: Ref<Token>
}

impl LabeledToken {
    pub fn new(base: Ref<Token>, label: Symbol) -> LabeledToken {
        LabeledToken{refcount: 1, label: label, token: base}
    }
}

impl Indexed for LabeledToken { fn index_data(&self) -> &IndexData { self.token.index_data() } }
impl symbol::Nameable for LabeledToken { fn name(&self) -> Symbol { self.token.name() } }
default_refcounted_impl!(LabeledToken);


/* **************************************************************** 
 * Synthetic
 */

/// Token representing some constraint on another token.  Synthetics are
/// currently used to represent arbitrary finite repetitions of other tokens.
#[derive(Debug)]
pub struct Synthetic {
    refcount: usize,
    base: Ref<Token>,
    repetitions: Range<usize>,
    rules: HashSet<Ref<Rule>>
}

impl Synthetic {
    pub fn new(base: Ref<Token>, repetitions: Range<usize>) -> Synthetic {
        Synthetic{refcount: 1, base: base, repetitions: repetitions,
                  rules: HashSet::new()}
    }
}
impl Token for Synthetic {
    fn token_type(&self) -> Type { Type::Synthetic }    
}

impl Indexed for Synthetic { fn index_data(&self) -> &IndexData { self.base.index_data() } }

impl ordered::Ordered for Synthetic {
    fn get_order(&self) -> usize { self.base.get_order() }
}


impl Repeatable for Synthetic {
    fn get_repetitions(&self) -> std::ops::Range<usize> {
        self.repetitions.as_range()
    }
}

impl Default for Synthetic {
    fn default() -> Synthetic {
        Synthetic{refcount: 0, base: Ref::null(), repetitions: Range::from_range(1..2), rules: HashSet::new()}
    }
}

impl Hash for Synthetic {
    fn hash<H: Hasher>(&self, state: &mut H) {
        unsafe { self.base.data_id() }.hash(state);
        self.repetitions.start.hash(state);
        self.repetitions.end.hash(state);
        for ref rule in self.rules.iter() { rule.hash(state) }
    }
}

impl Eq for Synthetic {}
impl PartialEq<Synthetic> for Synthetic
{
    fn eq(&self, other: &Synthetic) -> bool {
        self.base.data_id() == other.base.data_id() && self.repetitions == other.repetitions
    }
}

impl Nullable for Synthetic {
    fn is_nullable(&self) -> bool {
        self.repetitions.start == 0 || self.base.is_nullable()
    }
}

impl HasFirstSet for Synthetic { fn first_set_into(&self, out: &mut Vec<Ref<Token>>) { out.push(self.base.clone()) } }
impl symbol::Nameable for Synthetic { fn name(&self) -> Symbol { self.base.name() } }
default_refcounted_impl!(Synthetic);
// impl HasRootScope<Grammar> for Synthetic { fn root_scope(&self) -> Ref<Grammar> { self.base.root_scope() } }
// impl HasParentScope<Namespace> for Synthetic { fn parent_scope(&self) -> Ref<Namespace> { self.base.parent_scope() } }

// impl intrusive::RefCounted for Synthetic {
//     fn get_refcount(&self) -> usize { self.refcount }
//     fn add_ref(&mut self) -> usize { self.refcount += 1; self.refcount }
//     fn remove_ref(&mut self) -> usize { self.refcount -= 1; self.refcount }
// }


/* ****************************************************************
 * Terminal
 */

/// A terminal token. 
#[derive(Debug)]
pub struct Terminal {
    refcount: usize,
    base: BaseToken,
}

impl Terminal {
    pub fn new(sym: Symbol, ord: usize) -> Terminal {
        Terminal{refcount: 1,
                 base: BaseToken::new(sym, ord)}
    }
}

impl Indexed for Terminal { fn index_data(&self) -> &IndexData { &self.base.index } }

impl ordered::Ordered for Terminal {
    fn get_order(&self) -> usize { self.base.order }
}


impl Token for Terminal {
    fn token_type(&self) -> Type { Type::Terminal }
}


impl Repeatable for Terminal {}
impl Nullable for Terminal { fn is_nullable(&self) -> bool { false } }
impl HasFirstSet for Terminal {
    fn first_set_into(&self, out: &mut Vec<Ref<Token>>)  {
        let x = Reference::<Token>::from_trait_obj(self);
        out.push(x);
    }
}

impl Hash for Terminal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.base.hash(state)
    }
}

impl Eq for Terminal {}
impl PartialEq<Terminal> for Terminal
{
    fn eq(&self, other: &Terminal) -> bool {
        self.base == other.base
    }
}

default_nameable_impl!(Terminal, base.name);
default_refcounted_impl!(Terminal);
// default_has_root_scope_impl!(Terminal, Grammar, base.parent_scope);
// default_has_parent_scope_impl!(Terminal, Namespace, base.parent_scope);

/* ****************************************************************
 * Nonterminal
 */

/// Any compund (non-atomic) element within a grammar.  Nonterminals serve as
/// syntactic and/or semantic groupings of terminal tokens.
#[derive(Debug)]
pub struct Nonterminal {
    refcount: usize,
    base: BaseToken,
    rules: HashSet<Ref<Rule>>,
    index: Option<Box<TokenIndex>>,
}

impl Repeatable for Nonterminal {}
default_nameable_impl!(Nonterminal, base.name);
default_refcounted_impl!(Nonterminal);

// default_has_root_scope_impl!(Nonterminal, Grammar, base.parent_scope);
// default_has_parent_scope_impl!(Nonterminal, Namespace, base.parent_scope);

impl Nonterminal {
    pub fn new(sym: Symbol, ord: usize) -> Nonterminal {
        Nonterminal{refcount: 1,
                    base: BaseToken::new(sym, ord),
                    rules: HashSet::new(),
                    index: None}
    }

    fn is_superposition(&self) -> bool {
        self.rules.iter().all(|r| r.rhs.len() == 1 && match r.rhs[0].token_type() {
            Type::Terminal => true, _ => false } )
    }

}
impl Token for Nonterminal {
    fn token_type(&self) -> Type { Type::Nonterminal }
}

impl Indexed for Nonterminal { fn index_data(&self) -> &IndexData { &self.base.index } }

impl ordered::Ordered for Nonterminal {
    fn get_order(&self) -> usize { self.base.order }
}


impl Nullable for Nonterminal {
    fn is_nullable(&self) -> bool {
        self.rules.iter().any(|r| r.rhs.len() == 0 || r.rhs.iter().all(|rt| rt.is_nullable()))
    }
}

impl HasFirstSet for Nonterminal {
    fn first_set_into(&self, out: &mut Vec<Ref<Token>>) {
        for rule in self.rules.iter() { rule.first_set_into(out) }
    }
}

impl Hash for Nonterminal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.base.hash(state);
        for ref rule in self.rules.iter() { rule.hash(state) }
    }
}

impl Eq for Nonterminal {}

impl PartialEq<Nonterminal> for Nonterminal
{
    fn eq(&self, other: &Nonterminal) -> bool {
        self.base == other.base && match self.rules.difference(&other.rules).size_hint() {
            (_, Some(x)) => x == 0,
            _ => false }
    }
}
