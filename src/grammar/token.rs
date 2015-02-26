///! Token traits, variants, and related data structures.
use std;
use std::fmt;
use std::ops::Range;
use std::default::Default;
use std::hash::{Hash, Hasher};
use std::collections::{LinkedList,HashMap,HashSet};

use symbol;
use symbol::Symbol;
use grammar::Rule;
use grammar::TokenIndex;
use util::intrusive;
use util::intrusive::Ref;

/// Per-token data stored in the index.
struct IndexData {
    /// Reference to the token itself.
    token: Ref<Token>,

    /// Rules in which the token appears on the left-hand side.  These rules
    /// define the token.
    // FIXME: do we need this?  It may make more sense to store rules in their
    //     parent LHS nonterminal or synthetic, since terminals (and
    //     potentially other token types we dream up) don't have rules.
    rules_for: LinkedList<Rule>,

    /// (rule,vec(position)) pairs for rules that contain the token on their
    /// right-hand sides.  Rules in this list are used-by relations.
    rules_containing: LinkedList<(Ref<Rule>,Vec<usize>)>
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
    Synthetic
}

static TOKEN_SCOPE_SEPARATOR: char = ':';


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
pub trait Repeatable {
    /// Fetch the repetition range for this token.  
    fn get_repetitions(&self) -> Range<usize> { 1..2 } /* default 1 */
}


/// Abstract interface to all Token variants.
pub trait Token:
    Eq +
    fmt::Debug + 
    symbol::Nameable +
    intrusive::ExplicitlySized +
    intrusive::RefCounted +
    HasFirstSet +
    Repeatable +
    Nullable {
    /// Get the underlying token's type.
    fn token_type(&self) -> Type;

    /// Create a SyntheticToken that represents a variable- or fixed-length
    /// number of repetitions of another token.
    fn repeat(&self, range: Range<usize>) -> Synthetic {
        Synthetic{base: <Self as intrusive::RefCounted>::ref_to_self(), repetitions: range, ..Default::default()}
    }

}

impl<T> intrusive::RefCounted for T where T: Token {
    fn get_refcount(&self) -> usize { self.refcount }
    fn add_ref(&mut self) -> usize { self.refcount += 1; self.refcount }
    fn remove_ref(&mut self) -> usize { self.refcount -= 1; self.refcount }
}



/* ****************************************************************
 * Spec
 */
/// Summary of a token's parameters relevant to its use within a grammar.
/// Spec is used to find and create tokens in
/// a [TokenIndex](struct.TokenIndex.html), which also uses it internally as
/// the key type in mappings to Tokens.
pub struct Spec {
    name: Symbol,
    repetitions: Range<usize>,
    label: Option<Symbol>,
    // parent_scope: Ref<Namespace>
}

impl Default for Spec {
    fn default() -> Spec {
        Spec{name: symbol::Inline::new(""), repetitions: 1..2, label: None}
    }
}

/* ****************************************************************
 * BaseToken -- common base for concrete token variants.
 */

/// Common data shared by all concrete token types. 
#[derive(Debug)]
pub struct BaseToken {
    order: usize,
    name: Symbol
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
default_refcounted_impl!(BaseToken, refcount);
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

impl symbol::Nameable for LabeledToken { fn name(&self) -> Symbol { self.token.name() } }
default_refcounted_impl!(LabeledToken, refcount);


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


impl Token for Synthetic {
    fn token_type(&self) -> Type { Type::Synthetic }    
}

impl intrusive::ExplicitlySized for Synthetic {
    fn get_type_size(&self) -> usize { std::mem::size_of::<Self>() }
    fn get_type_align(&self) -> usize { std::mem::align_of::<Self>() }
}

impl Repeatable for Synthetic {
    fn get_repetitions(&self) -> Range<usize> {
        self.repetitions
    }
}

impl Default for Synthetic {
    fn default() -> Synthetic {
        Synthetic{refcount: 0, base: Ref::null(), repetitions: 1..2, rules: HashSet::new()}
    }
}

impl Hash for Synthetic {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.base.hash(state);
        self.repetitions.start.hash(state);
        self.repetitions.end.hash(state);
        for ref rule in self.rules.iter() { rule.hash(state) }
    }
}

impl Eq for Synthetic {}
impl PartialEq<Synthetic> for Synthetic
{
    fn eq(&self, other: &Synthetic) -> bool {
        self.base == other.base && self.repetitions == other.repetitions
    }
}

impl Nullable for Synthetic {
    fn is_nullable(&self) -> bool {
        self.repetitions.start == 0 || self.base.is_nullable()
    }
}

impl HasFirstSet for Synthetic { fn first_set_into(&self, out: &mut Vec<Ref<Token>>) { out.push(self.base.clone()) } }
impl symbol::Nameable for Synthetic { fn name(&self) -> Symbol { self.base.name() } }
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


impl Token for Terminal {
    fn token_type(&self) -> Type { Type::Terminal }
}
impl intrusive::ExplicitlySized for Terminal {
    fn get_type_size(&self) -> usize { std::mem::size_of::<Self>() }
    fn get_type_align(&self) -> usize { std::mem::align_of::<Self>() }
}

impl Repeatable for Terminal {}
impl Nullable for Terminal { fn is_nullable(&self) -> bool { false } }
impl HasFirstSet for Terminal {
    fn first_set_into(&self, out: &mut Vec<Ref<Token>>)  {
        out.push(<Self as intrusive::RefCounted>::ref_to_self())
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
// default_refcounted_impl!(Terminal, base.refcount);
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
    index: TokenIndex,
}

impl Repeatable for Nonterminal {}
default_nameable_impl!(Nonterminal, base.name);
// default_refcounted_impl!(Nonterminal, base.refcount);
// default_has_root_scope_impl!(Nonterminal, Grammar, base.parent_scope);
// default_has_parent_scope_impl!(Nonterminal, Namespace, base.parent_scope);

impl Nonterminal {
    /// Fetch a reference to the token as a `Namespace`.  If the token is not
    /// a `Namespace`, returns Ref(None). 
    fn get_scope_index(&self) -> &TokenIndex {
        &self.index
    }
    
    fn is_superposition(&self) -> bool {
        self.rules.iter().all(|r| r.rhs.len() == 1 && match r.rhs[0].token_type() {
            Type::Terminal => true, _ => false } )
    }

}
impl Token for Nonterminal {
    fn token_type(&self) -> Type { Type::Nonterminal }
}
impl intrusive::ExplicitlySized for Nonterminal {
    fn get_type_size(&self) -> usize { std::mem::size_of::<Self>() }
    fn get_type_align(&self) -> usize { std::mem::align_of::<Self>() }
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
