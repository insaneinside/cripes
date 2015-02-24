///! Types and methods for syntax storage and manipulation.
use ordered;
use super::symbol;
use symbol::Symbol;

use std;
use std::fmt;
use std::default::Default;
use std::ops::{Deref,Range};
use std::collections::{HashSet,HashMap};
use std::vec::Vec;

use std::hash::{Hash, Hasher};

use super::util;
use super::util::intrusive;
use super::util::intrusive::{Ref,RefCounted};

/* ****************************************************************
 * Tokens, and their Exquisite Varieties
 */
/// Data-type used solely to identify the underlying concrete token type.
pub enum TokenType {
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
    util::intrusive::ExplicitlySized +
    util::intrusive::RefCounted +
    HasFirstSet +
    Repeatable +
    Nullable {
    fn token_type(&self) -> TokenType;

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
 * TokenSpec and TokenIndex
 */
/// Provides default values for specifying token parameters to a TokenProvider.
pub struct TokenSpec<'a> {
    name: &'a str,
    repetitions: Range<usize>,
    label: Option<&'a str>,
    // parent_scope: Ref<Namespace>
}


impl<'a> Default for TokenSpec<'a> {
    fn default() -> TokenSpec<'a> {
        TokenSpec{name: "", repetitions: 1..2, label: None}
    }
}

/// Per-scope grammar-data index.
pub struct TokenIndex {
    /// All tokens directly within this scope.
    pub tokens: HashMap<symbol::ID,Ref<Token>>,

    /// Rules for non-terminal tokens in this scope.  
    pub rules_for: HashMap<symbol::ID,Vec<Rule>>,

    /// Rules containing any token in this scope on their right-hand sides 
    pub rules_containing: HashMap<symbol::ID,Vec<(Ref<Rule>,Vec<usize>)>>,
}

impl TokenIndex {
    /// Find or create a token in this scope.
    ///
    /// @param name Name of the token to find or create.
    pub fn token_by_name(&mut self, name: &str) -> &Token {
        self.token_by_spec(TokenSpec{name: name, ..Default::default()})
    }

    /// Find or create a token in this scope. 
    ///
    /// @param spec Attributes the returned token should have.
    pub fn token_by_spec(&mut self, spec: TokenSpec) -> &Token {
        match self.find_token_by_spec(spec) {
            Some(tok) => tok,
            None => self.create_token(spec)
        }
    }

    /// Find a token in the current scope by name.
    fn find_token_by_name(&self, name: &str) -> Option<&Token> {
        
    }

    /// Find a token in the current scope by explicitly specifying
    /// its parameters.
    fn find_token_by_spec(&self, spec: TokenSpec) -> Option<&Token> {
        let ref tok = self.tokens[Symbol::hash(spec.name)];
        None
    }

    fn create_token(&mut self, spec: TokenSpec) -> &Token {
        let nonterminal_name = spec.name.chars().all(|c| c.is_lowercase());
        let terminal_name = spec.name.chars().all(|c| c.is_uppercase());
    }
}
        /* let parts = spec.split(TOKEN_SCOPE_SEPARATOR).collect::<Vec<&str>()>();
         * for part in parts.drain() {
         *             
         *         }
         *     }             */
impl intrusive::ExplicitlySized for TokenIndex {
    fn get_type_size(&self) -> usize { std::mem::size_of::<Self>() }
    fn get_type_align(&self) -> usize { std::mem::align_of::<Self>() }
}


impl Eq for TokenIndex {}
impl PartialEq<TokenIndex> for TokenIndex
{
    fn eq(&self, other: &TokenIndex) -> bool {
        self.tokens.iter().all(|(k, v)| other.tokens[*k].deref() == (*v).deref())
    }
}

impl fmt::Debug for TokenIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "")
    }
}


/* ****************************************************************
 * BaseToken -- common base for concrete token variants.
 */

/// Common data shared by all concrete token types. 
#[derive(Debug)]
pub struct BaseToken {
    order: usize,
    name: Ref<Symbol>
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
    label: Ref<Symbol>,
    token: Ref<Token>
}

impl symbol::Nameable for LabeledToken { fn name(&self) -> Ref<Symbol> { self.token.name() } }
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
    fn token_type(&self) -> TokenType { TokenType::Synthetic }    
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
impl symbol::Nameable for Synthetic { fn name(&self) -> Ref<Symbol> { self.base.name() } }
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
    fn token_type(&self) -> TokenType { TokenType::Terminal }
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
            TokenType::Terminal => true, _ => false } )
    }

}
impl Token for Nonterminal {
    fn token_type(&self) -> TokenType { TokenType::Nonterminal }
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

/* ****************************************************************
 * Rule
 */

/// A rule within a grammar.  Each rule defines *one* possible representation of
/// a particular Nonterminal in terms of a sequence of tokens, which may include
/// the same Nonterminal.
pub struct Rule
{
    refcount: usize,
    lhs: intrusive::Ref<Nonterminal>,
    rhs: Vec<intrusive::Ref<Token>>
}

impl Rule {
    /// Find all positions on the rule's right-hand side that a particular
    /// token appears.  This is useful for e.g. calculating a token's
    /// follow-set.
    pub fn rhs_indices_of(&self, tok: &Token) -> Vec<usize>
    {
        std::iter::range(0, self.rhs.len()).
            zip(self.rhs.iter()).
            filter_map(|(i, rhs_tok)| if rhs_tok == tok { Some(i) } else { None }).
            collect::<Vec<usize>>()
    }
}

impl HasFirstSet for Rule {
    fn first_set_into(&self, out: &mut Vec<Ref<Token>>) {
        for tok in self.rhs.iter() {
            match tok.token_type() {
                TokenType::Terminal => { out.push((*tok).clone()); break; },
                _ => { tok.first_set_into(out);
                       if ! tok.is_nullable() { break; } }
            }
        }
    }
}

impl Hash for Rule {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.lhs.hash(state);
        self.rhs.hash(state);
    }
}

impl fmt::Debug for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut r = write!(f, "{:?} ::=", (&*self.lhs as &symbol::Nameable).name());
        for x in self.rhs.iter() { r = write!(f, " {:?}", *x) }
        r
    }
}

impl Eq for Rule {}
impl PartialEq<Rule> for Rule {
    fn eq(&self, other: &Rule) -> bool {
        self.lhs == other.lhs && self.rhs == other.rhs
    }
}

impl intrusive::ExplicitlySized for Rule {
    fn get_type_size(&self) -> usize { std::mem::size_of::<Self>() }
    fn get_type_align(&self) -> usize { std::mem::align_of::<Self>() }
}


default_refcounted_impl!(Rule, refcount);

/* ****************************************************************
 * Grammar
 */
/// A set of terminal and nonterminal tokens, and rules that define
/// their relationships.
pub struct Grammar {
    refcount: usize,
    order_manager: ordered::Manager,
    index: TokenIndex,
}

impl Grammar {
    fn get_scope_index(&self) -> &TokenIndex {
        &self.index
    }
}

impl fmt::Debug for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "grammar {{\n"));
        for (ref sym, ref tok) in self.index.tokens.iter() {
            for ref rule in self.index.rules_for[**sym].iter() {
                try!(write!(f, "  {:?}\n", rule))
            }
        }
        write!(f, "}}")
    }
}

// impl symbol::scope::HasRootScope<Grammar> for Grammar {
//   fn root_scope(&self) -> intrusive::Ref<Grammar> {
//       intrusive::ref_to(self)
//   }
// }

impl Eq for Grammar {}
impl PartialEq<Grammar> for Grammar {
    fn eq(&self, other: &Grammar) -> bool {
        self.index == other.index
    }
}

impl intrusive::ExplicitlySized for Grammar {
    fn get_type_size(&self) -> usize { std::mem::size_of::<Self>() }
    fn get_type_align(&self) -> usize { std::mem::align_of::<Self>() }
}


default_refcounted_impl!(Grammar, refcount);
