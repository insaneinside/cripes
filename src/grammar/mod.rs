//! Types and methods for syntax storage and manipulation.

use std;
use std::fmt;
use std::vec::Vec;
use std::clone::Clone;
use std::default::Default;
use std::ops::Deref;
use std::hash::{Hash, Hasher};
use std::collections::HashMap;


use ordered;
use symbol;
use symbol::Symbol;
use util::intrusive;
use util::intrusive::{Ref,RefCounted,Reference,RefCountHolder};


pub mod token;
pub use grammar::token::Token;
pub use grammar::token::Spec as TokenSpec;
pub use grammar::token::Type as TokenType;
use grammar::token::{Terminal,Nonterminal,Synthetic,LabeledToken};
use grammar::token::{HasFirstSet,Typed,Nullable,Indexed};

// pub use self::token::Type as TokenType;
// pub use self::token::Spec

/// Per-scope grammar-data index.
pub struct TokenIndex {
    /// Resources this index uses for token IDs and name/label symbols.
    resources: Ref<CommonResources>,

    /// All tokens directly within this scope.
    pub tokens: HashMap<token::Spec,Ref<Token>>,
}

impl TokenIndex {
    fn new(resources: Ref<CommonResources>) -> Self {
        TokenIndex{resources: resources, tokens: HashMap::new()}
    }

    /// Fetch the symbol corresponding to a given string.
    pub fn symbol(&mut self, name: &str) -> Symbol {
        self.resources.symbol_pool.symbol(name)
    }

    /// Find or create a token in this scope.
    ///
    /// @param name Name of the token to find or create.
    pub fn token_by_name(&mut self, name: &str) -> Ref<Token> {
        let sym = self.symbol(name);
        self.token_by_spec(TokenSpec{name: sym, ..Default::default()})
    }

    /// Find or create a token in this scope.
    ///
    /// @param spec Attributes the returned token should have.
    pub fn token_by_spec(&mut self, spec: TokenSpec) -> Ref<Token> {
        match self.find_token_by_spec(spec) {
            Some(tok) => tok,
            None => { let tok = self.create_token(spec).ok().unwrap();
                      self.store_token(spec, tok) }
        }
    }

    /// Find a token in the current scope by name.
    pub fn find_token_by_name(&self, name: Symbol) -> Option<Ref<Token>> {
        self.find_token_by_spec(TokenSpec{name: name, ..Default::default()})
    }

    /// Find a token in the current scope by explicitly specifying
    /// its parameters.
    pub fn find_token_by_spec(&self, spec: TokenSpec) -> Option<Ref<Token>> {
        match self.tokens.get(&spec) {
            Some(ref_tok) => Some(ref_tok.clone()),
            None => None
        }
    }

    fn store_token<'a>(&'a mut self, spec: TokenSpec, tok: Ref<Token>) -> Ref<Token> {
        self.tokens.insert(spec, tok.clone());
        tok
    }

    /// Unconditionally create a token, but don't store it in the index.
    fn create_token(&mut self, spec: TokenSpec) -> Result<Ref<Token>,&str> {
        let name_is_terminal = spec.name.as_slice().chars().all(|c| c.is_uppercase());
        let name_is_nonterminal = spec.name.as_slice().chars().all(|c| c.is_lowercase());
        if ! name_is_terminal && ! name_is_nonterminal { Err("Invalid mixed-case token name") }
        // If this is a synthetic or labeled token, create it as appropriate.
        else if spec.requires_base() {
            let base = self.token_by_spec(spec.base_spec());
            match spec.label {
                Some(lbl) => Ok(Reference::new_on_heap(LabeledToken::new(base, lbl))),
                _ => if spec.repetitions.as_range() == (1..2) { unreachable!() }
                else { Ok(Reference::new_on_heap(Synthetic::new(base, spec.repetitions))) }
            }
            // Otherwise create the basic token.
        } else {
            let order = self.resources.order_manager.next_order(ordered::Category::TOKEN);
            if name_is_terminal {
                Ok(Reference::new_on_heap(Terminal::new(spec.name, order)))
            } else if name_is_nonterminal {
                Ok(Reference::new_on_heap(Nonterminal::new(spec.name, order)))
            } else { unreachable!() }
        }
    }
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

impl token::HasFirstSet for Rule {
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
        for elt in self.rhs.iter() { elt.data_id().hash(state); }
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

default_refcounted_impl!(Rule, refcount);

/* ****************************************************************
 * CommonResources
 */
/// Per-grammar resources required by every scope within that grammar.
#[derive(Debug)]
struct CommonResources {
    refcount: usize,
    order_manager: ordered::Manager,
    symbol_pool: symbol::Pool,
}
default_refcounted_impl!(CommonResources);
impl CommonResources {
    fn new() -> CommonResources {
        CommonResources{refcount: 1,
                        symbol_pool: symbol::Pool::new(),
                        order_manager: ordered::Manager::new()}
    }
}

/* ****************************************************************
 * Grammar
 */
/// A set of terminal and nonterminal tokens, and rules that define
/// their relationships.
pub struct Grammar {
    refcount: usize,
    resources: Ref<CommonResources>,
    index: TokenIndex,
}

impl Grammar {
    pub fn new() -> Grammar {
        let resources: Ref<CommonResources> = Reference::new_on_heap(CommonResources::new());
        Grammar{refcount: 1,
                resources: resources.clone(),
                index: TokenIndex::new(resources)}
    }

    /// Get the top-level token index for the grammar.
    pub fn index(&self) -> &TokenIndex {
        &self.index
    }
    pub fn index_mut(&mut self) -> &mut TokenIndex {
        &mut self.index
    }
}

impl fmt::Debug for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "grammar {{\n"));
        for (_, ref tok) in self.index.tokens.iter() {
            for ref rule in tok.index_data().rules_for.iter() {
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


default_refcounted_impl!(Grammar, refcount);
