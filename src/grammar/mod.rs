///! Types and methods for syntax storage and manipulation.

use std;
use std::fmt;
use std::vec::Vec;
use std::default::Default;
use std::ops::{Deref,Range};
use std::hash::{Hash, Hasher};
use std::collections::HashMap;


use super::ordered;
use symbol;
use symbol::Symbol;
use super::util;
use super::util::intrusive;
use super::util::intrusive::{Ref,RefCounted};

pub mod token;
pub use grammar::token::Spec as TokenSpec;
pub use grammar::token::Type as TokenType;
pub use grammar::token::{Token,Nonterminal};

// pub use self::token::Type as TokenType;
// pub use self::token::Spec

/// Per-scope grammar-data index.
pub struct TokenIndex {
    /// Symbol pool used to create new token names.
    pub symbol_pool: Ref<symbol::Pool>,

    /// All tokens directly within this scope.
    pub tokens: HashMap<token::Spec,token::IndexData>,
}

impl TokenIndex {
    /// Fetch the symbol corresponding to a given string.
    pub fn symbol(&self, name: &str) -> Symbol {
        self.symbol_pool.symbol(name)
    }

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
    pub fn find_token_by_name(&self, name: &str) -> Option<&Token> {
        self.find_token_by_spec(TokenSpec{name: name, ..Default::default()})
    }

    /// Find a token in the current scope by explicitly specifying
    /// its parameters.
    pub fn find_token_by_spec(&self, spec: TokenSpec) -> Option<&Token> {
        let tok = self.tokens.get(spec.name);
        None
    }

    /// Unconditionally create a token.
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
    /// Get the top-level token index for the grammar.
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
