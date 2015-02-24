///! Defines abstract items related to the finite-state automatons used by
///! most parsers.

/* ****************************************************************
 * Item
 */
/// A rule and associated parse-point, and optionally a sequence of lookahead
/// tokens.  An Item represents one rule, and the progress in parsing it, for
/// a given State of the parser's generated finite automaton.
pub struct Item
{
    /// Grammar rule associated with this item. 
    rule: Rc<Rule>,

    /// Number of tokens on the parser stack that should be considered part of
    /// the rule's right-hand side.
    point: usize,

    /// If not None, a sequence of tokens that are permitted to follow those in
    /// the right-hand side of the rule.
    lookahead: Option<Vec<Token>>
}

/* ****************************************************************
 * State
 */
/// A state within a finite-state automaton constructed for a Grammar.
pub struct State
{
    /// State creation-order index.
    order: usize,

    /// Items in this state.
    items: HashSet<Item>,
}
