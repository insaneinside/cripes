//! Graph-based representations of finite automata for use in
//! parser/lexer generation.
//!
//! ## Configuring your Automaton
//!
//! Automaton implementations defined by this module are designed for maximum
//! composability: by implementing [`TypesConfig`](trait.TypesConfig.html) for
//! a custom tag type, you gain full control over the types used to represent
//! the various components of an automaton.
//!
//! A typical `TypesConfig` implementation might look like this:
//!
//!


use std::iter;
use std::slice;
use std::ops::Range;
use std::marker::PhantomData;
use std::fmt::{self, Debug, Display};
use std::collections::HashMap;

use itertools::Itertools;
use bit_set::BitSet;

use pattern::{self, Atom, Enumerable, ByteOrChar, Element, Repetition, RepeatCount};
#[cfg(feature = "pattern-class")]
use pattern::{Class, ClassIter};

use util::set::{self, Contains};
use util::graph::interface::*;
use util::graph::{self, Build, BuildFull, BuildNodes, Builder, Target, WeightedNode};

pub mod interface;

use self::interface::{Automaton, DeterministicAutomaton, NondeterministicAutomaton};
use self::interface::{Transition as ITransition, Graph as IGraph};
use self::interface::{State as IState, StateMut};

/// "Trait alias" for common bounds on associated types in `TypesConfig`.
pub trait ConfigType: Clone + Debug + PartialEq {}
impl<T> ConfigType for T where T: Clone + Debug + PartialEq {}

/// Trait describing the derived parameter types used by the automaton
/// implementations in this module.  These
pub trait TypesConfig: Copy + Clone + Debug + PartialEq {
    /// Atom type used by inputs to automaton transitions.
    ///
    /// Atoms are what drive an automaton: they cause it to change states by
    /// triggering transitions with matching inputs.  Lexers' atoms are usually
    /// some kind of character, while a parser's atoms will generally be the
    /// set of terminal tokens in its grammar.
    type Atom: Atom + Enumerable + ConfigType;

    /// Type used to store named-submatch identifiers in actions.
    ///
    /// This type is used by the concrete `StateAction` and `TransitionAction`
    /// types provided in this module, but not otherwise required by trait
    /// bounds.  You are, however, encouraged to make use of this associated
    /// type when designing your own action types.
    type MatchId: ConfigType;

    /// Type used to represent arbitrary expressions in actions.
    ///
    /// This type is used by the concrete `StateAction` and `TransitionAction`
    /// types provided in this module, but not otherwise required by trait
    /// bounds.  You are, however, encouraged to make use of this associated
    /// type when designing your own action types.
    type Expr: ConfigType;

    /// Action type stored on states.
    type StateAction: ConfigType;

    /// Action type stored on transitions.
    type TransitionAction: ConfigType;

    /// Input type stored on transitions.
    ///
    /// An `Input` is used to represent all possible atomic values that may
    /// trigger a single transition.
    type Input:
        interface::Input<Atom=Self::Atom> +
        set::Contains<Self::Atom> +
        ConfigType;

    /// Type used to represent automaton transitions.
    type Transition:
        interface::Actions<Action=Self::TransitionAction> +
        interface::Transition<Input=Self::Input> +
        ConfigType;

    /// Type used to represent automaton states.
    type State: interface::StateMut<Action=Self::StateAction> + ConfigType;
}

/// `TypesConfig` implementation using the state, transition, and action types
/// defined in this module and the specified atom, match-id, and
/// expression types.
#[derive(Debug, PartialEq, Hash)]
pub struct BasicTypesConfig<A, M, E>(::std::marker::PhantomData<(A, M, E)>);

impl<A, M, E> Copy for BasicTypesConfig<A, M, E> {}
impl<A, M, E> Clone for BasicTypesConfig<A, M, E> {
    fn clone(&self) -> Self {
        BasicTypesConfig(::std::marker::PhantomData)
    }
}

impl<A, M, E> TypesConfig for BasicTypesConfig<A, M, E>
    where A: Atom + Enumerable + ConfigType,
          M: ConfigType,
          E: ConfigType,
{
    type Atom = A;
    type MatchId = M;
    type Expr = E;
    type StateAction = StateAction<Self>;
    type TransitionAction = TransitionAction<Self>;
    type Input = Input<Self::Atom>;
    type State = State<Self>;
    type Transition = Transition<Self>;
}


/// An action to be performed when a generated parser follows
/// a particular transition.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TransitionAction<C: TypesConfig<TransitionAction=TransitionAction<C>>> {
    /// Capture the value that triggered the transition.
    Capture(C::MatchId),

    /// Successfully finish the match, returning with the given expression.
    Accept(C::Expr)
}

impl<C> TransitionAction<C>
    where C: TypesConfig<TransitionAction=TransitionAction<C>>
{
    /// Check if the action specifies an "accept" action
    pub fn is_accept(&self) -> bool {
        match self {
            &TransitionAction::Accept(_) => true,
            _ => false
        }
    }

    /// Check if the action specifies a capture
    pub fn is_capture(&self) -> bool {
        match self {
            &TransitionAction::Capture(_) => true,
            _ => false
        }
    }

    /// Fetch the match-ID contained in a `Capture` action.
    pub fn match_id(&self) -> Option<C::MatchId>
        where C::MatchId: Copy
    {
        match self {
            &TransitionAction::Capture(id) => Some(id),
            _ => None
        }
    }

}

/// An action to be performed when a generated parser follows
/// a particular transition.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum StateAction<C: TypesConfig<StateAction=StateAction<C>>> {
    /// Mark the start of a captured portion of input at the current cursor
    /// location
    BeginSubmatch(C::MatchId),

    /// Mark the end of a captured portion of input at the current cursor
    /// location
    EndSubmatch(C::MatchId),

    /// Successfully finish the match, returning with the given expression.
    Accept(C::Expr)
}


// ----------------------------------------------------------------
// Input

/// Possible inputs for a Transition.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Input<A: Atom + Enumerable> {
    /// Transition on any input atom.
    Any,

    /// Transition on a specific atom.
    Atom(A),

    /// Transition on any one of a class of atoms.
    #[cfg(feature = "pattern-class")]
    Class(Class<A>),

    /// Transition on any atom that does _not_ match the contained input.
    Not(Box<Input<A>>),
}

impl<A: Atom + Enumerable> Input<A> {
    /// Check whether the input contains a `Not` variant.
    pub fn is_inverted(&self) -> bool {
        match self {
            &Input::Not(_) => true,
            _ => false,
        }
    }

    /// Fetch an iterator over the atoms specified in a transition's input.
    pub fn iter(&self) -> InputIter<A> {
        match self {
            &Input::Any => panic!("`Input::iter()` is not implemented for `Input::Any`"),
            &Input::Atom(a) => InputIterImpl::Once(iter::once(a)).into(),
            &Input::Class(ref c) => InputIterImpl::Class(c.iter()).into(),
            &Input::Not(ref input) => match &**input {
                &Input::Any => InputIterImpl::Empty(iter::empty()).into(),
                &Input::Atom(_) => panic!("`Input::iter()` is not implemented for `Input::Not(Atom)`"),
                &Input::Class(_) => panic!("`Input::iter()` is not implemented for `Input::Not(Class)`"),
                &Input::Not(_) => panic!("Invalid doubly-nested `Not` transition"),
            }
        }
    }
}

/// Iterator over the individual atoms specified in a transition's input.
pub struct InputIter<'a, A: 'a + Atom>(InputIterImpl<'a, A>);

enum InputIterImpl<'a, A: 'a + Atom> {
    Empty(iter::Empty<A>),
    Once(iter::Once<A>),
    Class(ClassIter<'a, A>)
}

impl<'a, A: 'a + Atom> From<InputIterImpl<'a, A>> for InputIter<'a, A> {
    fn from(iii: InputIterImpl<'a, A>) -> Self {
        InputIter(iii)
    }
}

impl<'a, A: 'a + Atom + Enumerable> Iterator for InputIter<'a, A> {
    type Item = A;

    fn next(&mut self) -> Option<Self::Item> {
        match (*self).0 {
            InputIterImpl::Empty(ref mut e) => e.next(),
            InputIterImpl::Once(ref mut o) => o.next(),
            InputIterImpl::Class(ref mut c) => c.next(),
        }
    }
}

impl<A: Atom + Enumerable> interface::Input for Input<A> {
    type Atom = A;
}

impl<A: Atom + Enumerable> PartialEq<A> for Input<A> {
    fn eq(&self, a: &A) -> bool {
        match self {
            &Input::Atom(atom) => *a == atom,
            _ => false
        }
    }
}

// ----------------------------------------------------------------
// Transition

/// Transition type used in automaton implementations.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Transition<C: TypesConfig> {
    /// Input that the transition accepts.
    input: C::Input,

    /// Actions to be performed when the transition is followed.
    actions: Vec<C::TransitionAction>
}


impl<C: TypesConfig> interface::Actions for Transition<C> {
    type Action = C::TransitionAction;

    /// Fetch a slice over the actions to be performed when the transition
    /// is followed.
    #[inline]
    fn actions(&self) -> &[Self::Action] {
        &self.actions[..]
    }
}


impl<C: TypesConfig> Transition<C>
{
    /// Create a new Transition with the specified input and an empty set of
    /// actions.
    pub fn new(input: C::Input) -> Self {
        Transition{input: input, actions: Vec::new()}
    }
}

impl<C> From<Input<C::Atom>> for Transition<C>
    where C: TypesConfig<Input=Input<<C as TypesConfig>::Atom>>
{
    fn from(input: Input<C::Atom>) -> Self {
        Transition{input: input, actions: Vec::new()}
    }
}


// FIXME: [bug] this should really be a TryFrom impl since not all Elements are
// valid inputs.
impl<C: TypesConfig> From<Element<C::Atom>> for Transition<C>
    where C::Input: From<Element<C::Atom>>
{
    #[inline]
    fn from(elt: Element<C::Atom>) -> Self {
        Transition{input: elt.into(), actions: Vec::new()}
    }
}

// FIXME: [bug] this should really be a TryFrom impl since not all Elements are
// valid inputs.
impl<A: Atom + Enumerable> From<Element<A>> for Input<A> {
    fn from(elt: Element<A>) -> Self {
        match elt {
            Element::Wildcard => Input::Any,
            Element::Atom(a) => Input::Atom(a),
            #[cfg(feature = "pattern-class")]
            Element::Class(a) => Input::Class(a),
            Element::Not(elt) => Input::Not(Box::new((*elt).into())),
            _ => panic!("Unexpected element type for conversion to NFA transition: {:?}", elt)
        }
    }
}

impl<C: TypesConfig> set::IsSubsetOf<Transition<C>> for Transition<C> {
    #[inline]
    fn is_subset_of(&self, other: &Self) -> bool {
        self.input.is_subset_of(&other.input)
    }
}


impl<A: Atom> set::IsSubsetOf<Input<A>> for Input<A> {
    fn is_subset_of(&self, other: &Self) -> bool {
        match self {
            // FIXME: [bug] technically a Class *could* specify _all_
            // possible atom values, but we're assuming it won't.
            &Input::Any => other == &Input::Any,
            &Input::Atom(a) => match other {
                &Input::Any => true,
                &Input::Atom(b) => a == b,
                #[cfg(feature = "pattern-class")]
                &Input::Class(ref c) => c.contains(a),
                &Input::Not(ref input) => ! self.is_subset_of(&**input) },
            #[cfg(feature = "pattern-class")]
            &Input::Class(ref c) => match other {
                &Input::Any => true,
                // A class CAN be a subset of an atom -- if the atom is the
                // only member of the class!  (Proper subset is a different
                // story, of course.)
                &Input::Atom(a) => c.len() == 1 && c.contains(a),
                &Input::Class(ref d) => c.is_subset_of(d),
                &Input::Not(ref input) => ! self.is_subset_of(&**input) },
            &Input::Not(ref input) => ! (&**input).is_subset_of(other),
        }
    }
}

impl<C: TypesConfig> set::Contains<C::Atom> for Transition<C>
    where C::Input: set::Contains<C::Atom>{
    #[inline]
    fn contains(&self, atom: C::Atom) -> bool {
        self.input.contains(atom)
    }
}

impl<A: Atom + Enumerable> set::Contains<A> for Input<A> {
    fn contains(&self, atom: A) -> bool {
        match self {
            &Input::Any => true,
            &Input::Atom(a) => atom == a,
            #[cfg(feature = "pattern-class")]
            &Input::Class(ref c) => c.contains(atom),
            &Input::Not(ref input) => ! input.contains(atom),
        }
    }
}

impl<C: TypesConfig> interface::Transition for Transition<C> {
    type Input = C::Input;

    /// Fetch a reference to the input accepted by the transition.
    #[inline]
    fn input(&self) -> &Self::Input {
        &self.input
    }
}

impl<C: TypesConfig> Display for Transition<C>
    where C::Input: Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <C::Input as Display>::fmt(&self.input, f)
    }
}

impl Display for Input<char> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Input::Atom(ref x) => {
                if x.is_whitespace() || x.is_control() {
                    write!(f, "'{}'", x.escape_default().collect::<String>()) }
                else { write!(f, "{:?}", x) } },
            &Input::Any => f.write_str("(any)"),
            _ => <Self as Debug>::fmt(self, f)
        }

    }
}


impl Display for Input<ByteOrChar> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Input::Atom(c) => {
                match c {
                    ByteOrChar::Byte(b) => Display::fmt(&b, f),
                    ByteOrChar::Char(c) => Display::fmt(&c, f)
                } },
            &Input::Any => f.write_str("(any)"),
            _ => <Self as Debug>::fmt(self, f)
        }
    }
}


impl Display for Input<u8> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Input::Atom(ref c) => {
                let x = *c as char;
                if x.is_whitespace() || x.is_control() {
                    write!(f, "'{}'", x.escape_default().collect::<String>()) }
                else { write!(f, "{:?}", x) } },
            &Input::Any => f.write_str("(any)"),
            _ => <Self as Debug>::fmt(self, f)
        }

    }
}


bitflags! {
    flags StateFlags: u8 {
        const STATE_ENTRY   = 1 << 0,
        const STATE_ACCEPT  = 1 << 1
    }
}

// ================================================================
// States
/** A node within a pattern's flow graph.

    States exist immediately before and after a token has been consumed; they
    associate arbitrary actions with parser states.
 */
#[derive(Clone, Debug, PartialEq)]
pub struct State<C: TypesConfig<State=State<C>>> {
    flags: StateFlags,

    /// Actions that should be performed when the parser reaches this state.
    actions: Vec<C::StateAction>

}

impl<C> interface::State for State<C>
    where C: TypesConfig<State=State<C>>
{
    fn is_accept(&self) -> bool {
        self.flags.contains(STATE_ACCEPT)
    }

    fn is_entry(&self) -> bool {
        self.flags.contains(STATE_ENTRY)
    }

}

impl<C> interface::StateMut for State<C>
    where C: TypesConfig<State=State<C>>
{
    fn set_accept(&mut self, accept: bool) {
        if accept { self.flags.insert(STATE_ACCEPT); }
        else { self.flags.remove(STATE_ACCEPT); }
    }
    fn set_entry(&mut self, entry: bool) {
        if entry { self.flags.insert(STATE_ENTRY); }
        else { self.flags.remove(STATE_ENTRY); }
    }
}

impl<C> State<C>
    where C: TypesConfig<State=State<C>>
{
    /// Create a new state with no set flags or specified actions.
    pub fn new() -> Self {
        State{flags: StateFlags::empty(), actions: Vec::new()}
    }

    /// Fetch the current list of assigned actions
    #[inline(always)]
    pub fn actions(&self) -> &[C::StateAction] {
        self.actions.as_slice()
    }
}

impl<C> interface::Actions for State<C>
    where C: TypesConfig<State=State<C>>
{
    type Action = C::StateAction;

    fn actions(&self) -> &[Self::Action] {
        self.actions.as_slice()
    }
}

impl<C> interface::ActionsMut for State<C>
    where C: TypesConfig<State=State<C>>
{
    /// Add an action to be performed when the parser reaches this state.
    fn add_action(&mut self, a: C::StateAction) {
        self.actions.push(a);
    }
}

impl<C> Display for State<C>
    where C: TypesConfig<State=State<C>>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <Self as Debug>::fmt(self, f)
    }
}

impl<C> Default for State<C>
    where C: TypesConfig<State=State<C>>
{
    fn default() -> Self {
        State::new()
    }
}


// Merging iterator implementation.
impl<'a, C> iter::FromIterator<&'a State<C>> for State<C>
    where C: 'static + TypesConfig<State=State<C>>
{
    /// Merge states from an iterator into a single state.
    fn from_iter<I: IntoIterator<Item=&'a State<C>>>(iter: I) -> Self {
        let mut out = State{flags: StateFlags::empty(), actions: Vec::new()};
        for state in iter {
            out.flags |= state.flags;
            out.actions.extend_from_slice(&state.actions[..]);
        }
        out
    }
}



// ================================================================
// Graphs

/// Graph type used for automata.
pub type GraphImpl<C> = graph::WeightedGraph<<C as TypesConfig>::State,<C as TypesConfig>::Transition>;


// ================================================================

/// Graph-based representation of automata patterns.
#[derive(Clone, Debug)]
struct GraphRepr<C: TypesConfig> {
    /// Graph representation of the control-flow of the pattern's automaton.
    graph: GraphImpl<C>,

    /// Index of the node that serves as the main entry point for the graph.
    entry: <GraphImpl<C> as Graph>::NodeId

    // Stored node analyses
    //node_analyses: HashMap<(TypeId,NodeId),Box<CloneAny>>,

    // Stored edge analyses
    //edge_analyses: HashMap<(TypeId,EdgeId),Box<CloneAny>>
}


/// Iterator over a state's inputs (outgoing-edge values)
pub struct InputsIter<'a, C: 'a + TypesConfig> {
    graph: &'a GraphImpl<C>,
    edges: slice::Iter<'a,<GraphImpl<C> as Graph>::EdgeId>
}

impl<'a, C> Iterator for InputsIter<'a, C>
    where C: 'a + TypesConfig
{
    type Item = &'a <C::Transition as interface::Transition>::Input;

    fn next(&mut self) -> Option<Self::Item> {
        match self.edges.next() {
            Some(eid) => Some(self.graph[*eid].input()),
            None => None,
        }
    }
}

/// Iterator over a state's inputs and corresponding next-state IDs
pub struct TransitionsIter<'a, C: 'a + TypesConfig> {
    graph: &'a GraphImpl<C>,
    edges: slice::Iter<'a,<GraphImpl<C> as Graph>::EdgeId>
}

impl<'a, C> Iterator for TransitionsIter<'a, C>
    where C: 'a + TypesConfig
{
    type Item = (<NFA<C> as Automaton<'a>>::TransitionId,
                 <NFA<C> as Automaton<'a>>::StateId);

    fn next(&mut self) -> Option<Self::Item> {
        match self.edges.next() {
            Some(eid)
                => { let next_state = self.graph.edge_target(*eid);
                     Some((*eid, next_state)) },
            None => None
        }
    }
}

/// Iterator over the IDs of states reachable from specific
/// state-and-input pair.
pub struct NextStatesIter<'a, C: 'a + TypesConfig> {
    graph: &'a GraphImpl<C>,
    edges: slice::Iter<'a,<GraphImpl<C> as Graph>::EdgeId>,
    input: C::Atom
}

impl<'a, C> Iterator for NextStatesIter<'a, C>
    where C: 'a + TypesConfig,
          C::Input: set::Contains<C::Atom>
{
    type Item = <NFA<C> as Automaton<'a>>::StateId;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.edges.next() {
                Some(eid) if (*self.graph[*eid]).input().contains(self.input) => return Some(self.graph.edge_target(*eid)),
                Some(_) => continue,
                None => return None
            }
        }
    }
}

macro_rules! ids_iter {
    ($name: ident, $id: ident, $doc: expr) => (
        #[doc = $doc]
        pub struct $name<'a,C: 'a + TypesConfig> {
            range: Range<usize>,
            id: PhantomData<&'a <NFA<C> as Automaton<'a>>::$id>
        }

        impl<'a, C> Iterator for $name<'a,C>
            where C: 'a + TypesConfig
        {
            type Item = <NFA<C> as Automaton<'a>>::$id;

            fn next(&mut self) -> Option<Self::Item> {
                self.range.next().map(|x| x.into())
            }
        }
    );
}

ids_iter!(StateIdsIter, StateId, "Iterator over the IDs of states in an automaton.");
ids_iter!(TransitionIdsIter, TransitionId, "Iterator over the IDs of transitions in an automaton.");

// ----------------------------------------------------------------


macro_rules! impl_automaton {
    ($Ap: ty, $N: ident, $C: ident) => {
        impl<'a,$C: 'a + TypesConfig> Automaton<'a> for $Ap {
            type Atom = $C::Atom;
            type Input = <$C::Transition as interface::Transition>::Input;

            type Transition = $C::Transition;
            type TransitionId = <GraphImpl<$C> as Graph>::EdgeId;
            type TransitionIdsIter = TransitionIdsIter<'a, $C>;
            type TransitionAction = C::TransitionAction;

            type State = $C::State;
            type StateId = <GraphImpl<$C> as Graph>::NodeId;
            type StateIdsIter = StateIdsIter<'a, $C>;
            type StateAction = C::StateAction;

            type InputsIter = InputsIter<'a,C>;
            type TransitionsIter = TransitionsIter<'a, $C>;

            #[inline]
            fn initial_state(&self) -> Self::StateId {
                self.0.entry
            }

            #[inline]
            fn state_count(&self) -> usize {
                self.0.graph.node_count()
            }

            #[inline]
            fn state_ids(&self) -> Self::StateIdsIter {
                StateIdsIter{range: 0..(self.0.graph.node_count()), id: PhantomData}
            }

            #[inline]
            fn state_inputs(&'a self, state: Self::StateId) -> Self::InputsIter {
                InputsIter{graph: &self.0.graph, edges: self.0.graph.outgoing_edges(state)}
            }

            #[inline]
            fn state_transitions(&'a self, state: Self::StateId) -> Self::TransitionsIter {
                TransitionsIter{graph: &self.0.graph, edges: self.0.graph.outgoing_edges(state)}
            }

            #[inline]
            fn state(&self, s: Self::StateId) -> &Self::State {
                &*self.0.graph[s]
            }

            #[inline]
            fn transition_count(&self) -> usize {
                self.0.graph.edge_count()
            }

            #[inline]
            fn transition_ids(&self) -> Self::TransitionIdsIter {
                TransitionIdsIter{range: 0..(self.0.graph.edge_count()), id: PhantomData}
            }

            #[inline]
            fn transition(&self, id: Self::TransitionId) -> &Self::Transition {
                &self.0.graph[id]
            }

            #[inline]
            fn transition_target(&self, id: Self::TransitionId) -> Self::StateId {
                self.0.graph.edge_target(id)
            }

            #[inline]
            fn transition_source(&self, id: Self::TransitionId) -> Self::StateId {
                self.0.graph.edge_source(id)
            }
        }
    }
}
macro_rules! impl_nondeterministic_automaton {
    ($Tp: ty, $N: ident, $C: ident) => {
        impl<'a,$C: 'a + TypesConfig> NondeterministicAutomaton<'a> for $Tp {
            type NextStatesIter = NextStatesIter<'a, $C>;
            #[inline]
            fn next_states(&'a self, state: Self::StateId, input: $C::Atom) -> Self::NextStatesIter {
                NextStatesIter{graph: &self.0.graph, edges: self.0.graph.outgoing_edges(state), input: input}
            }
        }
    }
}

// ----------------------------------------------------------------
// Utilities and functions for automaton construction.

// some helpers for creating identical closures
macro_rules! target_node {  ($slf:expr) => { |a: &mut _, b: &_| <Self as graph::BuildNodes<G, Element<C::Atom>>>::target_node($slf, a, b) }; }
macro_rules! build {  ($slf:expr) => { |a: &mut _, b, c| $slf.build(a, b, c) }; }
macro_rules! build_element {  ($G:ident,$C:ident,$slf:expr) => { |a: &mut _, b, c| <Self as graph::Build<$G,Element<$C::Atom>>>::build($slf, a, b, c) }; }



/// Helper for building an automaton graph from a `pattern::Element` pattern.
struct ElementBuilder<C>
{
    config: PhantomData<C>
}


impl<C, G> graph::Build<G, Element<C::Atom>> for ElementBuilder<C>
    where C: TypesConfig<StateAction=StateAction<C>>,
          G: ConcreteGraphMut<Node=graph::WeightedNode<C::State,<G as Graph>::EdgeId>,
                              Edge=graph::WeightedEdge<C::Transition,<G as Graph>::NodeId>>,
          C::Transition: From<Element<C::Atom>>,
          C::State: Default,
{
    fn build(&mut self, b: &mut Builder<G>,
             next: Target<G::NodeId>,
             input: Element<C::Atom>) -> G::NodeId {
        match input {
            Element::Anchor(_) => unimplemented!(),

            #[cfg(feature = "pattern-class")]
            Element::Class(_)
                => b.append_edge(next, input.into(), target_node!(self)),
            Element::Wildcard |
            Element::Atom(_) |
            Element::Not(_)
                => b.append_edge(next, input.into(), |g: &mut _, e: &_| <Self as BuildNodes<G, Element<C::Atom>>>::target_node(self, g, e)),
            Element::Sequence(seq)
                => b.chain(next, seq.iter().map(|x| x.clone().into()), build_element!(G, C, self)),
            Element::Union(union)
                => b.branch(next, union.iter().map(|x| x.clone().into()), build_element!(G, C, self)),

            Element::Repeat(repetition) => <Self as Build<G, Repetition<Element<C::Atom>>>>::build(self, b, next, *repetition),
        }
    }
}
impl<G, T, U> graph::Build<G, Repetition<U>> for T
    where T: graph::BuildFull<G, U>,
          G: ConcreteGraphMut + DirectedGraph,
          U: Clone
{
    fn build(&mut self, builder: &mut Builder<G>,
             next: Target<G::NodeId>,
             repetition: Repetition<U>) -> G::NodeId {
        match repetition.count() {
            RepeatCount::Any
                => {
                    // Unbounded repetition; this is the famous
                    // Kleene star.

                    // As we'll see below, additional structure is required
                    // when any of the inputs has outgoing transitions
                    // to itself.
                    let have_input_loops
                        = {
                            let g = builder.graph();

                            builder.stage_inputs().iter()
                                .flat_map(|id| iter::repeat(*id).zip(g.direct_successors(*id)))
                                .any(|(id, target_id)| id == target_id)
                        };

                    if have_input_loops {
                        // If any input *already* has a transition that
                        // targets itself, we need to
                        //
                        //   (1) mark all input nodes as outputs to handle
                        //       the zero-occurrences case;
                        builder.mark_inputs_as_outputs();

                        // [This recurse call prevents us from munging
                        // a parent stage's inputs and outputs list when we
                        // call `advance`.]
                        builder.recurse(next, repetition.into_inner().0, |b: &mut _, _, input: U| {
                            //   (2) create to a new output node to "lock
                            //       in" the element being repeated and
                            //       prevent following e.g. a preceding
                            //       unbounded repetition once we've
                            //       started this one; and finally
                            self.build(b, Target::NewOutput, input.clone());
                            b.advance();
                            self.build(b, Target::InputOutput, input)
                        })
                    } else {
                        // Without input loops, we simply build the
                        // repeated element as a transition back to the
                        // starting point (this also has the effect of
                        // marking all inputs as outputs).
                        self.build(builder, Target::InputOutput, repetition.into_inner().0)
                    } },

            // When the number of repetitions is bounded on the lower end
            // only, we first add the minimum number of occurrences
            // in-line; e.g. for the regular expression /a{2,}/ we'd begin
            // with the following:
            //
            //   (0)--"a"-->(1)--"a"-->(2)
            //
            // To handle the unbounded upper end, we simply add another
            // transition from the terminal node back to itself:
            //
            //   (0)--"a"-->(1)--"a"-->((2))<-╮
            //                          |     │
            //                          ╰-"a"-╯
            RepeatCount::AtLeast(n)
                => { assert!(n > 0);
                     builder.recurse(next, repetition.into_inner().0, move |b: &mut Builder<G>, tgt, input: U| {
                         b.chain(tgt, iter::repeat(input.clone()).take(n), build!(self));
                         b.advance();
                         let o = self.build(b, Target::InputOutput, input);
                         b.mark_output(o);
                         o
                     })
                },

            // When the repetition count is bounded at the upper end
            // (let's call that limit N), we simply build the repeated
            // element's subgraph N times in a chain, marking the
            // chain's intermediary nodes as stage outputs.
            RepeatCount::AtMost(n)
                => builder.recurse(next, repetition.into_inner().0, |b: &mut Builder<G>, tgt, input: U| {
                    let mut intermediates = Vec::from(b.stage_inputs());

                    assert!(n > 0);

                    let mut o: G::NodeId = 13.into();
                    for _ in 0..n {
                        o = self.build(b, tgt, input.clone());
                        intermediates.push(o);
                        b.advance();
                    }
                    for id in intermediates { b.mark_output(id); }
                    o
                }),
            // Exact repetition is the simplest possible case: we just
            // chain the repeated element together the specified number
            // of times.
            RepeatCount::Exact(n)
                => builder.chain(next, iter::repeat(repetition.into_inner().0).take(n), build!(self)),

            // Ranges (where the minimum is greater than zero and the
            // maximum is finite) are handled as exact repetitions
            // followed by `RepeatCount::AtMost` repetitions.
            RepeatCount::Between(m, n) => {
                builder.recurse(next, repetition.into_inner().0, move |b: &mut Builder<G>, tgt, input: U| {
                    b.chain(tgt, iter::repeat(input.clone()).take(m.clone()), build!(self));
                    b.advance();
                    b.recurse(tgt, Repetition::new(input, RepeatCount::AtMost(n - m)),
                              |b: &mut Builder<G>, tgt, input: Repetition<_>| {
                                  <Self as Build<G, Repetition<_>>>::build(self, b, tgt, input)
                              })
                })
            }
        }
    }
}


impl<C, G, I> graph::BuildNodes<G, I> for ElementBuilder<C>
        where G: ConcreteGraphMut<Node=graph::WeightedNode<C::State,<G as Graph>::EdgeId>>,
              C: TypesConfig,
              C::State: Default
{
    fn entry_node(&mut self, g: &mut G, _: &I)  -> G::NodeId {
        g.add_node(<C::State as Default>::default())
    }

    fn target_node<E>(&mut self, g: &mut G, _: &E) -> G::NodeId
        where (G::NodeId, G::NodeId, E): Into<G::Edge>
    {
        g.add_node(<C::State as Default>::default())
    }
}


impl<C, G> graph::BuildFull<G, Element<C::Atom>> for ElementBuilder<C>
    where C: TypesConfig<StateAction=StateAction<C>>,
          C::Transition: From<Element<C::Atom>>,
          G: ConcreteGraphMut<Node=graph::WeightedNode<C::State,<G as Graph>::EdgeId>,
                              Edge=graph::WeightedEdge<C::Transition,<G as Graph>::NodeId>>,
          C::State: Default
{
    fn finish(&mut self, b: &mut Builder<G>) {
        b.with_graph_mut(|b, mut g| {
            for id in b.stage_outputs().iter() {
                g.node_mut(*id).set_accept(true);
            }
        });
    }
}

/// Graph-based nondeterministic finite automaton.
///
/// NFAs are the result of transforming
/// a [`Pattern`](../pattern/type.Element.html) into a graph.
#[derive(Clone, Debug)]
pub struct NFA<C: TypesConfig>(GraphRepr<C>);
impl_automaton!(NFA<C>, NFA, C);
impl_nondeterministic_automaton!(NFA<C>, NFA, C);

impl<C: TypesConfig> From<pattern::Element<C::Atom>> for NFA<C>
    where C: TypesConfig<StateAction=StateAction<C>>,
          C::Transition: From<Element<C::Atom>>,
          C::State: Default
{
    /// Convert a pattern element into an NFA, using the given closure or
    /// function to map named captures to match IDs.
    fn from(elt: pattern::Element<C::Atom>) -> Self {
        let builder = ElementBuilder::<C> { config: PhantomData };
        let (graph, entry) = <ElementBuilder<C> as BuildFull<GraphImpl<C>, Element<C::Atom>>>::build_full(builder, elt);
        NFA(GraphRepr{graph: graph, entry: entry})
    }
}

impl<C: TypesConfig> interface::Graph for NFA<C> {
    type Graph = GraphImpl<C>;

    /// Fetch a reference to the graph used to represent the NFA.
    fn graph(&self) -> &Self::Graph {
        &self.0.graph
    }
}

// ----------------------------------------------------------------

/// Graph-based deterministic finite automaton.
#[derive(Clone, Debug)]
pub struct DFA<C: TypesConfig>(GraphRepr<C>);
impl_automaton!(DFA<C>, DFA, C);

impl<C: TypesConfig> From<pattern::Element<C::Atom>> for DFA<C>
    where C: TypesConfig<StateAction=StateAction<C>,State=State<C>>,
          C::Transition: From<C::Input> + From<Element<C::Atom>>,
          C::State: Default + for<'a> iter::FromIterator<&'a C::State>,
{
    /// Convert a pattern element into an NFA.
    fn from(elt: pattern::Element<C::Atom>) -> Self {
        NFA::<C>::from(elt).into()
    }
}

impl<C: TypesConfig> interface::Graph for DFA<C> {
    type Graph = GraphImpl<C>;

    /// Fetch a reference to the graph used to represent the NFA.
    fn graph(&self) -> &Self::Graph {
        &self.0.graph
    }
}

/// Use the powerset construction method to build the graph for the
/// deterministic finite automaton that corresponds to the given
/// non-deterministic finite automaton, starting at the given set of
/// NFA states.
///
/// This implementation avoids the necessity of manually tracking "unmarked"
/// (unhandled) states in the resulting DFA by eagerly recursing whenever it
/// finds a set of NFA states _not_ already present as a key in the mapping.
///
/// ## Arguments
///
///   - `n` is the source NFA
///
///   - `g` is the graph for the DFA being built
///
///   - `states_set` specifies the set of NFA states for which a corresponding
///     DFA state with outgoing transitions is to be built
///
///   - `states_map` records the IDs of DFA states (in `g`) that correspond to
///     sets of NFA states
///
/// After `build_dfa_recursive` returns, `states_set` contains the IDs of
/// accepting states in the DFA.
fn build_dfa_recursive<'n, 'g, 'd, C, D, N, G>(n: &'n N, g: &'g mut G, states_set: &mut BitSet,
                                            states_map: &mut HashMap<BitSet, D::StateId>)
                                            -> D::StateId
    where C: 'n + TypesConfig,
          D: Automaton<'d,State=C::State,StateAction=C::StateAction>,
          D::Transition: From<D::Input>,
          D::State: iter::FromIterator<&'n N::State>,

          N: NondeterministicAutomaton<'n,Atom=D::Atom,State=D::State,StateAction=D::StateAction>,
          N::State: 'n,
          N::Transition: From<N::Input> + Into<D::Transition>,
          <N::Transition as interface::Transition>::Input: Into<<D::Transition as interface::Transition>::Input>,

          G: 'g + Graph + ConcreteGraphMut<Node=WeightedNode<D::State,<G as Graph>::EdgeId>>,
          G::Node: From<D::State>,
          G::NodeId: Into<D::StateId>,
          G::Edge: From<(usize, usize, D::Transition)>
{
    // if the provided set of NFA states isn't already in the states map,
    // insert it and copy actions/flags from the NFA states
    let dfa_state: D::StateId =
        if states_map.contains_key(states_set) {
            states_map[&*states_set]
        } else {
            let state = { states_set.iter().map(|ns| -> &'n N::State { n.state(ns.into()) }).collect::<D::State>() };
            /*let mut state = State{flags: StateFlags::empty(), actions: Vec::new()};
            for nfa_state in states_set.iter().map(|ns| n.state(ns.into())) {
                state.flags |= (*nfa_state).flags;
                state.actions.extend_from_slice(&nfa_state.actions[..]);
            }*/
            let sid = g.add_node(state).into();
            states_map.insert(states_set.clone(), sid);
            sid
        };

    // for each possible input on any of the NFA states corresponding to
    // this DFA state, determine the set of NFA states we can arrive at.
    let mut target_states = BitSet::with_capacity(n.state_count());

    let nfa_states = states_set.clone();
    states_set.clear();

    if (*g.node(dfa_state.index().into())).is_accept() {
        states_set.insert(dfa_state.index());
    }

    for (input, next_states) in
        nfa_states.into_iter()

        // convert `src: StateId...`
        // to      `(input: T, dst: StateId)...`
        .flat_map(|sid| n.state_transitions(sid.into()))

        // convert `(input: Input<A>, id: StateId)...`
        // to      `(input, Vec<(input, id)>)...`
        .sorted_by(|&(tr1, _), &(tr2, _)| n.transition(tr1).input().cmp(n.transition(tr2).input()))
        .into_iter()
        .group_by(|&(tr, _)| n.transition(tr).input())
        .into_iter()            // itertools::structs::GroupBy implements
                                // IntoIterator, not Iterator.

        // convert `(input, Vec<(input, StateId)>)...`
        // to      `(input, Vec<StateId>)...`
        .map(|(input, transitions)| (input, transitions.into_iter().map(|(_, next_state)| next_state.index())))
    {
        // Initialize the target-states set to the set of states for this input
        target_states.clear();
        target_states.extend(next_states);

        // Determine the *DFA* target state; we'll recursively create it if it
        // doesn't exist.
        let target_dfa_state: D::StateId =
            if ! states_map.contains_key(&target_states) {
                let sid = { build_dfa_recursive::<C, D, N, G>(n, g, &mut target_states, states_map) };
                states_set.union_with(&target_states); // record accepting DFA states
                sid
            } else {
                *states_map.get(&target_states).unwrap()
            };
        if g.node(target_dfa_state.index().into()).data().is_accept() {
            states_set.insert(target_dfa_state.index());
        }

        // Add the edge that corresponds to this input.
        g.add_edge((dfa_state.index(), target_dfa_state.index(), D::Transition::from(input.clone().into())));
    }

    dfa_state
}

impl<'a, C> DeterministicAutomaton<'a> for DFA<C>
    where C: 'a + TypesConfig,
          C::Input: interface::Input<Atom=C::Atom>
{
    #[inline]
    fn next_state(&self, s: Self::StateId, input: C::Atom) -> Option<Self::StateId> {
        // With an adjacency-list graph representation, the best we can do is
        // a binary search -- IF the lists are sorted, which they're not
        // for now.
        //
        // FIXME: [optimize] replace linear with binary search?
        let g = self.graph();
        g.outgoing_edges(s)
            .find(|eid| (*g[*eid]).input().contains(input))
            .map(|eid| g.edge_target(*eid))
    }
}


impl<C> From<NFA<C>> for DFA<C>
    where C: TypesConfig,
          C::Transition: From<C::Input>,
          C::Transition: From<C::Transition>,
          C::State: for<'a> iter::FromIterator<&'a C::State>,
{
    /// Convert a non-deterministic finite automaton to a minimal deterministic
    /// one using the powerset construction and (if the crate feature
    /// "brzozowski" is enabled) Brzozowski's minimization algorithm.
    #[cfg_attr(not(feature = "brzozowski"), allow(unused_mut))]
    fn from(n: NFA<C>) -> Self {
        let mut states_map: HashMap<BitSet,<GraphImpl<C> as Graph>::NodeId> =
            HashMap::new();
        let mut nfa_states = BitSet::with_capacity(n.state_count());
        nfa_states.insert(n.initial_state().index());

        // Build the initial graph.
        let (entry, mut graph) = {
            let mut g = GraphImpl::<C>::new();
            let entry = build_dfa_recursive::<C,DFA<C>,_,_>(&n, &mut g, &mut nfa_states,
                                                              &mut states_map);
            (entry, g) };

        #[cfg(feature = "brzozowski")]
        let (entry, graph) = {
            // Use Brzozowski's algorithm to minimize the DFA.  This algorithm is
            // more brute-force than clever: we simply reverse the DFA to get an
            // NFA graph, which we use as the input to the powerset-construction
            // routine to construct the reversed, minimized DFA.
            //
            // FIXME: [optimize] We could achieve better worst-case behavior by
            // employing a partition-based minimization algorithm [e.g. Hopcroft
            // (1971)] instead.
            states_map.clear();
            graph.rev();

            let mut final_graph = GraphImpl::<C>::new();
            let intermediate_nfa = NFA::<C>(GraphRepr{graph: graph, entry: entry});
            for state in nfa_states.into_iter() {
                let mut start = BitSet::new();
                start.insert(state);
                build_dfa_recursive::<C,DFA<C>,_,_>(&intermediate_nfa, &mut final_graph, &mut start,
                                                    &mut states_map);
            }
            final_graph.rev();


            // Find the initial state in the final DFA graph; there should be
            // exactly one with no incoming edges from other nodes.
            let entry = final_graph.node_ids()
                .find(|&sid| final_graph.incoming_edges(sid).all(|&eid| final_graph.edge_source(eid) == sid))
                .expect("Failed to find entry node for minimized DFA")
                .into();
            (entry, final_graph)
        };

        DFA(GraphRepr{graph: graph, entry: entry})
    }
}
