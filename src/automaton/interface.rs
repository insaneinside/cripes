//! Basic interface traits for state-machine/automaton implementations.

use std::fmt::Debug;

use util::{graph, set};
use util::graph::interface::Id;
use pattern::Atom;

/// Trait for types used to describe possible inputs that trigger
/// a given transition.
pub trait Input: Clone + Debug + Eq + Ord + set::Contains<<Self as Input>::Atom> + set::IsSubsetOf<Self> {
    /// Smallest transition unit used.
    type Atom: Atom;
}

/// Trait for types that contain some specification of actions to be performed.
pub trait Actions {
    /// Action type
    type Action;

    /// Fetch a slice over the contained actions.
    fn actions(&self) -> &[Self::Action];
}

/// Trait for types that support adding actions.
pub trait ActionsMut: Actions {
    /// Add an action to the object.
    fn add_action(&mut self, action: Self::Action);
}

/// An automaton state.
pub trait State: Actions {
    /// Check if the state is "accepting", i.e. is valid as an exit node for
    /// the automaton.
    fn is_accept(&self) -> bool;

    /// Check if the state is a valid entry node for the automaton.
    fn is_entry(&self) -> bool;
}

/// Mutation methods for automaton states.
pub trait StateMut: State + ActionsMut {
    /// Set whether the state is an accepting state.
    fn set_accept(&mut self, accept: bool);

    /// Set whether the state is an accepting state.
    fn set_entry(&mut self, entry: bool);
}

/// Metadata-bearing transition from one automaton state to another.
pub trait Transition: Actions + Clone + Debug + PartialEq {
    /// Input type that triggers this transition.
    type Input: Input;

    /// Fetch the input on which the transition is followed.
    fn input(&self) -> &Self::Input;
}

/// Interface for automaton implementations
pub trait Automaton<'a, A: 'a + Atom> {
    /// Action type used by [`Actions`](trait.Actions.html) implementations on
    /// [`State`](trait.State.html) and [`Transition`](state.Transition.html)
    /// associated types.
    type Action;

    /// Concrete type used to represent automaton states.
    type State: StateMut<Action=Self::Action>;

    /// Type used to identify individual states.
    type StateId: Id;

    /// Iterator over the IDs of states in the automaton.
    type StateIdsIter: Iterator<Item=Self::StateId>;

    /// Concrete type used to represent the possible atomic inputs that can
    /// trigger a transition.
    type Input: 'a + Input<Atom=A>;

    /// Iterator over the possible inputs for a state.
    type InputsIter: Iterator<Item=&'a Self::Input>;

    /// Type used to store the input and any relevant metadata for a transition
    /// from one state to another.
    type Transition: Transition<Input=Self::Input, Action=Self::Action>;

    /// Type used to identify individual transitions.
    type TransitionId: Id;

    /// Iterator over the IDs of transitions in the automaton.
    type TransitionIdsIter: 'a + Iterator<Item=Self::TransitionId>;

    /// Iterator over the possible outgoing transitions for a state.
    type TransitionsIter: Iterator<Item=(Self::TransitionId, Self::StateId)>;

    // --------------------------------

    /// Get the number of states in the automaton.
    fn state_count(&self) -> usize;

    /// Get an iterator over the IDs of all states in the automaton.
    fn state_ids(&self) -> Self::StateIdsIter;

    /// Get the ID of the automaton's initial state.
    fn initial_state(&self) -> Self::StateId;

    /// Get a reference to a specific state.
    fn state(&self, s: Self::StateId) -> &Self::State;

    /// Fetch an iterator over the valid inputs for a given state.
    fn state_inputs(&'a self, s: Self::StateId) -> Self::InputsIter;

    /// Fetch an iterator over the `(transition_id, next_state_id)` pairs for
    /// a given state.
    fn state_transitions(&'a self, s: Self::StateId) -> Self::TransitionsIter;

    // --------------------------------

    /// Get the number of transitions in the automaton.
    fn transition_count(&self) -> usize;

    /// Get an iterator over the IDs of all transitions in the automaton
    fn transition_ids(&self) -> Self::TransitionIdsIter;

    /// Get a reference to a specific transition.
    fn transition(&self, id: Self::TransitionId) -> &Self::Transition;
}

/// Interface for deterministic automatons.
pub trait DeterministicAutomaton<'a, A: 'a + Atom>: Automaton<'a, A> {
    /// Determine the next state to which the DFA will transition given its
    /// current state and the input token.
    fn next_state(&self, current_state: Self::StateId, input: A) -> Option<Self::StateId>;
}

/// Interface for non-deterministic automatons.
pub trait NondeterministicAutomaton<'a,A: 'a + Atom>: Automaton<'a, A> {
    /// Iterator type returned by `next_states`.
    type NextStatesIter: Iterator<Item=Self::StateId>;

    /// Fetch an iterator over the set of states to which the automaton
    /// transitions on the given input.
    fn next_states(&'a self, current_state: Self::StateId, input: A) -> Self::NextStatesIter;
}

/// Interface for graph-based automata that support exposing their internal
/// graph representations.
pub trait Graph {
    /// Concrete graph type used by the automaton.
    type Graph: graph::DirectedGraph;

    /// Fetch a reference to the automaton's internal graph.
    fn graph(&self) -> &Self::Graph;
}
