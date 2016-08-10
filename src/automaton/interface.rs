//! Basic interface traits for state-machine/automaton implementations.

use std::fmt::Debug;

use util::set;
use util::graph::interface::Id;
use pattern::Atom;

/// Trait bounds for any type used as a transition in an automaton
pub trait Transition: Clone + Debug + Eq + Ord + set::Contains<<Self as Transition>::Atom> + set::IsSubsetOf<Self> {
    /// Smallest transition unit used.
    type Atom: Atom;
}



/// Interface for automaton implementations
pub trait Automaton<'a, A: 'a + Atom> {
    /// Type used to identify individual states.
    type StateId: Id;

    /// Concrete type used to represent automaton states
    type State;

    /// Concrete type that represents possible inputs to a given state.
    type Transition: 'a + Transition<Atom=A>;

    /// Iterator over the IDs of states in the automaton.
    type StateIdsIter: Iterator<Item=Self::StateId>;

    /// Iterator over the possible inputs for a state.
    type InputsIter: Iterator<Item=&'a Self::Transition>;

    /// Iterator over the possible outgoing transitions for a state.
    type TransitionsIter: Iterator<Item=(&'a Self::Transition, Self::StateId)>;

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

    /// Fetch an iterator over the (transition, next state) pairs for
    /// a given state.
    fn state_transitions(&'a self, s: Self::StateId) -> Self::TransitionsIter;
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

