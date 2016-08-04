//! Graph-based representations of finite automatons.
//!
//! [`GraphRepr<T>`](struct.GraphRepr.html) explicitly stores *FA
//! transitions and states as edge and node weights, respectively, of
//! a graph.  This type is best suited for code-generation and other
//! read-only uses where you want to focus on specific parser states.

use std::iter;
use std::slice;
use std::ops::Range;
use std::marker::PhantomData;
#[cfg(feature="debug")]
use std::io::{self, Write};
use std::fmt::{self, Debug, Display};
use std::collections::HashMap;

use itertools::Itertools;
use bit_set::BitSet;

#[cfg(feature="regex")]
use regex_syntax;

use pattern::{self, Action, Atom, Element, RepeatCount};
use util::graph::interface::*;
use util::graph::{self,Build, Builder, Target, WeightedNode};//, Graph as Graphlike, DirectedGraph, Id as GraphID};

/// Trait bounds for any type used as a transition in an automaton
pub trait Transition: Clone + Eq + Ord {}
impl<T> Transition for T where T: Clone + Eq + Ord {}

//pub mod analysis;
//use self::analysis::{FlowStructure,NodeAnalysis};


bitflags! {
    flags StateFlags: u8 {
        const STATE_ENTRY = 1 << 0,
        const STATE_ACCEPT  = 1 << 1
    }
}

// ================================================================
// States
/** A node within a pattern's flow graph.

    States exist immediately before and after a token has been consumed; they
    associate arbitrary actions with parser states.
 */
#[derive(Clone,Debug)]
pub struct State {
    flags: StateFlags,

    /// Actions that should be performed when the parser reaches this state.
    actions: Vec<Action>

}

impl State {
    /** Create a new parse-point.
     */
    pub fn new() -> Self {
        State{flags: StateFlags::empty(), actions: Vec::new()}
    }

    /// Check if this is an accepting state
    pub fn is_accept(&self) -> bool {
        self.flags.contains(STATE_ACCEPT)
    }

    /// Set whether this is an accepting state
    pub fn set_accept(&mut self, accept: bool) {
        if accept { self.flags.insert(STATE_ACCEPT); }
        else { self.flags.remove(STATE_ACCEPT); }
    }
            

    /// Check if this is an entry state
    pub fn is_entry(&self) -> bool {
        self.flags.contains(STATE_ENTRY)
    }

    /// Add an action to be performed when the parser reaches this state.
    #[inline(always)]
    pub fn add_action(&mut self, a: Action) {
        self.actions.push(a);
    }

    /// Fetch the current list of assigned actions
    #[inline(always)]
    pub fn actions(&self) -> &[Action] {
        self.actions.as_slice()
    }
}

impl Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <Self as Debug>::fmt(self, f)
    }
}


// ================================================================
// Graphs

/// Interface for automaton implementations
pub trait Automaton<'a, T: 'a + Transition> {
    /// Type used to identify individual states.
    type StateId: Id;

    /// Concrete type used to represent automaton states
    type State;

    /// Iterator over the IDs of states in the automaton.
    type StateIdsIter: Iterator<Item=Self::StateId>;

    /// Iterator over the possible inputs for a state.
    type InputsIter: Iterator<Item=&'a T>;

    /// Iterator over the possible outgoing transitions for a state.
    type TransitionsIter: Iterator<Item=(&'a T, Self::StateId)>;

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
pub trait DeterministicAutomaton<'a, T: 'a + Transition>: Automaton<'a, T> {
    /// Determine the next state to which the DFA will transition given its
    /// current state and the input token.
    fn next_state(&self, current_state: Self::StateId, input: T) -> Option<Self::StateId>;
}

/// Interface for non-deterministic automatons.
pub trait NondeterministicAutomaton<'a,T: 'a + Transition>: Automaton<'a, T> {
    /// Iterator type returned by `next_states`.
    type NextStatesIter: Iterator<Item=Self::StateId>;

    /// Fetch an iterator over the set of states to which the automaton
    /// transitions on the given input.
    fn next_states(&'a self, current_state: Self::StateId, input: T) -> Self::NextStatesIter;
}

/// Wrapped node index.
type NodeId = graph::NodeIndex<u32>;

/// Wrapped edge index.
type EdgeId = graph::EdgeIndex<u32>;

/// Graph type used for patterns.
pub type GraphImpl<T> = graph::WeightedGraph<State,T>;


// ================================================================

/// Graph-based representation of automata patterns.
#[derive(Clone)]
struct GraphRepr<T: Transition> {
    /// Graph representation of the control-flow of the pattern's automaton.
    graph: GraphImpl<T>,

    /// Index of the node that serves as the main entry point for the graph.
    entry: <GraphImpl<T> as Graph>::NodeId

    // Stored node analyses
    //node_analyses: HashMap<(TypeId,NodeId),Box<CloneAny>>,

    // Stored edge analyses
    //edge_analyses: HashMap<(TypeId,EdgeId),Box<CloneAny>>
}


/// Iterator over a state's inputs (outgoing-edge values)
pub struct InputsIter<'a, T: 'a + Transition> {
    graph: &'a GraphImpl<T>,
    edges: slice::Iter<'a,<GraphImpl<T> as Graph>::EdgeId>
}

impl<'a, T> Iterator for InputsIter<'a, T>
    where T: 'a + Transition
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.edges.next() {
            Some(eid) => Some(&*self.graph[*eid]),
            None => None,
        }
    }
}

/// Iterator over a state's inputs and corresponding next-state IDs
pub struct TransitionsIter<'a, T: 'a + Transition> {
    graph: &'a GraphImpl<T>,
    edges: slice::Iter<'a,<GraphImpl<T> as Graph>::EdgeId>
}

impl<'a, T> Iterator for TransitionsIter<'a, T>
    where T: 'a + Transition
{
    type Item = (&'a T, <NFA<T> as Automaton<'a,T>>::StateId);

    fn next(&mut self) -> Option<Self::Item> {
        match self.edges.next() {
            Some(eid)
                => { let next_state = self.graph.edge_target(*eid);
                     Some((&*self.graph[*eid], next_state)) },
            None => None
        }
    }
}

/// Iterator over the IDs of states reachable from specific
/// state-and-input pair.
pub struct NextStatesIter<'a, T: 'a + Transition> {
    graph: &'a GraphImpl<T>,
    edges: slice::Iter<'a,<GraphImpl<T> as Graph>::EdgeId>,
    transition: T
}

impl<'a, T> Iterator for NextStatesIter<'a, T>
    where T: 'a + Transition
{
    type Item = <NFA<T> as Automaton<'a,T>>::StateId;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.edges.next() {
                Some(eid) if *self.graph[*eid] == self.transition => return Some(self.graph.edge_target(*eid)),
                Some(_) => continue,
                None => return None
            }
        }
    }
}

/// Iterator over the IDs of states in an automaton.
pub struct StateIdsIter<'a,T: 'a + Transition> {
    range: Range<usize>,
    id: PhantomData<&'a <NFA<T> as Automaton<'a,T>>::StateId>
}

impl<'a, T> Iterator for StateIdsIter<'a,T>
    where T: 'a + Transition
{
    type Item = <NFA<T> as Automaton<'a,T>>::StateId;

    fn next(&mut self) -> Option<Self::Item> {
        self.range.next().map(|x| x.into())
    }
}



// ----------------------------------------------------------------


macro_rules! impl_automaton {
    ($Tp: ty, $N: ident, $T: ident) => {
        impl<'a,$T: 'a + Transition> Automaton<'a,$T> for $Tp {
            type State = State;
            type StateId = <GraphImpl<$T> as Graph>::NodeId;
            type StateIdsIter = StateIdsIter<'a, T>;
            type InputsIter = InputsIter<'a,$T>;
            type TransitionsIter = TransitionsIter<'a,$T>;

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
        }
    }
}
macro_rules! impl_nondeterministic_automaton {
    ($Tp: ty, $N: ident, $T: ident) => {
        impl<'a,$T: 'a + Transition> NondeterministicAutomaton<'a,$T> for $Tp {
            type NextStatesIter = NextStatesIter<'a,$T>;
            #[inline]
            fn next_states(&'a self, state: Self::StateId, input: $T) -> Self::NextStatesIter {
                NextStatesIter{graph: &self.0.graph, edges: self.0.graph.outgoing_edges(state), transition: input}
            }
        }

        impl<A: Atom> From<Element<A>> for $N<pattern::Transition<A>> {
            fn from(elt: Element<A>) -> Self {
                let (g, e) = GraphRepr::build(elt);
                $N(GraphRepr{graph: g, entry: e})
            }
        }

        #[cfg(feature="regex")]
        impl From<regex_syntax::Expr> for $N<pattern::Transition<char>> {
            fn from(e: regex_syntax::Expr) -> Self { <Self as From<Element<_>>>::from(e.into()) }
        }
    }
}


/// Here we implement `graph::transform::Build` for a `GraphRepr` that uses 
impl<A: Atom> Build<GraphImpl<pattern::Transition<A>>> for GraphRepr<pattern::Transition<A>> {
    type Input = Element<A>;

    fn entry_node(g: &mut GraphImpl<pattern::Transition<A>>, _: &Self::Input)  -> <GraphImpl<pattern::Transition<A>> as Graph>::NodeId {
        g.add_node(State::new())
    }
    fn target_node<E>(g: &mut GraphImpl<pattern::Transition<A>>, _: &E) -> <GraphImpl<pattern::Transition<A>> as Graph>::NodeId {
        g.add_node(State::new())
    }

    fn finish(b: &mut Builder<GraphImpl<pattern::Transition<A>>>) {
        b.with_graph_mut(|b, mut g| {
            for id in b.stage_outputs().iter() {
                g[*id].set_accept(true);
            }
        });
    }

    fn build_recursive(b: &mut Builder<GraphImpl<pattern::Transition<A>>>,
                       next: Target<<GraphImpl<pattern::Transition<A>> as Graph>::NodeId>,
                       input: Self::Input) -> <GraphImpl<pattern::Transition<A>> as Graph>::NodeId {
        #[cfg(feature="debug")] {
            let input_fmt = format!("{:?}", input);
            write!(io::stderr(), concat!("[32mBefore[0m [1;97m{}[0m\n",
                                         "     next: {:?}\n",
                                         "   inputs: {:?}\n",
                                         "  outputs: {:?}\n"),
                   input_fmt, next, b.inputs, b.outputs).unwrap();
        }
        
        let o = match input {
            Element::Tagged{element, name} => {
                b.with_graph_mut(|b, mut g| {
                    for id in b.stage_inputs().iter() {
                        g[id].add_action(Action::BeginSubmatch(name.clone()));
                    }
                });

                let o = Self::build_recursive(b, next, *element);

                b.with_graph_mut(|b, mut g| {
                    for id in b.stage_outputs().iter() {
                        g[id].add_action(Action::EndSubmatch(name.clone()));
                    }
                });
                o
            },

            Element::Atomic(transition)
                => b.append_edge(next, transition, Self::target_node),
            Element::Sequence(elts)
                => b.chain(next, elts.iter().map(|x| x.clone().into()), &Self::build_recursive),
            Element::Union(elts)
                => b.branch(next, elts.iter().map(|x| x.clone().into()), &Self::build_recursive),

            Element::Repeat{element, count}
            => match count {
                RepeatCount::Any
                    => {
                        // Unbounded repetition; this is the famous
                        // Kleene star.

                        // As we'll see below, additional structure is required
                        // when any of the inputs has outgoing transitions
                        // to itself.
                        let have_input_loops
                            = {
                                let g = b.graph();

                                b.stage_inputs().iter()
                                    .flat_map(|id| iter::repeat(*id).zip(g.direct_successors(*id)))
                                    .any(|(id, target_id)| id == target_id)
                            };

                        if have_input_loops {
                            // If any input *already* has a transition that
                            // targets itself, we need to
                            //
                            //   (1) mark all input nodes as outputs to handle
                            //       the zero-occurrences case;
                            b.mark_inputs_as_outputs();

                            // [This recurse call prevents us from munging
                            // a parent stage's inputs and outputs list when we
                            // call `advance`.]
                            b.recurse(next, (*element).clone(), |b, _, input| {
                                //   (2) create to a new output node to "lock
                                //       in" the element being repeated and
                                //       prevent following e.g. a preceding
                                //       unbounded repetition once we've
                                //       started this one; and finally
                                Self::build_recursive(b, Target::NewOutput, input.clone());
                                b.advance();
                                Self::build_recursive(b, Target::InputOutput, input)
                            })
                        } else {
                            // Without input loops, we simply build the
                            // repeated element as a transition back to the
                            // starting point (this also has the effect of
                            // marking all inputs as outputs).
                            Self::build_recursive(b, Target::InputOutput, *element)
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
                    => b.recurse(next, *element, |b, tgt, input| {
                        b.chain(tgt, iter::repeat(input.clone()).take(n), &Self::build_recursive);
                        b.advance();
                        let o = Self::build_recursive(b, Target::InputOutput, input);
                        b.mark_output(o);
                        o
                    }),

                // When the repetition count is bounded at the upper end (let's
                // call that limit N), we simply build the repeated element's
                // subgraph N times in a chain, marking the chain's
                // intermediary nodes as stage outputs .
                RepeatCount::AtMost(n)
                    => b.recurse(next, *element, |b, tgt, input| {
                        let mut intermediates = Vec::from(b.stage_inputs());

                        let mut o = <GraphImpl<pattern::Transition<A>> as Graph>::NodeId::new(13);
                        for _ in 0..n {
                            o = Self::build_recursive(b, tgt, input.clone());
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
                    => b.chain(next, iter::repeat(*element).take(n), &Self::build_recursive),

                // Ranges (where the minimum is greater than zero and the
                // maximum is finite) are handled as exact repetitions followed
                // by RepeatCount::At
                RepeatCount::Between(n, m)
                    => b.recurse(next, (*element).clone(), |b, tgt, input| {
                        b.chain(tgt, iter::repeat(input).take(n), &Self::build_recursive);
                        b.advance();
                        b.recurse(tgt, Element::Repeat{element: element, count: RepeatCount::AtMost(m - n)}.into(),
                                  &Self::build_recursive)
                    })
            }
        };

        #[cfg(feature="debug")]
        write!(io::stderr(), concat!("[31mAfter[0m [1;97m{}[0m\n",
                                     "   inputs: {:?}\n",
                                     "  outputs: {:?}\n\n"),
               input_fmt, b.inputs, b.outputs).unwrap();
        
        o
    }
}

/// Graph-based nondeterministic finite automaton.
///
/// NFAs are the result of transforming
/// a [`Pattern`](../pattern/type.Pattern.html) into a graph.
pub struct NFA<T: Transition>(GraphRepr<T>);
impl_automaton!(NFA<T>, NFA, T);
impl_nondeterministic_automaton!(NFA<T>, NFA, T);

impl<T: Transition> NFA<T> {
    /// Fetch a reference to the graph used to represent the NFA.
    #[inline]
    pub fn graph(&self) -> &GraphImpl<T> {
        &self.0.graph
    }
}

// ----------------------------------------------------------------

/// Graph-based deterministic finite automaton.
pub struct DFA<T: Transition>(GraphRepr<T>);

impl<T: Transition> DFA<T> {
    /// Fetch a reference to the graph used to represent the NFA.
    #[inline]
    pub fn graph(&self) -> &GraphImpl<T> {
        &self.0.graph
    }
}


/// Build the graph for the deterministic finite automaton that corresponds to
/// the given non-deterministic finite automaton, starting at the given set of
/// NFA states.
///
/// This implementation avoids the necessity of manually tracking "unmarked"
/// (unhandled) states in the resulting DFA by eagerly recursing whenever it
/// finds a set of NFA states _not_ already present as a key in the mapping.
fn build_dfa_recursive<'a, D, T, N, G>(n: &'a N, g: &mut G, nfa_states: &BitSet,
                                       states_map: &mut HashMap<BitSet, D::StateId>)
                                       -> D::StateId
    where T: 'a + Transition + Display + Debug,
          N: NondeterministicAutomaton<'a, T, State=State>,
          D: Automaton<'a, T>,
          G: Graph + ConcreteGraph<Node=WeightedNode<State,<G as Graph>::EdgeId>>,
          G::Node: From<State>,
          G::NodeId: Into<D::StateId>,
          G::Edge: From<(usize, usize, T)>
{
    // if the provided set of NFA states isn't already in the states map, insert it and 
    let dfa_state: D::StateId = *states_map.entry(nfa_states.clone()).or_insert_with(|| {
        let sid = g.add_node(State::new());
        if nfa_states.iter().any(|ns| n.state(ns.into()).is_accept()) {
            g.node_mut(sid).set_accept(true);
        }
        sid.into()
    });

    // for each possible input on any of the NFA states corresponding to
    // this DFA state, determine the set of NFA states we can arrive at.
    let mut target_nfa_states = BitSet::with_capacity(n.state_count());

    for (input, next_states) in
        nfa_states.into_iter()

        // convert `src: StateId...`
        // to      `(input: T, dst: StateId)...`
        .flat_map(|sid| n.state_transitions(sid.into()))

        // convert `(input: T, id: StateId)...`
        // to      `(input, Vec<(input, id)>)...`
        .sorted_by(|&(tr1, _), &(tr2, _)| tr1.cmp(tr2))
        .into_iter()
        .group_by(|&(tr, _)| tr)

        // convert `(input, Vec<(input, StateId)>)...`
        // to      `(input, Vec<StateId>)...`
        .map(|(input, transitions)| (input, transitions.into_iter().map(|(_, next_state)| next_state.index())))
    {
        // Initialize the target-states set to the set of states for this input
        target_nfa_states.clear();
        target_nfa_states.extend(next_states);

        // Determine the *DFA* target state; we'll recursively create it if it
        // doesn't exist.
        let target_dfa_state: D::StateId =
            if ! states_map.contains_key(&target_nfa_states) {
                let sid = build_dfa_recursive::<D, T, N, G>(n, g, &target_nfa_states, states_map);
                states_map.insert(target_nfa_states.clone(), sid);
                sid
            } else {
                *states_map.get(&target_nfa_states).unwrap()
            };

        // Add the edge that corresponds to this input.
        g.add_edge((dfa_state.index(), target_dfa_state.index(), input.clone()));
    }

    dfa_state
}


impl_automaton!(DFA<T>, DFA, T);

impl<'a, T> DeterministicAutomaton<'a,T> for DFA<T>
    where T: 'a + Transition {
    #[inline]
    fn next_state(&self, s: Self::StateId, input: T) -> Option<Self::StateId> {
        // With an adjacency-list graph representation, the best we can do is
        // a binary search -- IF the lists are sorted, which they're not
        // for now.
        //
        // FIXME: [optimize] replace linear with binary search
        let g = self.graph();
        g.outgoing_edges(s)
            .find(|eid| *g[*eid] == input)
            .map(|eid| g.edge_target(*eid))
    }
}


impl<'a, T> From<NFA<T>> for DFA<T>
    where T: 'a + Transition + Display + Debug
{
    fn from(n: NFA<T>) -> Self {
        // To use `build_dfa_recursive`, we need to pass in references to
        // existing values.
        let mut g = GraphImpl::new();
        let mut states_map: HashMap<BitSet,<DFA<T> as Automaton<'a,T>>::StateId> =
            HashMap::new();
        let mut nfa_states = BitSet::with_capacity(n.state_count());
        nfa_states.insert(n.initial_state().index());

        let entry =
            build_dfa_recursive::<DFA<T>,_,_,_>(&n, &mut g, &nfa_states,
                                                &mut states_map);

        DFA(GraphRepr{graph: g, entry: entry})
    }
}
