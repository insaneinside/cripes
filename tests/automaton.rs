use std::iter::FromIterator;

use cripes::automaton::interface::{Automaton, State as IState, Transition as ITransition};
use cripes::automaton::{Input, Action, State, Transition, DFA, BasicTypesConfig, TypesConfig};
use cripes::pattern::{Element, Sequence};

#[derive(Copy, Clone, Debug, PartialEq)]
struct Conf;
impl BasicTypesConfig for Conf {
    type Atom = char;
    type MatchId = String;
    type Expr = ();
    type Action = Action<Self>;
}

impl TypesConfig for Conf {
    type Input = Input<Self::Atom>;
    type Transition = Transition<Self>;
    type State = State<Self>;
}

#[test]
fn simple_sequence() {
    let seq = Sequence::from_iter((&['a', 'b', 'c']).iter().cloned().map(Into::<Element<char>>::into));
    let dfa= DFA::<Conf>::from(Element::Sequence(seq));
    assert_eq!(4, dfa.state_count());
    assert_eq!(3, dfa.transition_count());
    assert_eq!(1, dfa.state_ids().filter(|id| dfa.state(*id).is_accept()).count());

    let mut sid = dfa.initial_state();
    let mut inputs = vec![];
    while ! dfa.state(sid).is_accept() {
        assert_eq!(1, dfa.state_transitions(sid).count());
        let (tid, next_sid) = dfa.state_transitions(sid).next().unwrap();
        sid = next_sid;
        inputs.push(dfa.transition(tid).input().clone());
    }
    assert_eq!(inputs, &['a', 'b', 'c']);
}
