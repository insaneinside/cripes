//! Test module for `cripes::util::graph::transform`, which provides utilities
//! for incremental construction and transformation of graphs.

use std::iter::{self, Iterator};

use cripes::util::graph::{Graph, ConcreteGraph, DirectedGraph,
                          WeightedGraph};
use cripes::util::graph::transform::{Builder, Target};

type G = WeightedGraph<(),char>;

#[derive(Clone, Debug)]
enum S {
    One(char),
    Seq(char, char) 
}       
impl From<char> for S {
    fn from(c: char) -> Self {
        S::One(c)
    }
}
#[derive(Clone, Debug)]
enum E {
    Single(S),
    Fork(S, S)
}
impl From<S> for E {
    fn from(c: S) -> Self {
        E::Single(c)
    }
}
impl From<(S, S)> for E {
    fn from(c: (S, S)) -> Self {
        E::Fork(c.0, c.1)
    }
}

fn target_node<E>(g: &mut G, _: &E) -> <G as Graph>::NodeId {
    g.add_node(())
}

fn build(ep: &mut Builder<G>,
         next: Target<<G as Graph>::NodeId>,
         input: E) -> <G as Graph>::NodeId {
    match input {
        E::Fork(a,b) => {
            let next = Target::Output(ep.recurse(next, a.into(), &build));
            ep.recurse(next, b.into(), &build);
            next.unwrap_id()
        },
        E::Single(conn) => {
            match conn {
                S::One(c) => ep.append_edge(next, c, target_node),
                S::Seq(a, b) => {
                    let mid = ep.append_edge(Target::AutoIntermediate, a, target_node);
                    ep.append_edge_from(mid, next, b, target_node)
                }
            }
        }
    }
}


/// Check that the `Builder` utility behaves as appropriate
#[test]
fn test_builder() {
    let mut g = G::new();
    let entry = g.add_node(());
    let mut ep = Builder::new(g, entry);

    // upon construction, `ep` should report one input and zero outputs:
    //
    // (0)
    //  I
    panic_unless_eq!(&[entry], ep.stage_inputs());
    assert_eq!(ep.stage_outputs(), &[]);

    // after appending an edge, there should be one input and one output:
    //
    // (0)→(1)
    //  I    O
    ep.append_edge(Target::AutoOutput, 'a', target_node);

    panic_unless_eq!(2, ep.graph().node_count(), "wrong node count");
    let n1 = ep.graph().node_ids().nth(1).unwrap();

    panic_unless_eq!(&[entry], ep.stage_inputs(), "unexpected inputs for stage");
    panic_unless_eq!(&[n1], ep.stage_outputs(), "unexpected outputs for stage");

    // after advancing, there should be one input and zero outputs:
    //
    // (0)→(1)
    //       I
    ep.advance();
    panic_unless_eq!(&[n1], ep.stage_inputs(), "incorrect inputs for stage");
    panic_unless_eq!(&[], ep.stage_outputs(), "incorrect outputs for stage");
    
    // after recursing, the inputs should remain unchanged:
    //
    //    a    b    c
    // (0)→(1)→(2)→(3)
    //       I         O
    //
    // B::build calls `advance` when handling a Seq item, but because
    // this call happens within the context of `recurse` the effects of the
    // call should be local to that context.
    ep.recurse(Target::AutoOutput, E::Single(S::Seq('b', 'c')),
               |b, tgt, input| {
                   let o = build(b, tgt, input);
                   println!("stage outputs: {:?}", b.stage_outputs());
                   o
               });
    println!("stage outputs: {:?}", ep.stage_outputs());
    panic_unless_eq!(4, ep.graph().node_count(), "wrong node count");
    let n2 = ep.graph().node_ids().nth(2).unwrap();
    let n3 = ep.graph().node_ids().nth(3).unwrap();

    panic_unless_eq!(&[n1], ep.stage_inputs(),  "incorrect inputs for stage");
    panic_unless_eq!(&[n3], ep.stage_outputs(), "incorrect outputs for stage");
    panic_unless_eq!(&[n2], ep.graph().direct_successors(n1).collect::<Vec<_>>().as_slice(),
                     "incorrect successors for node 1");

    // adding a fork here will call `recurse` twice -- but we expect no
    // additional output nodes because Builder re-uses the first output node
    // when possible:
    //
    //       ╭───────╮ d
    //       ├───────┤ e
    //    a  │ b    c ↓
    // (0)→(1)→(2)→(3)
    //       I         O
    build(&mut ep, Target::AutoOutput, E::Fork('d'.into(), 'e'.into()));
    panic_unless_eq!(4, ep.graph().node_count(), "wrong node count");
    panic_unless_eq!(&[n2, n3, n3], ep.graph().direct_successors(n1).collect::<Vec<_>>().as_slice(),
                     "incorrect successors for node 1");
    panic_unless_eq!(&[n1], ep.stage_inputs(), "incorrect inputs for stage");
    panic_unless_eq!(&[n3], ep.stage_outputs(), "incorrect outputs for stage");

}


/// Check that Builder::chain works as expected.
#[test]
fn test_builder_chain() {
    let mut g = G::new();
    let entry = g.add_node(());
    let mut b = Builder::new(g, entry);

    panic_unless_eq!(&[entry], b.stage_inputs());
    panic_unless_eq!(&[], b.stage_outputs());

    b.chain(Target::AutoOutput, iter::repeat(E::Single(S::One('a'))).take(3), &build);
    panic_unless_eq!(4, b.graph().node_count(), "Wrong number of nodes after chaining a transition");
    panic_unless_eq!(3, b.graph().edge_count(), "Wrong number of edges after chaining a transition");
    let n3 = b.graph().node_ids().nth(3).unwrap();
    panic_unless_eq!(&[entry], b.stage_inputs());
    panic_unless_eq!(&[n3], b.stage_outputs());

    b.advance();

    panic_unless_eq!(&[n3], b.stage_inputs());
    panic_unless_eq!(&[], b.stage_outputs());
}
