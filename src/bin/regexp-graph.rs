#![cfg(all(feature = "dot", feature = "regex"))]

#![feature(try_from)]
extern crate dot;
extern crate cripes;
extern crate itertools;
extern crate regex_syntax;

use std::convert::TryFrom;
use std::fmt::Display;
use std::io::{self, Write};

use cripes::util::graph::{Graph, DirectedGraph, ConcreteGraph, Id, WeightedNode, WeightedEdge};
use cripes::pattern::{Element, ByteOrChar};
use cripes::automaton::{State, DFA, NFA};


struct G<'a,T>(&'a T) where T: 'a + DirectedGraph;


impl<'a,T: DirectedGraph> dot::GraphWalk<'a,T::NodeId,T::EdgeId> for G<'a,T> {
    fn nodes(&self) -> dot::Nodes<'a,T::NodeId> { self.0.node_ids().collect() }
    fn edges(&self) -> dot::Edges<'a,T::EdgeId> { self.0.edge_ids().collect() }
    fn source(&self, e: &T::EdgeId) -> T::NodeId {
        self.0.edge_source(*e)
    }
    fn target(&self, e: &T::EdgeId) -> T::NodeId {
        self.0.edge_target(*e)
    }
}

impl<'a,T,W> dot::Labeller<'a,T::NodeId,T::EdgeId> for G<'a,T>
    where T: ConcreteGraph<Node=WeightedNode<State,<T as Graph>::EdgeId>,
                           Edge=WeightedEdge<W,<T as Graph>::NodeId>>,
          W: Display + Clone
{
    fn graph_id(&'a self) -> dot::Id<'a> { dot::Id::new("G0".to_owned()).unwrap() }
    fn node_id(&'a self, n: &T::NodeId) -> dot::Id<'a> { dot::Id::new(format!("N{}", n.index())).unwrap() }
    fn edge_label(&'a self, e: &T::EdgeId) -> dot::LabelText<'a> {
        let edge = &*self.0.edge(*e);
        dot::LabelText::LabelStr(format!("{}", edge.data()).into())
    }
    /*fn edge_style(&'a self, e: &EdgeId) -> dot::Style {
        /*match self.graph.edge(*e).input {
            Transition::Repeat{..} => dot::Style::Dashed,
            _ => */dot::Style::Solid
        // }
    }*/
    fn node_label(&'a self, n: &T::NodeId) -> dot::LabelText<'a> {
        let node = &*self.0.node(*n);
        if node.actions().is_empty() {
            dot::LabelText::LabelStr(format!("{}", n.index()).into())
        } else {
            dot::LabelText::LabelStr(format!("{}", n.index()).into())
                                 //<FlowStructure as NodeAnalysis<GraphReprImpl<char>>>::apply(&self.graph, *n)).into()
        }
    }
    fn node_shape(&'a self, node: &T::NodeId) -> Option<dot::LabelText<'a>> {
        let shape: &'static str =
            if self.0.node(*node).is_accept() { "doublecircle" }
            else { "circle" };
        Some(dot::LabelText::LabelStr(String::from(shape).into()))
    }
}

fn main() {
    let mut args = std::env::args();
    let progname = args.next().unwrap();

    let mut have_args = false;
    let mut nfa_only = false;
    let mut dump_structure = false;

    while let Some(arg) = args.next() {
        have_args = true;
        match arg.as_str() {
            "-s" => dump_structure = true,
            "-n" => nfa_only = true,
            "-d" => nfa_only = false,
            s => {
                match regex_syntax::Expr::parse(s) {
                    Ok(expr) => match Element::<ByteOrChar>::try_from(expr) {
                        Ok(structural_pattern) => {
                            if dump_structure {
                                println!("Pattern: {:?}", structural_pattern);
                            } else {
                                let nfa = NFA::from(structural_pattern);
                                if nfa_only {
                                    dot::render(&G(nfa.graph()), &mut io::stdout()).unwrap();
                                } else {
                                    let dfa = DFA::from(nfa);
                                    dot::render(&G(dfa.graph()), &mut io::stdout()).unwrap();
                                }
                            } },
                        Err(err) => {writeln!(io::stderr(), "{}", err).unwrap(); } },
                    Err(err)
                        => {writeln!(io::stderr(), "{}", err).unwrap(); },
                }
                break; },
        }
    }
    if ! have_args {
        writeln!(io::stderr(), "Usage: {} REGEXP", progname).unwrap();
    }
}
