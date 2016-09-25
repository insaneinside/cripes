//! Utilities for graph transformations.

use std::cell::{Ref, RefCell, RefMut};
use std::default::Default;
use super::interface::{Graph, ConcreteGraphMut, Id};
use util::splitvec::SplitVec;

// ----------------------------------------------------------------

/// Target-node specifier for use with `Builder::append_edge`.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Target<I> where I: Id {
    /// Use the specified target node, and ensure that it is in the outputs
    /// list
    Output(I),

    /// Use all current output nodes, creating one if none exist
    AllOutputs,

    /// Use the first available output node if one exists, or create one and
    /// add it to the outputs list
    AutoOutput,

    /// Unconditionally create a new node, and add it to the outputs list
    NewOutput,

    /// Use the specified target node, but do not add it to the outputs list
    Intermediate(I),

    /// Create a new target node, but do not add it to the outputs list
    AutoIntermediate,

    /// Use each input node as the output.
    InputOutput
}

impl<I> Target<I> where I: Id {
    /// Check if the target specifies a node that should be added to the
    /// Builder's outputs list.
    pub fn is_output(&self) -> bool {
        match self {
            &Target::Output(_) |
            &Target::AutoOutput |
            &Target::NewOutput |
            &Target::InputOutput |
            &Target::AllOutputs
                => true,
            &Target::Intermediate(_) |
            &Target::AutoIntermediate
                => false
        }
    }

    /// Resolve the target into a node id, potentially creating a new node
    ///
    /// `auto_output_candidate` is the calling Builder's first output (or None if
    /// no outputs currently exist).
    ///
    /// `f` is called when a new node should be created, and must return that
    /// node's ID.
    pub fn node_id<'a, F>(&self, auto_output_candidate: Option<I>, mut f: F) -> I
        where F: FnMut() -> I
    {
        match self {
            &Target::AutoOutput
                => auto_output_candidate.unwrap_or_else(|| f()),
            &Target::Output(id) |
            &Target::Intermediate(id)
                => id,
            &Target::NewOutput |
            &Target::AutoIntermediate
                => f(),
            &Target::AllOutputs
                => panic!("cannot handle `Target::AllOutputs` variant from `node_id`"),
            &Target::InputOutput
                => panic!("cannot handle `Target::InputOutput` variant from `node_id`")
        }
    }

    /// Fetch the ID contained in an `Output` or `Intermediate` variant.
    pub fn unwrap_id(&self) -> I {
        match self {
            &Target::Output(id) | &Target::Intermediate(id) => id,
            _ => panic!("Attempted to unwrap Target instance without a specific id")
        }
    }
}

// ----------------------------------------------------------------


/// Trait describing bounds on graph-construction functions and closures passed
/// to `Builder`'s methods.
pub trait BuildFn<G, I>: for<'a> Fn(&'a mut Builder<G>, Target<G::NodeId>, I) -> G::NodeId
    where G: Graph
{}

impl<G, I, F> BuildFn<G, I> for F
    where F: for<'a> Fn(&'a mut Builder<G>, Target<G::NodeId>, I) -> G::NodeId,
          G: Graph
{}

/// Closure type used when a new node must be created on a graph
pub trait NodeFn<G, E>: Fn(&mut G, &E) -> G::NodeId
    where G: Graph
{}

impl<G, E, F> NodeFn<G, E> for F
    where F: Fn(&mut G, &E) -> G::NodeId,
          G: Graph
{}

// ----------------------------------------------------------------

/// Handles node and edge addition for each stage of a graph
/// under construction.
#[derive(Clone, Debug)]
pub struct Builder<G>
    where G: ConcreteGraphMut
{
    graph: RefCell<G>,

    /// "out" nodes from the previous stage
    pub inputs: SplitVec<G::NodeId>,

    /// "out" nodes  for the stage under construction
    pub outputs: SplitVec<G::NodeId>,
}

impl<G> Builder<G> where G: ConcreteGraphMut
{
    /// Create an empty `Builder` instance for the given entry node
    pub fn new(g: G, entry: G::NodeId) -> Self {
        Builder{graph: RefCell::new(g),
                inputs: vec![entry].into(),
                outputs: SplitVec::new()}
    }

    /// Consume the Builder object, returning the built graph.
    pub fn finish(self) -> G {
        self.graph.into_inner()
    }

    /// Get a reference to the builder's graph
    pub fn graph(&self) -> Ref<G> {
        self.graph.borrow()
    }

    /// Get a reference to the builder's graph
    pub fn graph_mut(&mut self) -> &mut G {
        self.graph.get_mut()
    }

    /// Fetch a slice over the IDs of nodes that act as inputs for this stage
    pub fn stage_inputs(&self) -> &[G::NodeId] {
        &self.inputs[..]
    }

    /// Fetch a slice over the IDs of nodes that act as outputs for this stage
    pub fn stage_outputs(&self) -> &[G::NodeId] {
        &self.outputs[..]
    }

    /// Run the given function or closure, providing mutable access to the
    /// underlying graph.
    pub fn with_graph_mut<F>(&mut self, f: F)
        where F: FnOnce(&Self, RefMut<G>) {
        f(self, self.graph.borrow_mut());
    }

    /// Append an edge between all inputs for the stage and some target node.
    ///
    ///
    pub fn append_edge<E, F>(&mut self, tgt: Target<G::NodeId>, e: E, f: F) -> G::NodeId
        where F: NodeFn<G, E>,
              (G::NodeId, G::NodeId, E): Into<G::Edge>,
              E: Clone
    {
        match tgt {
            Target::InputOutput
                => {
                    for prev in self.inputs.iter() {
                        self.graph.get_mut().add_edge((*prev, *prev, e.clone()));
                    }
                    *self.inputs.first().unwrap()
                },
            Target::AllOutputs
                => {
                    // If the stage has no outputs, create one.
                    if self.outputs.is_empty() {
                        self.outputs.push(f(self.graph.get_mut(), &e));
                    }

                    for next in self.outputs.iter() {
                        for prev in self.inputs.iter() {
                            self.graph.get_mut().add_edge((*prev, *next, e.clone()));
                        }
                    }
                    *self.outputs.first().unwrap()
                },
            _ => {
                let next = tgt.node_id(self.outputs.first().cloned(), || f(self.graph.get_mut(), &e));
                if tgt.is_output() {
                    self.mark_output(next);
                }

                // For ALL inputs from the previous stage, add an edge from that input
                // to the specified output.
                for prev in self.inputs.iter() {
                    self.graph.get_mut().add_edge((*prev, next, e.clone()));
                }
                next
            }
        }
    }

    /// Add an edge from a *specific* source node to some target node
    pub fn append_edge_from<E, F>(&mut self,
                                  source: G::NodeId,
                                  target: Target<G::NodeId>,
                                  e: E,
                                  f: F) -> G::NodeId
        where F: NodeFn<G, E>,
              (G::NodeId, G::NodeId, E): Into<G::Edge>,
              E: Clone
    {
        let next = target.node_id(self.outputs.first().cloned(), || f(self.graph.get_mut(), &e));
        if target.is_output() {
            self.mark_output(next);
        }
        self.graph.get_mut().add_edge((source, next, e));
        next
    }

    /// Ensure that a particular node id is included in the current
    /// stage's outputs.
    pub fn mark_output(&mut self, id: G::NodeId) -> G::NodeId {
        if ! self.outputs.contains(&id) {
            self.outputs.push(id);
        }
        id
    }

    /// Mark all stage inputs as outputs.
    pub fn mark_inputs_as_outputs(&mut self) {
        for id in self.inputs.iter_mut() {
            self.outputs.push(*id)
        }
    }

    /// Build _within_ the current stage, preserving the list of stage inputs.
    pub fn recurse<F, I>(&mut self, next: Target<G::NodeId>, input: I, build: F) -> G::NodeId
        where F: for<'a> FnOnce(&'a mut Builder<G>, Target<G::NodeId>, I) -> G::NodeId
    {
        self.inputs.dedup();

        self.inputs.push_and_copy_state();

        let o = build(self, next, input);

        self.inputs.pop_state();

        o
    }


    /// Chain together the subgraphs built from the elements produced by
    /// an iterator.
    ///
    /// If the given target specifies a node, that node will be used as the
    /// terminal end of the subgraph chain in addition to being marked as
    /// a stage output.
    ///
    /// This method will **panic** if called with an empty iterator.
    pub fn chain<F, I, R>(&mut self, target: Target<G::NodeId>, input: R, build: F) -> G::NodeId
        where R: IntoIterator<Item=I>,
              F: for<'a> Fn(&'a mut Builder<G>, Target<G::NodeId>, I) -> G::NodeId
    {
        let mut o = None;
        let mut iter = input.into_iter().peekable();
        let build = &build;

        self.inputs.push_and_copy_state();
        self.outputs.push_state();
        while let Some(elt) = iter.next() {
            if iter.peek().is_some() {
                // If there's another element after this one, create a pristine
                // node to connect it to the one we're about to build
                o = Some(build(self, Target::NewOutput, elt));
                self.advance();
            } else {
                // Last element; connect to the specified target.
                match target {
                    Target::InputOutput
                        => {
                            // When `InputOutput` was specified, we need to
                            // copy the *chain* inputs -- i.e., the inputs to
                            // the _first_ stage in the chain -- to the current
                            // outputs list.
                            self.outputs.truncate(0);
                            self.outputs.extend_from_slice(self.inputs.prev_state().unwrap());
                            o = Some(build(self, Target::AllOutputs, elt));
                            self.outputs.pop_state();
                            self.outputs.extend_from_slice(self.inputs.prev_state().unwrap());
                        },
                    _ => {
                        self.outputs.pop_state();
                        o = Some(build(self, target, elt));
                    }
                }
            }
        }
        self.inputs.pop_state();
        o.unwrap()
    }

    /// Build subgraphs from the elements of an iterator between the current
    /// stage's inputs and some specified output.
    pub fn branch<F, I, R>(&mut self, target: Target<G::NodeId>, input: R, build: F) -> G::NodeId
        where R: IntoIterator<Item=I>,
              F: for<'a> Fn(&'a mut Builder<G>, Target<G::NodeId>, I) -> G::NodeId//BuildFn<G, I>
    {
        let build = &build;
        let mut target = target;
        for elt in input {
            target = Target::Output(self.recurse(target, elt.into(), build));
        }
        target.unwrap_id()
    }


    /// Finalize the current stage.
    ///
    /// This overwrites the current list of input nodes with the list of output
    /// nodes, and truncates the output-node list.
    pub fn advance(&mut self) {
        if self.outputs.len() > 0 {
            self.inputs.truncate(0);
            self.inputs.extend_from_slice(&self.outputs[..]);
            self.outputs.truncate(0);
        }
    }
}

// ----------------------------------------------------------------

/// Graph-building interface.
pub trait Build<G: ConcreteGraphMut> {
    /// Type from which the graph is built.
    type Input;

    /// Create the entry node for the given input, returning its ID.
    fn entry_node(g: &mut G, input: &Self::Input) -> G::NodeId;

    /// Create the target node for the given edge data, returning the ID of the
    /// new node.
    fn target_node<E>(g: &mut G, e: &E) -> G::NodeId
        where (G::NodeId, G::NodeId, E): Into<G::Edge>;



    /// Perform any finalization on the builder or the built graph.
    #[inline]
    fn finish(_: &mut Builder<G>) {
    }

    /// Recursively build the graph for the given input data.
    ///
    /// @param b Builder instance to use for building the graph
    ///
    /// @param next Output to which the stage built from this input data should
    /// connect
    ///
    /// @param input
    fn build_recursive(b: &mut Builder<G>,
                       next: Target<G::NodeId>,
                       input: Self::Input) -> G::NodeId;

    // /// Create a subgraph describing the given input, and return its
    // /// entry and exit node IDs.
    // ///
    // /// @return Tuple containing the stand-alone graph's entry- and
    // /// exit-node IDs.
    // fn build_subgraph<'g>(ep: &mut Builder<'g, G>,
    //                       input: Self::Input) -> (G::NodeId,
    //                                               G::NodeId) {
    //     Self::build_recursive(ep, input);
    // }


    /// Convert the given input into a graph.
    ///
    /// Returns the resulting graph and the entry node.
    fn build(input: Self::Input) -> (G, G::NodeId)
        where G: Default {
        let mut g = G::new();
        let entry = Self::entry_node(&mut g, &input);

        let mut builder = Builder::new(g, entry);
        Self::build_recursive(&mut builder, Target::AutoOutput, input);
        Self::finish(&mut builder);
        (builder.finish(), entry)
    }
}
