//! Interfaces and algorithms for analysis of NFA and DFA graphs.
use std::{fmt,mem};
use util::graph::{Graph, Node, WeightedEdge};
use super::{Atom, Edge as PatEdge, Transition};

/// Any analysis that can be applied to individual nodes.
pub trait NodeAnalysis<G: Graph> {
    /// Apply the analysis on a particular node, returning the result.
    fn apply(g: &G, n: <G as Graph>::NodeId) -> Self;
}

/// Classification of a node's connectivity in either the incoming or
/// outgoing direction.
#[derive(Debug,Eq,PartialEq)]
#[repr(u8)]
pub enum FlowType {
    /// Node has no edges for this direction.
    NIL = 0,

    /// Node has a single edge for this direction.
    SEQ = 1,

    /// Node has multiple edges for this direction.
    MULTI = 2
}

impl FlowType {
    /// Get the flow type corresponding to the number of outgoing or incoming
    /// edges on a node.
    pub fn for_edge_count(n: usize) -> Self {
        match n {
            0 => FlowType::NIL,
            1 => FlowType::SEQ,
            _ => FlowType::MULTI
        }
    }
}    


/// Structural control-flow analysis for a node. 
///
/// FlowStructure describes a node in terms the number of incoming and outgoing
/// edges (input-transitions) attached to it.
pub struct FlowStructure(u8);

impl FlowStructure {

    /// Create a FlowStructure for the given incoming and outgoing flow types.
    pub fn new(inc: FlowType, out: FlowType) -> Self {
        FlowStructure(((inc as u8) << 4) | out as u8)
    }

    /// Create a FlowStructure for the given incoming and outgoing edge counts.
    pub fn for_edge_counts(inc: usize, out: usize) -> Self {
        Self::new(FlowType::for_edge_count(inc), FlowType::for_edge_count(out))
    }

    /// Get the structure of the incoming side of the node.
    #[inline]
    pub fn incoming(&self) -> FlowType {
        unsafe { mem::transmute((self.0 & 0xF0) >> 4) }
    }

    /// Get the structure of the outgoing side of the node.
    #[inline]
    pub fn outgoing(&self) -> FlowType {
        unsafe { mem::transmute(self.0 & 0x0F) }
    }
}

impl fmt::Debug for FlowStructure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let inc = self.incoming();
        let out = self.outgoing();
        if inc == out { <FlowType as fmt::Debug>::fmt(&inc, f) }
        else { write!(f, "{:?}/{:?}", inc, out) }
    }
}


/// Determine if the specified edge ID refers to a "repeat" transition (i.e.,
/// a back-edge).
fn is_repeat_transition<T, G>(g: &G, e: <G as Graph>::EdgeId) -> bool
    where T: Atom, G: Graph<Edge=WeightedEdge<PatEdge<T>,<G as Graph>::NodeId>> {
    match (*g.edge(e)).input {
        Transition::Repeat{..} => true,
        _ => false
    }
}

impl<T, G> NodeAnalysis<G> for FlowStructure
where T: Atom, G: Graph<Edge=WeightedEdge<PatEdge<T>,<G as Graph>::NodeId>> {
    fn apply(g: &G, n: <G as Graph>::NodeId) -> Self {
        let n = g.node(n);

        FlowStructure::for_edge_counts(n.incoming_edges().filter(|eid| ! is_repeat_transition(g, **eid)).count(),
                                       n.outgoing_edges().filter(|eid| ! is_repeat_transition(g, **eid)).count())
    }
}

/*struct SequenceCombiner;

impl<T: Atom> SequentialPairsAnalysis<T> for SequenceCombiner {
    fn apply(g: Graph<T>, left: EdgeID, right: EdgeID) -> Option<Modification<T>> {
    }
}*/

//struct RedundantClosureAnalysis<T: Atom> { _data: PhantomData<T> }

/*impl<T: Atom> SequentialPairsAnalysis<T> for RedundantClosureAnalysis<T> {
    fn apply(g: Graph<T>, left: EdgeID, right: EdgeID) -> Option<Modification<T>> {
        let (edge_a, edge_b) = (g.edge_weight(left), g.edge_weight(right));
        if edge_a.is_none() || edge_b.is_none() { None }
        else {
            match (&edge_a.unwrap().pattern, &edge_b.unwrap().pattern) {
                (&Transition::Repeat{pattern: a, min: min_a, max: max_a},
                 &Transition::Repeat{pattern: b, min: minB, max: max_b}) => { None },
                _ => None
            }
        }
    }
}
*/
