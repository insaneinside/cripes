//! Implementations of various algorithms.

use super::interface::{Id, DirectedGraph};


/// Given a graph, find its strongly-connected components using the algorithm
/// in [1](#1).
///
/// [<a name="1">1</a>]: Gabow, Harold N. (2000), "Path-based depth-first search for strong and biconnected components"
pub fn gabow_scc<G>(g: &G) -> Vec<usize>
    where G: DirectedGraph
{
    let n = g.node_count();
    let mut components = vec![0; n];
    if n > 0 {
        let mut path = Vec::with_capacity(n);
        let mut boundaries = Vec::with_capacity(n);
        let mut c = n;

        for v in g.node_ids() {
            if components[v.index()] == 0 {
                gabow_dfs(g, v, &mut c, &mut path, &mut boundaries, &mut components[..]);
            }
        }
        // Renumber the groups to start at zero.
        let max = (&components).iter().cloned().max().unwrap();
        for x in &mut components {
            *x = max - *x;
        }
    }
    components
}

 /// Recursive part of Gabow's strongly-connected components algorithm.
fn gabow_dfs<G>(g: &G, v: G::NodeId, c: &mut usize,
                path: &mut Vec<usize>,
                boundaries: &mut Vec<usize>,
                components: &mut [usize])
    where G: DirectedGraph
{
    let vi = v.index();

    // Add `v` to the DFS path
    path.push(vi);
    let ptop = path.len() - 1;
    components[vi] = ptop;
    boundaries.push(ptop);

    for w in g.direct_successors(v) {
        let wi = w.index();

        if components[wi] == 0 {
            // This successor has not yet been processed; follow it.
            gabow_dfs(g, w, c, path, boundaries, components);
        } else {
            // "contract if necessary"
            while components[wi] < *boundaries.last().unwrap() {
                boundaries.pop();
            }
        }
    }

    // Number the vertices of the next strong component.
    if components[vi] == *boundaries.last().unwrap() {
        boundaries.pop();
        while components[vi] <= path.len() {
            components[path.pop().unwrap()] = *c;
        }
        *c += 1;
    }
}


#[cfg(test)]
mod tests {
    use util::graph::*;
    use super::gabow_scc;

   #[test]
    fn test_gabow_scc() {
        // (0)-->(1)-->(2)<->(3)
        // ↑ ↙↓    ↓   ↕
        // (4)-->(5)<->(6)<--(7)

        let mut g = BasicGraph::<u8>::new();
        g.add_nodes(8);
        g.add_edges(&[(0, 1), (1, 4), (1, 5), (1, 2), (2, 3), (2, 6), (3, 2), (3, 7),
                      (4, 0), (4, 5), (5, 6), (6, 5), (7, 6), (7, 3)]);
        assert_eq!(&[0, 0, 1, 1, 0, 2, 2, 1], &gabow_scc(&g)[..]);
    }
}
