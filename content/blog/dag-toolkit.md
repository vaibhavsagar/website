Title: An All-in-One DAG Toolkit
Date: 2017-06-10
Category: programming

I'd like to tell you about an algorithm that I'm frankly annoyed I didn't
discover earlier.

The algorithm is [Tarjan's Strongly Connected
Components](https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm)
(or SCCs) algorithm, and as the name suggests, it decomposes a directed graph
into its strongly connected components. A [directed
graph](https://en.wikipedia.org/wiki/Directed_graph) is one where the edges
have a direction associated with them, and a [strongly connected
component](https://en.wikipedia.org/wiki/Strongly_connected_component) of a
graph is a subgraph where each node can be reached from every other node, i.e.
there's a directed cycle somewhere in this subgraph.

<style>
img[src*='#center'] {
    display: block;
    margin: auto;
}
</style>

![Strongly Connected Components](https://upload.wikimedia.org/wikipedia/commons/5/5c/Scc.png#center)

So why does this matter? I don't recall ever having the desire to deeply know
the SCCs of a particular graph.

Let's look at a different problem. Given a directed graph, how do we know if it
is acyclic? The context here is that I was wondering how difficult it would be
literally draw a Git commit history as a graph and render that to a repo. Git
commits form a [directed acyclic
graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph) (DAG), which is a
directed graph without cycles, and I wanted to validate a user-drawn graph as
well as process it.

![Directed Acyclic Graph](https://upload.wikimedia.org/wikipedia/commons/f/fe/Tred-G.svg#center)

Consulting [the
oracle](https://stackoverflow.com/questions/583876/how-do-i-check-if-a-directed-graph-is-acyclic)
yielded the concept of a [topological
sort](https://en.wikipedia.org/wiki/Topological_sorting), which is where
vertices are ordered such that for all vertices _u_ and _v_, if there is an
edge from _u_ to _v_, _u_ appears earlier than _v_ in the sorted output.  The
directed edges for a Git commit graph are in the opposite direction from what
we want though, because they point from children to parents. What we really
want is a reverse topological sort. It would also be nice if I could somehow
highlight the subgraphs of an invalid graph that are responsible for it being
invalid.

![Topological Sort](https://upload.wikimedia.org/wikipedia/commons/c/c6/Topological_Ordering.svg#center)

This is where we come back to the SCCs of a graph. If there are any SCCs of
more than one vertex, the graph is not a DAG and those SCCs are the cause of
this.  Collapsing the SCCs of a directed graph to a single vertex always leads
to a DAG and this is known as the condensation of a directed graph, which I
think is a nice way to visualise the relationship between SCCs and DAGs.

![Condensation](https://upload.wikimedia.org/wikipedia/commons/2/20/Graph_Condensation.svg#center)

Alright, so we do actually want to know the SCCs of this graph, but first we
want to try to sort it topologically and reverse that order if that is
possible, otherwise we calculate the SCCs and identify the offending ones. This
seems a bit messy for graph properties that seem somewhat related. How cool
would it be if there were an algorithm that could calculate the SCCs and a
reverse topological sort for us *at the same time*?

It turns out that Tarjan's SCCs algorithm does exactly that! You'd think that
its performance might not be great because it does two things at once, but it
is linear in the number of edges and vertices, and has better constant factors
than [Kosaraju's
algorithm](https://en.wikipedia.org/wiki/Kosaraju's_algorithm), which only
computes SCCs. Here it is as pseudocode:

```
 algorithm tarjan is
  input: graph G = (V, E)
  output: set of strongly connected components (sets of vertices)

  index := 0
  S := empty array
  for each v in V do
    if (v.index is undefined) then
      strongconnect(v)
    end if
  end for

  function strongconnect(v)
    // Set the depth index for v to the smallest unused index
    v.index := index
    v.lowlink := index
    index := index + 1
    S.push(v)
    v.onStack := true

    // Consider successors of v
    for each (v, w) in E do
      if (w.index is undefined) then
        // Successor w has not yet been visited; recurse on it
        strongconnect(w)
        v.lowlink  := min(v.lowlink, w.lowlink)
      else if (w.onStack) then
        // Successor w is in stack S and hence in the current SCC
        // Note: The next line may look odd - but is correct.
        // It says w.index not w.lowlink; that is deliberate and from the original paper
        v.lowlink  := min(v.lowlink, w.index)
      end if
    end for

    // If v is a root node, pop the stack and generate an SCC
    if (v.lowlink = v.index) then
      start a new strongly connected component
      repeat
        w := S.pop()
        w.onStack := false
        add w to current strongly connected component
      while (w != v)
      output the current strongly connected component
    end if
  end function
```

The algorithm does a depth-first search, keeping track of two properties for
each vertex: when it was encountered (the _index_) and the lowest index of any
vertex reachable from this vertex (the _lowlink_). It pushes vertices on to a
stack as it goes and outputs a strongly connected component when it cannot find
any vertices it has not seen before.

As presented it is very imperative and I like Haskell, but fortunately
[imperative
Haskell](http://vaibhavsagar.com/blog/2017/05/29/imperative-haskell/) is pretty
straightforward and I have an implementation
[here](https://github.com/vaibhavsagar/courses/blob/master/algorithms1/week4/SCC.ipynb).

This seems like an incredibly niche use case, but validating and processing
DAGs in this way happens surprisingly frequently. Consider a build process
where inputs and outputs are nodes and their relationships are directed edges.
The presence of an SCC with more than one vertex indicates a cyclic dependency,
and dependencies need to be built before the nodes that they depend on, which
implies a reverse topological sort. This generalises to dataflow programming,
where loops need to be identified (and usually eliminated). I like to think of
this algorithm as an all-in-one DAG toolkit.

We can also use it to solve
[2SAT](https://en.wikipedia.org/wiki/2-satisfiability), which is the problem of
determining whether boolean variables in series of constraints of the form `a
|| b` can be assigned T and F values such that all constraints hold. This is
discussed [here]() but boils down to encoding the constraints as nodes and
edges, calculating the SCCs, and processing the output in reverse topologically
sorted order. An advantage to doing it this way is that the process can stop at
the first SCC that indicates unsatisfiability. I have an implementation of this
[here](https://github.com/vaibhavsagar/courses/blob/master/algorithms2/week6/Week6.ipynb).

Discovering this algorithm got me excited about theoretical computer science
and reminded me that algorithms can be fun, interesting, and an opportunity to
marvel at the music of the spheres. I'm curious to know what other equally
awesome algorithms are out there. Which one's your favourite?

Thanks to [Annie Cherkaev](https://anniecherkaev.com/) for the title and
feedback, and [Iain McCoy](https://twitter.com/imccoy) for suggesting [a more
functional
approach](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.45.3876).
