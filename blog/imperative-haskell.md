--------------------------------------------------------------------------------
title: Imperative Haskell
published: 2017-05-29
tags: haskell, programming
--------------------------------------------------------------------------------
_This post covers essentially the same material as a 5-minute presentation I
gave at
[RC](https://www.recurse.com/scout/click?t=5ac465e5d3396a7e491e42afac4c5c90),
because giving that talk over and over again doesn't scale and there are things
I would like to cover that are difficult within that time limit._

I was working through Tim Roughgarden's [Algorithms
1](https://www.coursera.org/learn/algorithm-design-analysis/) (which has now
been replaced by two smaller courses) and attempting to do all the exercises in
Haskell when I bumped up against an uncomfortable truth. Haskell's 'quicksort':

```haskell
qsort []     = []
qsort (x:xs) = lt ++ [x] ++ gt
    where lt = qsort [e | e <- xs, e <  x]
          gt = qsort [e | e <- xs, e >= x]
```

isn't a true quicksort! Specifically, it doesn't sort the elements in place,
and the assignment I was working on involved counting the number of
comparisons, so I couldn't get away with my fake quicksort. With my tail
between my legs, I gave up on my pure Haskell approach and implemented a
solution in Python:

```python
import sys
sys.setrecursionlimit(10000)

def partition_first(array, l, r):
    p = array[l]
    i = l + 1
    for j in range(l+1, r):
        if array[j] < p:
            array[j], array[i] = array[i], array[j]
            i += 1
    array[l], array[i-1] = array[i-1], array[l]
    return (i-1)

def partition_last(array, l, r):
    array[r-1], array[l] = array[l], array[r-1]
    return partition_first(array, l, r)

def partition_median(array, l, r):
    p_idx = choose_median(array, l, r)
    array[p_idx], array[l] = array[l], array[p_idx]
    return partition_first(array, l, r)

def choose_median(array, l, r):
    head = array[l]
    last = array[r-1]
    length = r-l
    if length % 2 == 0:
        mid_idx = l + (length//2) - 1
    else:
        mid_idx = l + (length//2)
    mid = array[mid_idx]
    options = [(l, head), (mid_idx, mid), (r-1, last)]
    options.remove(max(options, key=lambda v: v[1]))
    options.remove(min(options, key=lambda v: v[1]))
    return options[0][0]

def quicksort(array, start, end, partition):
    global comparisons
    if end<=start: return
    else:
        p_idx = partition(array, start, end)
        comparisons += (end-start-1)
        quicksort(array, start, p_idx, partition)
        quicksort(array, p_idx+1, end, partition)


comparisons = 0
inp1 = contents.copy()
quicksort(inp1, 0, len(inp1), partition_first)
print(comparisons)

comparisons = 0
inp2 = contents.copy()
quicksort(inp2, 0, len(inp2), partition_last)
print(comparisons)

comparisons = 0
inp3 = contents.copy()
quicksort(inp3, 0, len(inp3), partition_median)
print(comparisons)
```

This implementation is not particularly Pythonic: note the recursion limit and
the use of a global variable. I actually forgot to reset the variable to 0
between iterations, which was fun to track down. But it works!

So far, so good. This isn't something we'd be able to do in Haskell, right? And
even if we could, the equivalent implementation would be so different as to be
unrecognisable. At least this is what I thought until I took a closer look at
[Control.Monad.ST](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Monad-ST.html) and [Data.STRef](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-STRef.html).

One of my biggest gripes with Haskell is the quality of the documentation.
`Control.Monad.ST` is introduced as

> This library provides support for strict state threads, as described in the
> PLDI '94 paper by John Launchbury and Simon Peyton Jones _Lazy Functional
> State Threads_.

and `Data.STRef` is introduced as

> Mutable references in the (strict) ST monad.

I don't want to read a paper to figure out how to use these libraries, and in
fact I don't have to! In recognition of this, I humbly present alternative
descriptions for `Control.Monad.ST`:

> You asked for mutable state, here it is!

and `Data.STRef`:

> Variables that you can actually vary!!!1!1!one!1eleventyone

Code utilising these libraries can look very familiar to people used to
imperative languages, e.g. past me. Here's the above quicksort rewritten in
Haskell:

```haskell
{-# LANGUAGE RankNTypes #-}

import Control.Monad.ST
import Data.STRef
import Data.Vector (fromList, toList, freeze, thaw)
import Control.Monad
import Data.Vector.Mutable (STVector, read, write, swap)
import qualified Data.Vector as V (Vector, length)
import Data.List (sortOn)
import Prelude hiding (read)

vector = fromList contents

partitionFirst array l r = do
    p <- read array l
    i <- newSTRef (l+1)
    forM_ [l+1..(r-1)] $ \j -> do
        arrayJ <- read array j
        i'     <- readSTRef i
        when (arrayJ < p) $ do
            swap array i' j
            modifySTRef' i (+1)
    i' <- readSTRef i
    swap array (i'-1) l
    return (i'-1)

partitionLast array l r = do
    swap array (r-1) l
    partitionFirst array l r

partitionMedian array l r = do
    p <- chooseMedian array l r
    swap array p l
    partitionFirst array l r

chooseMedian array l r = do
    h <- read array l
    t <- read array (r-1)
    let len = r-l
    let mid = if (len `mod` 2) == 0
        then l + (len `div` 2) - 1
        else l + (len `div` 2)
    m <- read array mid
    let options = sortOn snd [(l, h), (mid, m), (r-1, t)]
    return (fst (options !! 1))

quicksort array start end partition comparisons = when (start < end) $ do
    i <- partition array start end
    modifySTRef' comparisons (+ (end-start-1))
    quicksort array start i   partition comparisons
    quicksort array (i+1) end partition comparisons

quicksort' :: Ord a => V.Vector a -> (forall s a. (Ord a) => STVector s a -> Int -> Int -> ST s Int) -> Int
quicksort' vector partition = runST $ do
    array  <- thaw vector
    comps  <- newSTRef 0
    quicksort array 0 (V.length vector) partition comps
    readSTRef comps

quicksort' vector partitionFirst
quicksort' vector partitionLast
quicksort' vector partitionMedian
```

This is roughly the same length as the Python implementation, and even improves
on it in some ways: no recursion limit fiddling and no global variables.

If we can write Haskell that resembles Python, and Python is executable
pseudocode, can we cut out the middleman and translate pseudocode directly to
Haskell? Let's take a look at another problem.

I needed to calculate the size of the strongly connected components of a graph
for another assignment, and I decided to use Tarjan's Strongly Connected
Components algorithm. The pseudocode for that (as taken from Wikipedia) is:

```noweb
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

and here's what that looks like in Haskell:

```haskell
import qualified Data.Array as A
import qualified Data.Graph as G

import Control.Monad       (forM_, when)
import Control.Monad.ST
import Data.STRef
import Data.Vector.Mutable (STVector, read, replicate, write)
import Prelude hiding      (read, replicate)

tarjan graph = runST $ do
    index    <- newSTRef 0
    stack    <- newSTRef []
    stackSet <- replicate size False
    indices  <- replicate size Nothing
    lowlinks <- replicate size Nothing
    output   <- newSTRef []

    forM_ (G.vertices graph) $ \v -> do
        vIndex <- read indices v
        when (vIndex == Nothing) $
            strongConnect v graph index stack stackSet indices lowlinks output

    reverse <$> readSTRef output
    where size = snd (A.bounds graph) + 1

strongConnect v graph index stack stackSet indices lowlinks output = do
    i <- readSTRef index
    write indices  v (Just i)
    write lowlinks v (Just i)
    modifySTRef' index (+1)
    push v

    forM_ (graph A.! v) $ \w -> read indices w >>= \found -> case found of
        Nothing -> do
            strongConnect w graph index stack stackSet indices lowlinks output
            write lowlinks v =<< (min <$> read lowlinks v <*> read lowlinks w)
        Just{}  -> read stackSet w >>= \wOnStack -> when wOnStack $
            write lowlinks v =<< (min <$> read lowlinks v <*> read indices  w)

    vLowLink <- read lowlinks v
    vIndex   <- read indices  v
    when (vLowLink == vIndex) $ modifySTRef' output . (:) =<< addSCC v []
    where
        addSCC v scc = do
            w <- pop
            let scc' = w:scc
            if w == v then return scc' else addSCC v scc'

        push e = do
            modifySTRef' stack (e:)
            write stackSet e True

        pop = do
            e <- head <$> readSTRef stack
            modifySTRef' stack tail
            write stackSet e False
            return e
```

Aside from explicitly declaring our variables and passing them around, I think
this looks pretty close.

How do we square this with Haskell's reputation for purity and referential
transparency? That's the subject of [the paper mentioned
above](https://www.microsoft.com/en-us/research/publication/lazy-functional-state-threads/)
that you don't have to read (but totally can if you want)! They figured out a
way to provide a principled pure interface to mutable state by passing the
references as arguments into each function that makes use of them and
leveraging the type system to make sure any impurity is well contained. The
correctness of this approach was [very recently
verified](http://iris-project.org/pdfs/2017-icfp-runST-submission.pdf). If
desired, we can replace any of the functions with purer and more idiomatic
definitions without changing the output, and that satisfies the definition of
referential transparency!

Why don't we do this all the time, when Haskell is at least a serviceable
imperative language? Because writing imperative programs is hard! They don't
compose as well, have less useful type signatures, and are harder to reason
about. Getting away from those things is why we have Haskell to begin with! The
real question should be: how can we avoid doing things this way as much as
possible?

Before I discovered this part of Haskell, I had this perception of Haskell (and
declarative programming more generally) as "imperative programming but less"
from a practical perspective. I thought that although writing declarative code
in Python was purely (heh) a matter of discipline, writing imperative code in
Haskell required completely reconceptualising the algorithm. Thanks to `ST`, I
now know that this not the case, which is a huge relief. If required, I can do
a literal translation of the algorithm, and clean it up (or not) later. In fact
Haskell is "imperative programming and more", and that's awesome!

Thanks to [Peter Fraenkel](http://blog.podsnap.com/), [Julia
Evans](https://jvns.ca/), and [Michelle Steigerwalt](http://msteigerwalt.com/)
for feedback.

_If you'd rather try to make sense of the [set of disconnected
files](https://github.com/vaibhavsagar/presentations/tree/master/imperative-haskell)
that constitutes my slides for that presentation, you can do that instead,
although I wouldn't recommend it._
