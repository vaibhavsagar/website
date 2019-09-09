--------------------------------------------------------------------------------
title: "Refactoring Haskell: A Case Study"
published: 2019-02-12
tags: programming, haskell
--------------------------------------------------------------------------------

Many people claim that [refactoring Haskell is a
joy](https://twitter.com/search?q=haskell%20refactoring). I've certainly found
this to be the case, but what does that mean in practice? I thought it might be
useful to demonstrate by refactoring some of my own code.

The code we're looking at today is an implementation of [Tarjan's Strongly
Connected Components
algorithm](https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm)
used to determine whether a given [2-SAT
problem](https://en.wikipedia.org/wiki/2-satisfiability) is satisfiable or not,
and was written to complete [an online
course](https://online.stanford.edu/course/algorithms-design-and-analysis-part-1)
that is now offered in a different form. I've [written about Tarjan's algorithm
previously](/blog/2017/06/10/dag-toolkit/) and it can be
used to determine the satisfiability of a 2-SAT problem by checking if any SCC
contains both a variable and its negation. If it does, we have a contradiction
and the problem is unsatisfiable, otherwise the problem is satisfiable.

This code isn't particularly elegant or easy to follow, and it's lousy with
mutable state. Despite these drawbacks, it is still relatively straightforward
to refactor.

If you'd like to follow along, I have the code (and some test data) available
[at this
gist](https://gist.github.com/vaibhavsagar/2418c9dd79da431065ad0d80e690b12f)
with each revision representing a refactoring step.

The initial version of the code is as follows:

<details>
<summary style="cursor: pointer">Initial 2SAT.hs</summary>

```haskell
{-# LANGUAGE LambdaCase #-}

import qualified Data.Graph      as G
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Array      as A
import qualified Prelude         as P

import Prelude hiding (lookup)

import Control.Monad.ST
import Data.STRef
import Control.Monad (forM_, when)
import Data.Maybe (isJust, isNothing, fromJust)

tarjan :: Int -> G.Graph -> Maybe [S.Set Int]
tarjan n graph = runST $ do
    index    <- newSTRef 0
    stack    <- newSTRef []
    stackSet <- newSTRef S.empty
    indices  <- newSTRef M.empty
    lowlinks <- newSTRef M.empty
    output   <- newSTRef (Just [])

    forM_ (G.vertices graph) $ \v -> do
        vIndex <- M.lookup v <$> readSTRef indices
        when (isNothing vIndex) $
            strongConnect n v graph index stack stackSet indices lowlinks output

    readSTRef output

strongConnect
    :: Int
    -> Int
    -> G.Graph
    -> STRef s Int
    -> STRef s [Int]
    -> STRef s (S.Set Int)
    -> STRef s (M.Map Int Int)
    -> STRef s (M.Map Int Int)
    -> STRef s (Maybe [S.Set Int])
    -> ST    s ()
strongConnect n v graph index stack stackSet indices lowlinks output = do
    i <- readSTRef index
    insert v i indices
    insert v i lowlinks
    modifySTRef' index (+1)
    push stack stackSet v

    forM_ (graph A.! v) $ \w -> lookup w indices >>= \case
        Nothing     -> do
            strongConnect n w graph index stack stackSet indices lowlinks output
            vLowLink <- fromJust <$> lookup v lowlinks
            wLowLink <- fromJust <$> lookup w lowlinks
            insert v (min vLowLink wLowLink) lowlinks
        Just wIndex -> do
            wOnStack <- S.member w <$> readSTRef stackSet
            when wOnStack $ do
                vLowLink <- fromJust <$> lookup v lowlinks
                insert v (min vLowLink wIndex) lowlinks

    vLowLink <- fromJust <$> lookup v lowlinks
    vIndex   <- fromJust <$> lookup v indices
    when (vLowLink == vIndex) $ do
        scc <- addSCC n v S.empty stack stackSet
        modifySTRef' output $ \sccs -> (:) <$> scc <*> sccs
    where
        lookup value hashMap     = M.lookup value <$> readSTRef hashMap
        insert key value hashMap = modifySTRef' hashMap (M.insert key value)

addSCC :: Int -> Int -> S.Set Int -> STRef s [Int] -> STRef s (S.Set Int) -> ST s (Maybe (S.Set Int))
addSCC n v scc stack stackSet = pop stack stackSet >>= \w -> if ((other n w) `S.member` scc) then return Nothing else
    let scc' = S.insert w scc
    in if w == v then return (Just scc') else addSCC n v scc' stack stackSet

push :: STRef s [Int] -> STRef s (S.Set Int) -> Int -> ST s ()
push stack stackSet e = do
    modifySTRef' stack    (e:)
    modifySTRef' stackSet (S.insert e)

pop :: STRef s [Int] -> STRef s (S.Set Int) -> ST s Int
pop stack stackSet = do
    e <- head <$> readSTRef stack
    modifySTRef' stack tail
    modifySTRef' stackSet (S.delete e)
    return e

denormalise     = subtract
normalise       = (+)
other n v       = 2*n - v
clauses n [u,v] = [(other n u, v), (other n v, u)]

checkSat :: String -> IO Bool
checkSat name = do
    p <- map (map P.read . words) . lines <$> readFile name
    let pNo    = head $ head p
        pn     = map (map (normalise pNo)) $ tail p
        pGraph = G.buildG (0,2*pNo) $ concatMap (clauses pNo) pn
    return $ (Nothing /=) $ tarjan pNo pGraph
```

</details>

I've included 2SAT-specific functionality for completeness, but I'll only be
changing the `tarjan` function and the functions it depends on
(`strongConnect`, `addSCC`, `push`, and `pop`).

The first change is using more suitable data structures. Tarjan's algorithm is
only linear in the size of the graph when operations, such as checking if `w` is
on the stack and looking up indices, happen in constant time ($O(1)$). I'm
currently using `Data.Map` and `Data.Set` which are both implemented with trees
and are $O(\log{}n)$ in these operations. A better choice would be
[`Data.Vector.Mutable`](http://hackage.haskell.org/package/vector/docs/Data-Vector-Mutable.html)
from the `vector` package, which does have constant-time operations.

This refactoring mostly consists of initialising vectors with a known length
and replacing calls to `lookup` and `insert` with calls to `read` and `write`.

<details>
<summary style="cursor: pointer">2SAT.hs using `vector`</summary>

```haskell
{-# LANGUAGE LambdaCase #-}

import qualified Data.Graph as G
import qualified Data.Array as A
import qualified Prelude    as P

import Prelude hiding (lookup, read, replicate)

import Control.Monad.ST
import Data.STRef
import Control.Monad       (forM_, when)
import Data.Maybe          (isJust, isNothing, fromJust)
import Data.Vector.Mutable (STVector, read, replicate, write)

tarjan :: Int -> G.Graph -> Maybe [[Int]]
tarjan n graph = runST $ do
    index    <- newSTRef 0
    stack    <- newSTRef []
    stackSet <- replicate size False
    indices  <- replicate size Nothing
    lowlinks <- replicate size Nothing
    output   <- newSTRef (Just [])

    forM_ (G.vertices graph) $ \v -> do
        vIndex <- read indices v
        when (isNothing vIndex) $
            strongConnect n v graph index stack stackSet indices lowlinks output

    readSTRef output
    where
        size = snd (A.bounds graph) + 1

strongConnect
    :: Int
    -> Int
    -> G.Graph
    -> STRef s Int
    -> STRef s [Int]
    -> STVector s Bool
    -> STVector s (Maybe Int)
    -> STVector s (Maybe Int)
    -> STRef s (Maybe [[Int]])
    -> ST    s ()
strongConnect n v graph index stack stackSet indices lowlinks output = do
    i <- readSTRef index
    write indices  v (Just i)
    write lowlinks v (Just i)
    modifySTRef' index (+1)
    push stack stackSet v

    forM_ (graph A.! v) $ \w -> read indices w >>= \case
        Nothing     -> do
            strongConnect n w graph index stack stackSet indices lowlinks output
            vLowLink <- fromJust <$> read lowlinks v
            wLowLink <- fromJust <$> read lowlinks w
            write lowlinks v (Just (min vLowLink wLowLink))
        Just wIndex -> do
            wOnStack <- read stackSet w
            when wOnStack $ do
                vLowLink <- fromJust <$> read lowlinks v
                write lowlinks v (Just (min vLowLink wIndex))

    vLowLink <- fromJust <$> read lowlinks v
    vIndex   <- fromJust <$> read indices  v
    when (vLowLink == vIndex) $ do
        scc <- addSCC n v [] stack stackSet
        modifySTRef' output $ \sccs -> (:) <$> scc <*> sccs

addSCC :: Int -> Int -> [Int] -> STRef s [Int] -> STVector s Bool -> ST s (Maybe [Int])
addSCC n v scc stack stackSet = pop stack stackSet >>= \w -> if ((other n w) `elem` scc) then return Nothing else
    let scc' = w:scc
    in if w == v then return (Just scc') else addSCC n v scc' stack stackSet

push :: STRef s [Int] -> STVector s Bool -> Int -> ST s ()
push stack stackSet e = do
    modifySTRef' stack (e:)
    write stackSet e True

pop :: STRef s [Int] -> STVector s Bool -> ST s Int
pop stack stackSet = do
    e <- head <$> readSTRef stack
    modifySTRef' stack tail
    write stackSet e False
    return e

denormalise     = subtract
normalise       = (+)
other n v       = 2*n - v
clauses n [u,v] = [(other n u, v), (other n v, u)]

checkSat :: String -> IO Bool
checkSat name = do
    p <- map (map P.read . words) . lines <$> readFile name
    let pNo    = head $ head p
        pn     = map (map (normalise pNo)) $ tail p
        pGraph = G.buildG (0,2*pNo) $ concatMap (clauses pNo) pn
    return $ (Nothing /=) $ tarjan pNo pGraph
```
</details>

I didn't notice a significant difference in speed on my inputs, but it's good
to know that the algorithm has been implemented with the correct asymptotics
now!

_Sidenote: A `Vector` of `Bool`s can be much more compactly represented as a
sequence of 0s and 1s, which are just machine words. For implementations of
this in Haskell, see the [bv](https://hackage.haskell.org/package/bv) or
[bv-little](https://hackage.haskell.org/package/bv-little) packages. Using
these could be another possible refactoring._

Looking at the code again, I notice some repetition of the form

```haskell
x <- fromJust <$> lookup vectorX i
y <- fromJust <$> lookup vectorY j
write vectorZ k (Just (operation x y))
```

and with the judicious use of `(=<<)` and `(<*>)` this can instead be

```haskell
write vectorZ k =<< (operation <$> lookup vectorX i <*> lookup vectorY j)
```

There are a couple of other places we could use `(<*>)`:

<details>
<summary style="cursor: pointer">2SAT.hs using `(<*>)`</summary>

```haskell
{-# LANGUAGE LambdaCase #-}

import qualified Data.Graph as G
import qualified Data.Array as A
import qualified Prelude    as P

import Prelude hiding (lookup, read, replicate)

import Control.Monad.ST
import Data.STRef
import Control.Monad       (forM_, when)
import Data.Maybe          (isJust, isNothing, fromJust)
import Data.Vector.Mutable (STVector, read, replicate, write)

tarjan :: Int -> G.Graph -> Maybe [[Int]]
tarjan n graph = runST $ do
    index    <- newSTRef 0
    stack    <- newSTRef []
    stackSet <- replicate size False
    indices  <- replicate size Nothing
    lowlinks <- replicate size Nothing
    output   <- newSTRef (Just [])

    forM_ (G.vertices graph) $ \v -> do
        vIndex <- read indices v
        when (isNothing vIndex) $
            strongConnect n v graph index stack stackSet indices lowlinks output

    readSTRef output
    where
        size = snd (A.bounds graph) + 1

strongConnect
    :: Int
    -> Int
    -> G.Graph
    -> STRef s Int
    -> STRef s [Int]
    -> STVector s Bool
    -> STVector s (Maybe Int)
    -> STVector s (Maybe Int)
    -> STRef s (Maybe [[Int]])
    -> ST    s ()
strongConnect n v graph index stack stackSet indices lowlinks output = do
    i <- readSTRef index
    write indices  v (Just i)
    write lowlinks v (Just i)
    modifySTRef' index (+1)
    push stack stackSet v

    forM_ (graph A.! v) $ \w -> read indices w >>= \case
        Nothing -> do
            strongConnect n w graph index stack stackSet indices lowlinks output
            write lowlinks v =<< (min <$> read lowlinks v <*> read lowlinks w)
        Just{}  -> do
            wOnStack <- read stackSet w
            when wOnStack $ do
                write lowlinks v =<< (min <$> read lowlinks v <*> read indices w)

    vLowLink <- fromJust <$> read lowlinks v
    vIndex   <- fromJust <$> read indices  v
    when (vLowLink == vIndex) $ do
        scc <- addSCC n v [] stack stackSet
        modifySTRef' output $ \sccs -> (:) <$> scc <*> sccs

addSCC :: Int -> Int -> [Int] -> STRef s [Int] -> STVector s Bool -> ST s (Maybe [Int])
addSCC n v scc stack stackSet = pop stack stackSet >>= \w -> if ((other n w) `elem` scc) then return Nothing else
    let scc' = w:scc
    in if w == v then return (Just scc') else addSCC n v scc' stack stackSet

push :: STRef s [Int] -> STVector s Bool -> Int -> ST s ()
push stack stackSet e = do
    modifySTRef' stack (e:)
    write stackSet e True

pop :: STRef s [Int] -> STVector s Bool -> ST s Int
pop stack stackSet = do
    e <- head <$> readSTRef stack
    modifySTRef' stack tail
    write stackSet e False
    return e

denormalise     = subtract
normalise       = (+)
other n v       = 2*n - v
clauses n [u,v] = [(other n u, v), (other n v, u)]

checkSat :: String -> IO Bool
checkSat name = do
    p <- map (map P.read . words) . lines <$> readFile name
    let pNo    = head $ head p
        pn     = map (map (normalise pNo)) $ tail p
        pGraph = G.buildG (0,2*pNo) $ concatMap (clauses pNo) pn
    return $ (Nothing /=) $ tarjan pNo pGraph
```
</details>

This is much nicer with the applicative combinators.

I would like to clean up that `when` as well, and for that I'd need a function
like

```haskell
whenM :: Monad m => m Bool -> m () -> m ()
```
which is [available in Neil Mitchell's `extra`
package](hackage.haskell.org/package/extra/docs/Control-Monad-Extra.html#v:whenM).

I don't think it's worth pulling in that dependency though, so I'll just copy
that definition:

<details>
<summary style="cursor: pointer">2SAT.hs using `whenM`</summary>
```haskell
{-# LANGUAGE LambdaCase #-}

import qualified Data.Graph as G
import qualified Data.Array as A
import qualified Prelude    as P

import Prelude hiding (lookup, read, replicate)

import Control.Monad.ST
import Data.STRef
import Control.Monad       (forM_)
import Data.Vector.Mutable (STVector, read, replicate, write)

whenM :: Monad m => m Bool -> m () -> m ()
whenM condM block = condM >>= \cond -> if cond then block else return ()

tarjan :: Int -> G.Graph -> Maybe [[Int]]
tarjan n graph = runST $ do
    index    <- newSTRef 0
    stack    <- newSTRef []
    stackSet <- replicate size False
    indices  <- replicate size Nothing
    lowlinks <- replicate size Nothing
    output   <- newSTRef (Just [])

    forM_ (G.vertices graph) $ \v ->
        whenM ((==) Nothing <$> read indices v) $
            strongConnect n v graph index stack stackSet indices lowlinks output

    readSTRef output
    where
        size = snd (A.bounds graph) + 1

strongConnect
    :: Int
    -> Int
    -> G.Graph
    -> STRef s Int
    -> STRef s [Int]
    -> STVector s Bool
    -> STVector s (Maybe Int)
    -> STVector s (Maybe Int)
    -> STRef s (Maybe [[Int]])
    -> ST    s ()
strongConnect n v graph index stack stackSet indices lowlinks output = do
    i <- readSTRef index
    write indices  v (Just i)
    write lowlinks v (Just i)
    modifySTRef' index (+1)
    push stack stackSet v

    forM_ (graph A.! v) $ \w -> read indices w >>= \case
        Nothing -> do
            strongConnect n w graph index stack stackSet indices lowlinks output
            write lowlinks v =<< (min <$> read lowlinks v <*> read lowlinks w)
        Just{}  -> whenM (read stackSet w) $
            write lowlinks v =<< (min <$> read lowlinks v <*> read indices  w)

    whenM ((==) <$> read lowlinks v <*> read indices v) $ do
        scc <- addSCC n v [] stack stackSet
        modifySTRef' output $ \sccs -> (:) <$> scc <*> sccs

addSCC :: Int -> Int -> [Int] -> STRef s [Int] -> STVector s Bool -> ST s (Maybe [Int])
addSCC n v scc stack stackSet = pop stack stackSet >>= \w -> if ((other n w) `elem` scc) then return Nothing else
    let scc' = w:scc
    in if w == v then return (Just scc') else addSCC n v scc' stack stackSet

push :: STRef s [Int] -> STVector s Bool -> Int -> ST s ()
push stack stackSet e = do
    modifySTRef' stack (e:)
    write stackSet e True

pop :: STRef s [Int] -> STVector s Bool -> ST s Int
pop stack stackSet = do
    e <- head <$> readSTRef stack
    modifySTRef' stack tail
    write stackSet e False
    return e

denormalise     = subtract
normalise       = (+)
other n v       = 2*n - v
clauses n [u,v] = [(other n u, v), (other n v, u)]

checkSat :: String -> IO Bool
checkSat name = do
    p <- map (map P.read . words) . lines <$> readFile name
    let pNo    = head $ head p
        pn     = map (map (normalise pNo)) $ tail p
        pGraph = G.buildG (0,2*pNo) $ concatMap (clauses pNo) pn
    return $ (Nothing /=) $ tarjan pNo pGraph
```
</details>

Now I don't actually even need `when` anymore!

Since most of the auxiliary functions aren't used outside `strongConnect`, it
might make sense to put them under a `where` clause. This would also make the
parameters passed to `strongConnect` available to these functions. This is one
place that the `ScopedTypeVariables` language extension is necessary, otherwise
GHC can't tell that the `s` in the type signature of `strongConnect` is the
same `s` as the one in each type signature under the `where` clause.

<details>
<summary style="cursor: pointer">2SAT.hs using `where`</summary>
```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Graph as G
import qualified Data.Array as A
import qualified Prelude    as P

import Prelude hiding (lookup, read, replicate)

import Control.Monad.ST
import Data.STRef
import Control.Monad       (forM_)
import Data.Vector.Mutable (STVector, read, replicate, write)

whenM :: Monad m => m Bool -> m () -> m ()
whenM condM block = condM >>= \cond -> if cond then block else return ()

tarjan :: Int -> G.Graph -> Maybe [[Int]]
tarjan n graph = runST $ do
    index    <- newSTRef 0
    stack    <- newSTRef []
    stackSet <- replicate size False
    indices  <- replicate size Nothing
    lowlinks <- replicate size Nothing
    output   <- newSTRef (Just [])

    forM_ (G.vertices graph) $ \v ->
        whenM ((==) Nothing <$> read indices v) $
            strongConnect n v graph index stack stackSet indices lowlinks output

    readSTRef output
    where
        size = snd (A.bounds graph) + 1

strongConnect
    :: forall s
    .  Int
    -> Int
    -> G.Graph
    -> STRef s Int
    -> STRef s [Int]
    -> STVector s Bool
    -> STVector s (Maybe Int)
    -> STVector s (Maybe Int)
    -> STRef s (Maybe [[Int]])
    -> ST    s ()
strongConnect n v graph index stack stackSet indices lowlinks output = do
    i <- readSTRef index
    write indices  v (Just i)
    write lowlinks v (Just i)
    modifySTRef' index (+1)
    push v

    forM_ (graph A.! v) $ \w -> read indices w >>= \case
        Nothing -> do
            strongConnect n w graph index stack stackSet indices lowlinks output
            write lowlinks v =<< (min <$> read lowlinks v <*> read lowlinks w)
        Just{}  -> whenM (read stackSet w) $
            write lowlinks v =<< (min <$> read lowlinks v <*> read indices  w)

    whenM ((==) <$> read lowlinks v <*> read indices v) $ do
        scc <- addSCC n v []
        modifySTRef' output $ \sccs -> (:) <$> scc <*> sccs
    where
        addSCC :: Int -> Int -> [Int] -> ST s (Maybe [Int])
        addSCC n v scc = pop >>= \w -> if ((other n w) `elem` scc) then return Nothing else
            let scc' = w:scc
            in if w == v then return (Just scc') else addSCC n v scc'
        push :: Int -> ST s ()
        push e = do
            modifySTRef' stack (e:)
            write stackSet e True
        pop :: ST s Int
        pop = do
            e <- head <$> readSTRef stack
            modifySTRef' stack tail
            write stackSet e False
            return e

denormalise     = subtract
normalise       = (+)
other n v       = 2*n - v
clauses n [u,v] = [(other n u, v), (other n v, u)]

checkSat :: String -> IO Bool
checkSat name = do
    p <- map (map P.read . words) . lines <$> readFile name
    let pNo    = head $ head p
        pn     = map (map (normalise pNo)) $ tail p
        pGraph = G.buildG (0,2*pNo) $ concatMap (clauses pNo) pn
    return $ (Nothing /=) $ tarjan pNo pGraph
```
</details>

I think the logic is clearer now that the auxiliary functions take fewer
arguments.

Instead of a large number of implictly related variables, it might be nice to
define a single product type containing our entire environment and pass just
one value around. With `NamedFieldPuns` only minimal code changes are required:

<details>
<summary style="cursor: pointer">2SAT.hs using `NamedFieldPuns`</summary>
```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Graph as G
import qualified Data.Array as A
import qualified Prelude    as P

import Prelude hiding (lookup, read, replicate)

import Control.Monad.ST
import Data.STRef
import Control.Monad       (forM_)
import Data.Vector.Mutable (STVector, read, replicate, write)

data TarjanEnv s = TarjanEnv
    { index    :: STRef s Int
    , stack    :: STRef s [Int]
    , stackSet :: STVector s Bool
    , indices  :: STVector s (Maybe Int)
    , lowlinks :: STVector s (Maybe Int)
    , output   :: STRef s (Maybe [[Int]])
    }

whenM :: Monad m => m Bool -> m () -> m ()
whenM condM block = condM >>= \cond -> if cond then block else return ()

tarjan :: Int -> G.Graph -> Maybe [[Int]]
tarjan n graph = runST $ do
    tarjanEnv <- TarjanEnv
        <$> newSTRef 0
        <*> newSTRef []
        <*> replicate size False
        <*> replicate size Nothing
        <*> replicate size Nothing
        <*> newSTRef (Just [])

    forM_ (G.vertices graph) $ \v ->
        whenM ((==) Nothing <$> read (indices tarjanEnv) v) $
            strongConnect n v graph tarjanEnv

    readSTRef (output tarjanEnv)
    where
        size = snd (A.bounds graph) + 1

strongConnect :: forall s. Int -> Int -> G.Graph -> TarjanEnv s -> ST s ()
strongConnect n v graph tarjanEnv@TarjanEnv{ index, stack, stackSet, indices, lowlinks, output } = do
    i <- readSTRef index
    write indices  v (Just i)
    write lowlinks v (Just i)
    modifySTRef' index (+1)
    push v

    forM_ (graph A.! v) $ \w -> read indices w >>= \case
        Nothing -> do
            strongConnect n w graph tarjanEnv
            write lowlinks v =<< (min <$> read lowlinks v <*> read lowlinks w)
        Just{}  -> whenM (read stackSet w) $
            write lowlinks v =<< (min <$> read lowlinks v <*> read indices  w)

    whenM ((==) <$> read lowlinks v <*> read indices v) $ do
        scc <- addSCC n v []
        modifySTRef' output $ \sccs -> (:) <$> scc <*> sccs
    where
        addSCC :: Int -> Int -> [Int] -> ST s (Maybe [Int])
        addSCC n v scc = pop >>= \w -> if ((other n w) `elem` scc) then return Nothing else
            let scc' = w:scc
            in if w == v then return (Just scc') else addSCC n v scc'
        push :: Int -> ST s ()
        push e = do
            modifySTRef' stack (e:)
            write stackSet e True
        pop :: ST s Int
        pop = do
            e <- head <$> readSTRef stack
            modifySTRef' stack tail
            write stackSet e False
            return e

denormalise     = subtract
normalise       = (+)
other n v       = 2*n - v
clauses n [u,v] = [(other n u, v), (other n v, u)]

checkSat :: String -> IO Bool
checkSat name = do
    p <- map (map P.read . words) . lines <$> readFile name
    let pNo    = head $ head p
        pn     = map (map (normalise pNo)) $ tail p
        pGraph = G.buildG (0,2*pNo) $ concatMap (clauses pNo) pn
    return $ (Nothing /=) $ tarjan pNo pGraph
```
</details>

Let's pause here. Although more refactoring is certainly possible, my last two
steps did not reduce the line count and may have in fact made the code harder
to understand.

How have we benefited from this refactoring? Aside from the code being shorter
and better structured, it's now easier to make meaningful improvements. For
example, this implementation is more inefficient than it needs to be, because
it doesn't short-circuit when it finds that the current problem is
unsatisfiable. Instead it works through the rest of the problem, only to throw
all that work away. A sophisticated solution to this problem might involve the
use of the
[`ExceptT`](https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Except.html)
monad transformer to throw an exception and exit early, but there is a simpler
approach: we can store an extra boolean variable denoting whether or not the
current problem is possibly satisfiable, and only continue working if it is.
I'll call this variable `possible`, update it in `addSCC`, and check for it
before each call to `strongConnect` in `tarjan`. It takes more effort to
reformat the code than to make this change:

<details>
<summary style="cursor: pointer">2SAT.hs with short-circuiting</summary>
```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Graph as G
import qualified Data.Array as A
import qualified Prelude    as P

import Prelude hiding (lookup, read, replicate)

import Control.Monad.ST
import Data.STRef
import Control.Monad       (forM_)
import Data.Vector.Mutable (STVector, read, replicate, write)

data TarjanEnv s = TarjanEnv
    { index    :: STRef s Int
    , stack    :: STRef s [Int]
    , stackSet :: STVector s Bool
    , indices  :: STVector s (Maybe Int)
    , lowlinks :: STVector s (Maybe Int)
    , output   :: STRef s (Maybe [[Int]])
    , possible :: STRef s Bool
    }

whenM :: Monad m => m Bool -> m () -> m ()
whenM condM block = condM >>= \cond -> if cond then block else return ()

tarjan :: Int -> G.Graph -> Maybe [[Int]]
tarjan n graph = runST $ do
    tarjanEnv <- TarjanEnv
        <$> newSTRef 0
        <*> newSTRef []
        <*> replicate size False
        <*> replicate size Nothing
        <*> replicate size Nothing
        <*> newSTRef (Just [])
        <*> newSTRef True

    forM_ (G.vertices graph) $ \v ->
        whenM ((&&)
            <$> ((==) Nothing <$> read (indices tarjanEnv) v)
            <*> readSTRef (possible tarjanEnv)) $
                strongConnect n v graph tarjanEnv

    readSTRef (output tarjanEnv)
    where
        size = snd (A.bounds graph) + 1

strongConnect :: forall s. Int -> Int -> G.Graph -> TarjanEnv s -> ST s ()
strongConnect n v graph tarjanEnv@TarjanEnv{ index, stack, stackSet, indices, lowlinks, output, possible } = do
    i <- readSTRef index
    write indices  v (Just i)
    write lowlinks v (Just i)
    modifySTRef' index (+1)
    push v

    forM_ (graph A.! v) $ \w -> read indices w >>= \case
        Nothing -> do
            strongConnect n w graph tarjanEnv
            write lowlinks v =<< (min <$> read lowlinks v <*> read lowlinks w)
        Just{}  -> whenM (read stackSet w) $
            write lowlinks v =<< (min <$> read lowlinks v <*> read indices  w)

    whenM ((==) <$> read lowlinks v <*> read indices v) $ do
        scc <- addSCC n v []
        modifySTRef' output $ \sccs -> (:) <$> scc <*> sccs
    where
        addSCC :: Int -> Int -> [Int] -> ST s (Maybe [Int])
        addSCC n v scc = pop >>= \w -> if ((other n w) `elem` scc)
            then writeSTRef possible False >> return Nothing
            else
                let scc' = w:scc
                in if w == v then return (Just scc') else addSCC n v scc'
        push :: Int -> ST s ()
        push e = do
            modifySTRef' stack (e:)
            write stackSet e True
        pop :: ST s Int
        pop = do
            e <- head <$> readSTRef stack
            modifySTRef' stack tail
            write stackSet e False
            return e

denormalise     = subtract
normalise       = (+)
other n v       = 2*n - v
clauses n [u,v] = [(other n u, v), (other n v, u)]

checkSat :: String -> IO Bool
checkSat name = do
    p <- map (map P.read . words) . lines <$> readFile name
    let pNo    = head $ head p
        pn     = map (map (normalise pNo)) $ tail p
        pGraph = G.buildG (0,2*pNo) $ concatMap (clauses pNo) pn
    return $ (Nothing /=) $ tarjan pNo pGraph
```
</details>

This change does seem to make a significant difference, and it's good to know
we're not doing useless work.

I think this is a good place to stop, and I hope I've been able to demonstrate
some of Haskell's strengths when it comes to refactoring. In my experience,
it's not usually necessary to deeply understand Haskell code in order to
attempt a refactoring, especially if it's backed by well-chosen types and a
good test suite. I also find that I'm able to be more daring when writing new
code, because bad up-front design is less costly and even the jankiest working
code can be gently massaged into something presentable.

_Thanks to
[Joel Burget](https://joelburget.com/), [Mat Fournier](http://www.matfournier.com/), [Robert Klotzner](https://eskimor.gonimo.com/), [Tenor](https://github.com/L8D), [Tom Harding](http://www.tomharding.me/), and [Tyler Weir](http://www.tylerweir.com/) for suggestions and
feedback._
