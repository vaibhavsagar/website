--------------------------------------------------------------------------------
title: Lazy Functional State Threads
published: 2017-06-19
tags: haskell, programming, monads
--------------------------------------------------------------------------------

A funny thing happened when I was writing my Imperative Haskell post: after
railing against Haskell's tendency to tell you to go off and read a paper when
introducing a library, I went off and read 'Lazy Functional State Threads' and
was gobsmacked by how accessible I found it. I'd like to try and demystify it
for a wider audience.

The paper begins by admitting that we sometimes want to express strict stateful
computations in a purely-functional language efficiently. We want to do this
[because of reasons](http://www.threewordphrase.com/pardonme.gif). Some
examples of algorithms we'd like to express are those based on mutable hash
tables, union find, and especially input/output. However, the language we are
working with is renowned for its laziness, which means the order of evaluation
can be counterintuitive, and its referential transparency, which means a lack
of side effects.

We square this circle by leveraging the type system, which simultaneously
allows most of the features we take for granted in imperative languages
(multiple named variables, in-place updates) as well as being encapsulated and
referentially transparent (for certain values of referentially transparent).
Let's define a 'state transformer' that takes an initial state as input and
returns a final state. This can be represented as a value of type `ST s a`
where `s` is the state type and `a` is the return type.

<!-- picture of state transformer -->

A state transformer can have multiple inputs (multiple arguments to a function)
and multiple outputs (a tuple).

<!-- picture of multiple input/output state transformer -->

The simplest state transformer takes a value of type `a` and sticks it in a
state transformer. We'll call it `returnST`.

<!-- picture of returnST -->

Let's talk about 'state' now, specifically mutable variables or references.
Let's specify an API for working with references:

```haskell
newVar   :: a -> ST s (MutVar s a)
readVar  :: MutVar s a -> ST s a
writeVar :: MutVar s a -> a -> ST s ()
```

In other words, `newVar` takes a value of type `a` and returns a state
transformer that can be supplied a state to output a new state containing a
fresh reference to the provided value. `readVar` is a state transformer which
leaves the provided state unchanged but extracts the value in the reference
from it. `writeVar` takes a reference and a new value and returns a state
transformer that updates the reference to point to the new value. The fact that
the return value is `()` indicates that this state transformer is only useful
for its effect on the state.

To compose these state transformers together we can define a `thenST`:

```haskell
thenST :: ST s a -> (a -> ST s b) -> ST s b
```

<!-- picture of thenST -->

Two things to note here: both `s1` and `s2` have to work with the same state
type `s`, and they way they work is inherently sequential, because the output
of `s1` is the input of `s2`. This is where 'thread' in the title comes from. I
like to think of this as the baton in a relay: you can't run when you're not
holding the baton, and you can't use a different team's baton because that's
against the rules.

While we let that sink in, I'd like to make a brief historical digression. At
this stage in Haskell's history, monads had been identified as a useful
abstraction and a predecessor to this paper had proposed a particular syntax
for working with them, but do-notation had not yet made its way into the
language. If you're not familiar with monads, you can think of them as an
interface for a type defined by two functions, `return` and `>>=`
(pronounced 'bind') and certain laws that these functions must obey:
```haskell
class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
```
The actual definition differs slightly in ways that aren't relevant here. Why
do I bring this up? If you look closely, we've encountered functions that look
very similar to the two above. If we textually replace `m` with `ST s`, we get

```haskell
return :: a -> ST s a
(>>=)  :: ST s a -> (a -> ST s b) -> ST s b
```
which are exactly the type signatures of `returnST` and `thenST`. This isn't a
coincidence. I will also claim (but not prove) that these definitions are
law-abiding.

What can we do with this information? Let's take this code snippet that swaps
the contents of two references:

```haskell
swap :: MutVar s a -> MutVar s a -> ST s ()
swap v w = readVar v    `thenST` (\a ->
           readVar w    `thenST` (\b ->
           writeVar v b `thenST` (\_ ->
           writeVar w a)))
```

This requires working with infix backticks and lambdas and looks pretty ugly,
but we can do better! We have do-notation which is syntax sugar that converts

```haskell
do x <- f
   y
```
into
```haskell
f >>= (\x -> y)
```
and
```haskell
do x
   y
```
into
```haskell
x >> y
```
where `>>` is like `>>=` that ignores the output of the left hand side:

```haskell
(>>) :: Monad m => m a -> m b -> m b
(>>) ma mb = ma >>= (\_ -> mb)
```

Armed with this knowledge, we can rewrite the above example:

```haskell
swap :: MutVar s a -> MutVar s a -> ST s ()
swap v w = do
    a <- readVar v
    b <- readVar w
    writeVar v b
    writeVar w a
```

Much better! We even define `(>>)` directly as `thenST_`. The rest of the code
samples in this post will use do-notation.

The authors also mention the existence of `fixST`, but this isn't discussed
elsewhere in the paper so I'll skip that.

We now move to the other main contribution of the paper, which is the matter of
how to prevent state from leaking out of our state transformers. The naive
approach would be to define a `runST` that would provide an initial state as
follows:

```haskell
runST :: ST s a -> a
```

But this would allow leakage as in the following example:

```haskell
let v = runST (newVar True)
in
runST (readVar v)
```

and because Haskell is a lazy language, we cannot enforce an ordering on
updates to the state. What we really want is for our `runST` to work _regardless
of which initial state it was given_, and we can encode that in the type
signature with rank-2 polymorphism as follows:

```haskell
runST :: ∀a. (∀s. ST s a) -> a
```

We can read ∀ as 'forall' and this is rank-2 because `s` is scoped within the
parentheses and we can't move it outside without changing the meaning of the
type signature. We can read this as 'for any `a`, given a state transformer
that would work with any `s`, provide it with some state and extract the `a`'.
Our previous example no longer typechecks, because `v` does not have the type
`∀s.ST s a`, and we can see that the typechecker enforces our requirement that
the state cannot leak out of the state transformer, hence ensuring referential
transparency. This definition still allows useful code such as:

```haskell
f :: MutVar s a -> MutVar s a
f v = runST $ do
    w <- newVar v
    readVar w
```

which works because `v` is never dereferenced.

We can generalise our references to consider an array of mutable references. An
API for this could look like:

```haskell
newArr    :: Ix i => (i, i) -> elt -> ST s (MutArr s i elt)
readArr   :: Ix i => MutArr s i elt -> i -> ST s elt
writeArr  :: Ix i => MutArr s i elt -> i -> elt -> ST s ()
freezeArr :: Ix i => MutArr s i elt -> ST s (Array i elt)
```

This is very similar to our API for references, but parametrised over the index
type `i` and including a function `freezeArr` that looks up the current value
of a mutable array in the state and returns an immutable copy of it.

With this API, we can define `accumArray`:

```haskell
accumArray :: Ix i => (a -> b -> a) -> a -> (i, i) -> [(i, b)] -> Array i a
```

This takes a function, an initial value, array bounds, and a list of indexed
values, and does a left fold over each indexed value, putting the result at the
associated index. This can be used to compute a histogram:

```haskell
hist :: Ix i => (i, i) -> [i] -> Array i Int
hist bnds is = accumArray (+) 0 bnds [(i, 1) | i <- is, inRange bnds i]
```

that counts occurrences of elements in `is` within the bounds provided or
`binSort`:

```haskell
binSort :: Ix i => (i,i) -> (a -> i) -> [a] -> Array i a
binSort bnds key vs = accumArray (flip (:)) [] bnds [(key v, v) | v <- vs]
```

that puts each element of `vs` into a bin based on its `key`. `accumArray` can
be defined as follows:

```haskell
accumArray f z bnds ivs = runST $ do
    a <- newArr bnds z
    fill a f ivs
    freezeArr a

fill a f []          = return ()
fill a f ((i,v):ivs) = do
    x <- readArr a i
    writeArr a i (f x v)
    fill a f ivs
```

This is a good example of a function that is internally imperative but
externally pure.

If we define a function to sequence state transformers:

```haskell
seqST :: [ST s ()] -> ST s ()
seqST = sequence_ -- originally defined as `foldr (>>) (return ())`
```

then we can rewrite `accumArray`:

```haskell
accumArray f z bnds ivs = runST $ do
    a <- newArr bnds z
    seqST (map (update a f) ivs)
    freezeArr s

update a f (i, v) = do
    x <- readArr a i
    writeArr a i (f x v)
```

Let's reformulate IO in terms of state transformers. It can be thought of as
a state transformer of type `ST RealWorld a`, where `RealWorld` is an abstract
type representing the real world. We can make this explicit as a type synonym:

```haskell
type IO a = ST RealWorld a
```

We also have a few functions that are specific to IO but not other ST
computations, e.g.

```haskell
putChar :: Char -> IO ()
getChar :: IO Char
```

And this is enough to build e.g. `putString`:

```haskell
putString cs = seqST (map putChar cs)
```

However, we can take it further and define both `putChar` and `getChar` in
terms of `ccall`, which is a language construct that allows the programmer to
call any C procedure (with certain restrictions placed on its use):

```haskell
putChar c = do
    ccall putChar c
    return ()
getChar = ccall getChar
```

Because `IO` is not polymorphic in its state, it can't be used with `runST`.
This is the behaviour we want, and we need to define a special function to
execute `IO` actions. We can call this `mainIO`:

```haskell
mainIO :: IO ()
```

and have it play a role similar to `main()` in C. In fact, IO in GHC is
implemented in Haskell in precisely this manner.

There follows a section with formal semantics which formalises what we've seen
already and outlines a proof of safety. I won't go into more detail than that.
