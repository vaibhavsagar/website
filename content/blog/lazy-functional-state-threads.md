Title: Lazy Functional State Threads
Date: 2017-06-19
Category: programming
Status: draft

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
