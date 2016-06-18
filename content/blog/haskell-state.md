Title: You Could Have Invented The State Monad
Date: 2016-06-17
Category: programming

I'm attempting [NICTA/course](https://github.com/NICTA/course) a second time. I
gave up the last time because none of the State exercises were making sense and
I found myself leaning so heavily on the solutions that I wasn't actually
learning anything. This time I was much better prepared after watching lots of
CanFPG talks, reading lots of blog posts and writing a little Haskell, and I
easily cleared the State hurdle. In fact, I'm now going to demonstrate how you
(yes, _you_) could have come up with it (with a little help).

The fundamental insight of State is that it is a function that takes a value of
type `s` and returns a tuple of some value `a` and a new value of type `s`:

```haskell
newtype State s a = State { runState :: s -> (a,s) }
```

Given such a type, what would its `Functor` instance look like?

```haskell
instance Functor (State s) where
  (<$>) :: (a -> b) -> State s a -> State s b
```

Our implementation should be another State that takes a value `s0`, passes it
to the second argument `sa` (resulting in `(a, s1)`) and calls the function
`fn` on `a`:

```haskell
  (<$>) fn (State sa) = State (\s0 -> let (a, s1) = sa s0 in (fn a, s1))
```

This is a State that takes `s0` and returns `(b, s1)`, which is exactly what we
wanted.

Let's look at the `Applicative` instance next:

```haskell
instance Applicative (State s) where
  pure  :: a -> State s a
  (<*>) :: State s (a -> b) -> State s a -> State s b
```

The implementation for `pure` explains where the `a` in our State comes from.
Given some `a`, return a State that, when fed a value `s`, results in `(a,s)`.
It practically writes itself.

```haskell
  pure a = State (\s -> (a,s))
```

`(<*>)` is a bit trickier, because we're dealing with both the State the
function is in and the State its argument is in. The implementation should be a
State that takes a value `s0`, feeds it to `sa` to get `(fn, s1)`, feeds `s1`
to `sb` to get `(a, s2)`, and calls `fn` on `a`:

```haskell
  (<*>) (State sa) (State sb) =
    State (\s0 -> let (fn, s1) = sa s0
                      (a , s2) = sb s1
                  in (fn a, s2))
```

The hardest thing is remembering to thread `s0` through `sa` and `sb`.

Finally, let's look at the `Monad` instance:

```haskell
instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
```
As with all our previous implementions, it has the form:

```haskell
  (>>=) (State sa) fn = State (\s0 -> let ??? in ???)
```

We know that we need to feed `s0` to `sa` to get an `a` to apply to `fn`:

```haskell
  (>>=) (State sa) fn =
    State (\s0 -> let (a, s1) = sa s0
                      ???     = fn a
                  in ???)
```

The result of `fn a` is a `State sb` but we need to return a tuple of `(b, s)`.
We can obtain one by feeding `s1` to `sb`:

```haskell
  (>>=) (State sa) fn =
    State (\s0 -> let (a, s1)  = sa s0
                      State sb = fn a
                  in sb s1)
```

Success!

Let's define a few functions to make our lives easier. `get` returns a State
that, when fed some `s`, returns `(s,s)`. This allows us to expose `s` for
direct modification:

```haskell
get :: State s s
get = State (\s -> (s, s))
```

`put` allows us to store a State that ignores the `s` passed to it later:

```haskell
put :: s -> State s ()
put s = State (\_ -> ((),s))
```

Sometimes we want the `s` and not the `a`:

```haskell
exec :: State s a -> s -> s
exec (State sa) s = snd $ sa s
```

At other times we want the `a` and not the `s`:

```haskell
eval :: State s a -> s -> a
eval (State sa) s = fst $ sa s
```

With all this machinery in place, we can do this:

```haskell
Prelude> exec (do i <- get; put (i+1); return ()) 0
1
```

I still couldn't believe that this worked the first time I tried it, so let's
desugar this:

```haskell
    do i <- get; put (i+1); return ()
 == get >>= \i -> put (i+1) >>= \_ -> pure ()
 == State (\s -> (s, s))    >>= \i ->
    State (\_ -> ((), i+1)) >>= \_ ->
    State (\s -> ((), s))
```

Let's simplify from the bottom up. By the definition of `(>>=)`:

```haskell
    (>>=) (State (\_ -> ((), i+1))) (\_ -> (State (\s -> ((), s)))) =
      State (\s0 -> let (a, s1)  = (\_ -> ((), i+1)) s0
                     -- (a, s1)  = ((), i+1)
                        State sb = (\_ -> (State (\s -> ((), s)))) a
                     --       sb = (\s -> ((), s))
                    in sb s1)
                     -- ((), i+1)
== State (\s0 -> ((), i+1))
== State (\_  -> ((), i+1))
```

Plugging that back in, we have

```haskell
State (\s -> (s,s)) >>= \i -> State (\_ -> ((), i+1))
```

Which we can simplify in the same way:

```haskell
    (>>=) (State (\s -> (s,s))) (\i -> State (\_ -> ((), i+1))) =
      State (\s0 -> let (a, s1)  = (\s -> (s,s)) s0
                     -- (a, s1)  = (s0, s0)
                        State sb = (\i -> State (\_ -> ((), i+1))) a
                     --       sb = (\_ -> ((), s0+1))
                    in sb s1)
                     -- ((), s0+1)
 == State (\s0 -> ((), s0+1))
 == State (\i  -> ((), i+1))
```

Finally, we have

```haskell
    exec (State (\i -> ((), i+1))) 0
 == snd $ runState (State (\i -> ((), i+1))) 0
 == snd $ (\i -> ((), i+1)) 0
 == snd $ ((), 1)
 == 1
```

This is my favourite thing about Haskell: the fact that it is built on
abstractions that can be reasoned about in such a rigorous manner.
