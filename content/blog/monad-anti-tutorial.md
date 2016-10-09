Title: Monad Anti-tutorial
Date: 2016-10-08
Category: programming
Status: draft

Like so many before me, I feel the primal urge to inflict yet another (Haskell)
monad tutorial on an innocent and unsuspecting universe. For a host of reasons,
this is a bad idea. One reason is that I'm not going to do a better job of
explaining monads than everyone else. However, I probably am better at not
explaining monads, which is why you get an anti-tutorial. I might try to
explain some other stuff though.

What is a monad (in Haskell)? `Monad` is a typeclass , which is very much like
an interface in many other languages. Because Haskell is not object-oriented,
it has types conform to interfaces instead of objects (which it doesn't have),
and each type can declare that it implements a particular typeclass. This
involves supplying definitions for each function that the typeclass specifies.
`Monad` looks very much like this:

```haskell
class Monad m where
    return :: a   -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
```

There are two things I think are confusing about this definition:

1. `return` is a regular function whose meaning is unrelated to the `return`
   statement in most other programming languages.

2. Haskell allows you to define infix operators by surrounding them in
   parentheses, and this one looks a bit odd and vaguely threatening.

There are some laws that well-behaved implementations are supposed to observe:

```haskell
return a >>= f           =  f a             -- (1)
m >>= return             =  m               -- (2)
m >>= (\x -> f x >>= g)  =  (m >>= f) >>= g -- (3)
```

I won't explain these, but I will point them out.

And that's all.

Really.

So what's all the hype about?

Haskell has syntax sugar for this particular typeclass, which is probably one
reason. This syntax sugar is known as `do`-notation. It allows you to write
code that looks like

```haskell
f a b = do
    a' <- a
    b' <- b
    return (a' + b')
```

that then gets rewritten to

```haskell
f a b =
    a >>= \a' ->
    b >>= \b' ->
    return (a' + b')
```

If you want to define variables in between you can use

```haskell
g a b = do
    a' <- a
    let a'' = a' + 1
    b' <- b
    return (a'' + b')
```

and this becomes

```haskell
g a b =
    a >>= \a' ->
    let a'' = a' + 1 in
    b >>= \b' ->
    return (a'' + b)
```

and if you don't care about the variable on the left hand side of the `<-`, you
can omit it like

```haskell
h a b = do
    a
    b' <- b
    return b'
```

which desugars to

```haskell
h a b =
    a >>= \_  ->
    b >>= \b' ->
    return b'
```

I think it's worth spending time on understanding `do` notation and converting
between the sugared and desugared representations because:

1. Familiarity with `do` notation will allow you to effectively use monads,
   whether or not you feel you understand them.

2. Doing this will teach you (or at least help you learn) the monad laws.

But enough about that. Let's walk through some contrived examples to see how
this works in practice!

I'd like to start with the `Maybe` type, which allows us to work with possibly
`null` values in a way I think is better than most other approaches. The
implementation (or `instance`) of `Monad` looks like

```haskell
instance Monad Maybe where
    return v         = Just v
    (>>=) Nothing  _ = Nothing
    (>>=) (Just v) f = f v
```

and in use it looks like

```haskell
λ> f (Just 1) (Just 2)
Just 3
λ> f Nothing (Just 2)
Nothing
λ> f (Just 1) Nothing
Nothing
λ> g (Just 1) (Just 2)
Just 4
```

This captures the idea that if any of the parameters is `Nothing`, the overall
result should also be `Nothing`. The cool part is that we don't need to worry
about this when defining `f`, because the underlying implementation takes care
of it for us.

A second example is the `List` type. A valid instance looks like

```haskell
instance Monad [] where
    return v   = [v]
    (>>=)  v f = concatMap f v
```

and you would use this like


```haskell
λ> f [1, 2] [2, 3, 4]
[3, 4, 5, 4, 5, 6]
λ> f [] [2, 3, 4]
[]
λ> g [1, 2] [2, 3, 4]
[4,5,6,5,6,7]
```

Where a `Maybe` can be one of two possible values, a `List` is an arbitrary
number of values.

So far both examples have been a bit box-like, but monads are more general than
that. Let's look at the `Reader` type, which allows us to `ask` for some value
that can be passed in later (very much like a function):

```haskell
data Reader e a = Reader {runReader :: e -> a}

ask = Reader (\e -> e)

instance Monad (Reader e) where
   return a           = Reader (\_ -> a)
   (>>=) (Reader r) f = Reader (\e -> runReader (f (r e)) e)
```

This one is a bit more interesting to use.

```haskell
x = do
   value <- ask
   return (value + 1)

y = do
   value <- ask
   return (value + 2)

λ> runReader (f x y) 1
5
λ> runReader (f x y) 2
7
λ> runReader (g x y) 1
6
```

Finally, let's look at the `IO` type, which I find a bit scary. I don't really
understand how it's implemented. so I'll be skipping that section. Fortunately,
we know how to use it, because we know the interface, so let's do that.

```
getInt = do
   input <- h (putStr "Enter integer: ") getLine
   let int = read input :: Int
   return int

λ> f getInt getInt
Enter integer: 1
Enter integer: 2
3
λ> g getInt getInt
Enter integer: 1
Enter integer: 2
4
```

Okay, enough examples. As you can see, we've used the same interface to work
deal with failure, an arbitrary number of values, an extra parameter, and
input/output. There are many useful monad instances in Haskell, from container
types such as Set and Map to abstractions such as Writer and State, through to
control flow as implemented by the Cont monad.

I don't think the answer to the question "what is a monad?" is as useful as
learning the mechanics of working with them, even ones you don't fully
understand. If you can make your type conform to the typeclass, Haskell will
give you a pretty general and flexible API to work with it. As far as I'm
concerned, this is the point of monads in Haskell.
