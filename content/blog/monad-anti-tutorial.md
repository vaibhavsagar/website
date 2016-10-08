Title: Monad Anti-tutorial
Date: 2016-10-08
Category: programming
Status: draft

Like so many before me, I feel the primal urge to inflict yet another (Haskell)
monad tutorial on an innocent and unsuspecting universe. For a host of reasons,
this is a bad idea, so instead, you get an anti-tutorial. This is where I don't
even attempt to explain monads! I might try to explain some other stuff though.

What is a monad (in Haskell)? `Monad` is a typeclass , which is very much like
an interface in many other languages. Because Haskell is not object-oriented,
it has types conform to interfaces instead, and each type can declare that it
implements a particular typeclass. `Monad` looks very much like this:

```haskell
class Monad m where
    return :: a   -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
```

There are some laws that well-behaved implementations are supposed to observe:

```haskell
return a >>= f           =  f a             -- (1)
m >>= return             =  m               -- (2)
m >>= (\x -> f x >>= g)  =  (m >>= f) >>= g -- (3)
```

Again, not explaining these, just pointing them out.

And that's all. Really.

So why the hype?

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

Let's walk through some contrived examples to see how this works in practice!

I'd like to start with the `Maybe` type, which represents the possibility of
failure. The implementation (or `instance`) of `Monad` looks like

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
```

This captures the idea that if any of the parameters is `Nothing`, the overall
result should also be `Nothing`. The cool part is that we don't need to worry
about this when defining `f`.

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
```
