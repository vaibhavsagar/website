Title: Monad Anti-tutorial
Date: 2016-10-12
Category: programming

I think becoming familiar with the mechanics of working with monads is more
important than the question of what a monad is. For this reason, I think monad
tutorials solve the wrong problem. I'm giving you a monad anti-tutorial
instead, where I don't try to explain what a monad is but I do try to show you
how to use them.

I'm assuming basic familiarity with Haskell's syntax, but even if you're not I
hope you'll still be able to follow this.

Let's start with a definition. `Monad` in Haskell is a typeclass, which is very
much like an interface in many other languages.  Because Haskell is not
object-oriented, it has types conform to typeclasses instead of objects
conforming to interfaces, and each type can declare that it implements a
particular typeclass by supplying definitions for each function that the
typeclass specifies. `Monad` looks very much like this:

```haskell
class Monad m where
    return :: a   -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
```

This means that for any type `m`, if you provide two function definitions
called `return` and `(>>=)` (pronounced 'bind') with the correct type
signatures, you've made a `Monad` instance!

There are two things I think are confusing about this definition:

1. `return` is a regular function whose meaning is unrelated to the `return`
   statement in most other programming languages.

2. Haskell allows you to define infix operators by surrounding them in
   parentheses, and this one looks a bit odd and vaguely threatening with its
   spiky edges.

There are some laws that well-behaved implementations are supposed to observe:

```haskell
return a >>= f           =  f a
m >>= return             =  m
m >>= (\x -> f x >>= g)  =  (m >>= f) >>= g
```

I include these because they're an important part of what a `Monad` is. These
laws aren't enforced by the typeclass definition so it is possible to define
unlawful instances. That's a very bad idea though.

And that's all. Really.

So what's all the hype about?

One reason might be that Haskell has syntax sugar for this particular
typeclass. This syntax sugar is known as `do`-notation, and it allows you to
write code that looks like

```haskell
foo a b = do
    a' <- a
    b' <- b
    return (a' + b')
```

that then gets rewritten to

```haskell
foo a b =
    a >>= \a' ->
    b >>= \b' ->
    return (a' + b')
```

If you want to define variables in between you can use

```haskell
bar a b = do
    a' <- a
    let a'' = a' + 1
    b' <- b
    return (a'' + b')
```

and this becomes

```haskell
bar a b =
    a >>= \a' ->
    let a'' = a' + 1 in
    b >>= \b' ->
    return (a'' + b)
```

Sometimes you don't care about the variable on the left hand side of the `<-`,
and you can omit it like

```haskell
baz a b = do
    a
    b' <- b
    return b'
```

which desugars to

```haskell
baz a b =
    a >>= \_  ->
    b >>= \b' ->
    return b'
```

I'll be using the above definitions in my examples, so if you find yourself
wondering where `bar` came from a bit later, check back here.

I think it's worth spending time on understanding `do` notation and converting
between the sugared and desugared representations because:

1. Familiarity with `do` notation will allow you to effectively use monads,
   whether or not you feel you understand them.

2. Doing this will teach you (or at least help you learn) the monad laws.

But enough about that. Let's walk through some contrived examples to see how
we can use the same typeclass (and the same functions) to do a bunch of very
different things!

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
λ> foo (Just 1) (Just 2)
Just 3
λ> foo Nothing (Just 2)
Nothing
λ> foo (Just 1) Nothing
Nothing
λ> bar (Just 1) (Just 2)
Just 4
```

This captures the idea that if any of the parameters is `Nothing`, the overall
result should also be `Nothing`. The cool part is that we don't need to worry
about this when defining `foo`, because the underlying implementation takes
care of it for us.

In a language like Python, we don't really have a way to abstract away the
fact that an input can be possibly null, and we have to specifically account
for this possibility. Similar code in Python would look like

```python
def foo(a, b):
   if a is not None and b is not None:
      return a + b
   else:
      return None
```

and this code would not work without modification for any of the following
examples.

A second example is the `List` type. A valid instance looks like

```haskell
instance Monad [] where
    return v   = [v]
    (>>=)  v f = concatMap f v
```

and you would use this like


```haskell
λ> foo [1, 2] [2, 3, 4]
[3, 4, 5, 4, 5, 6]
λ> foo [] [2, 3, 4]
[]
λ> bar [1, 2] [2, 3, 4]
[4,5,6,5,6,7]
```

Where a `Maybe` can be one of two possible values, a `List` is an arbitrary
number of values.

In Python this would be a list comprehension:

```python
def foo(a, b):
   return [a_+b_ for a_ in a for b_ in b]

```

which, again, is too specific to work with any of the other examples.

The `Maybe` type could be thought of as a box containing either one or no
elements, and similarly `List` may be thought of as a box containing an
arbitrary number of elements. It may be tempting to think of all monads as
boxes of some kind, but monads are more general than that. Let's look at the
`Reader` type, which allows us to `ask` for some value that can be passed in
later (in some circles this is called dependency injection):

```haskell
data Reader e a = Reader {runReader :: e -> a}

ask = Reader (\e -> e)

instance Monad (Reader e) where
   return a           = Reader (\_ -> a)
   (>>=) (Reader r) f = Reader (\e -> runReader (f (r e)) e)
```

I had to look this one up. I don't expect you to immediately understand this
implementation, my point is that this is bog-standard Haskell code. It's a bit
more interesting to use:

```haskell
x = do
   value <- ask
   return (value + 1)

y = do
   value <- ask
   return (value + 2)

λ> runReader (foo x y) 1
5
λ> runReader (foo x y) 2
7
λ> runReader (bar x y) 1
6
```

I have no idea how you'd do this in Python, but I can guarantee it's not
general either.

Finally, let's look at the `IO` type, which I find a bit scary. I don't really
understand how it's implemented. so I'll be skipping that section. Fortunately,
we know how to use it, because we are familiar with the interface, so let's go
straight to that.

```
getInt = do
   input <- baz (putStr "Enter integer: ") getLine
   let int = read input :: Int
   return int

λ> foo getInt getInt
Enter integer: 1
Enter integer: 2
3
λ> bar getInt getInt
Enter integer: 1
Enter integer: 2
4
```

Okay, enough examples. As you can see, we've used the same interface to deal
with failure, an arbitrary number of values, an extra parameter, and
input/output. There are many useful monad instances in Haskell, from container
types such as Set and Map to abstractions such as Writer and State, through to
control flow as implemented by the Cont monad.

If you can make your type conform to the typeclass, Haskell will give you a
pretty general and flexible API to work with it. Even if you don't completely
understand what's going on with a certain type, you know enough to use it based
on your knowledge of the interface. As far as I'm concerned, this is the point
of monads in Haskell.
