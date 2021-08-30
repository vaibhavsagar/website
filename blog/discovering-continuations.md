--------------------------------------------------------------------------------
title: Discovering Continuations with Typed Holes
published: 2017-05-22
tags: haskell, programming, monads
--------------------------------------------------------------------------------

I've been trying to wrap my head around continuations for a while. I was reading [David Luposchainsky's excellent article](https://github.com/quchen/articles/blob/master/cont_monad.md) on them and playing with his definitions in an IHaskell notebook when I found that typed holes are an excellent hammer to attack this particular nail with.

If you haven't encountered them before, [typed holes](https://wiki.haskell.org/GHC/Typed_holes) are a feature where you put one or more variables starting with `_` on the right hand side of a definition to get GHC to tell you the type of the value that fits in that hole, and you can narrow the hole as necessary to get the type of a subexpression until you have enough information to complete the definition. I like to think of this as a way of collaboratively filling in a definition with the compiler, instead of my usual approach which is to write a definition, listen carefully to GHC's complaints, and amend my definition accordingly. Typed holes are fully supported by GHCi and the full suite of editor integrations, but I personally find the edit/reload/squint cycle more tedious than coming up with the definition in IHaskell and then moving it to a module and adding type signatures after I'm satisfied that it works.

IHaskell has HLint integration and will suggest corrections for my intermediate cells unless I turn that off:


```haskell
:option no-lint
```

There's a useful GHC extension called `InstanceSigs` that will allow me to annotate my typeclass instances with their type signatures, so I'll turn that on:


```haskell
{-# LANGUAGE InstanceSigs #-}
```

I think his type definition makes an excellent starting point:


```haskell
newtype Cont r a = Cont { (>>-) :: (a -> r) -> r }
```

This defines a type `Cont` with an infix constructor `>>-` (that looks suspiciously similar to `>>=`) that takes a function from `a` to `r` and provides an `r`. One intuition for what this means is that a value of this type knows about an `a` but for whatever reason refuses to be upfront about it and demands to know what you're going to do with it and then does it for you, providing you with a final result `r`. Another intuition is that this is a generalisation of callbacks: a value of this type expects a callback to utilise the `a`. Anyway, on to my favourite part of working with mysterious data types: defining `Functor`, `Applicative`, and `Monad` instances for them! If you've done this before, you'll know that these typeclasses have certain laws that their instances are meant to obey, and it turns out that this type is polymorphic enough that we can just follow the typed holes and the resulting definitions will be lawful. You don't have to take my word for it and should verify this for yourself, but I won't be discussing the laws here. Let's begin!


```haskell
instance Functor (Cont r) where
    fmap :: (a -> b) -> Cont r a -> Cont r b
    fmap f cont = _
```


    <interactive>:3:19: error:
        • Found hole: _ :: Cont r b
          Where: ‘b’ is a rigid type variable bound by
                   the type signature for:
                     fmap :: forall a b. (a -> b) -> Cont r a -> Cont r b
                   at <interactive>:2:13-44
                 ‘r’ is a rigid type variable bound by
                   the instance declaration
                   at <interactive>:1:10-25
        • In the expression: _
          In an equation for ‘fmap’: fmap f cont = _
          In the instance declaration for ‘Functor (Cont r)’
        • Relevant bindings include
            cont :: Cont r a (bound at <interactive>:3:12)
            f :: a -> b (bound at <interactive>:3:10)
            fmap :: (a -> b) -> Cont r a -> Cont r b (bound at <interactive>:3:5)


We didn't really need a typed hole to tell us this, but at least we know what we have to work with. We know we have to provide a `Cont` value, so let's narrow our typed hole that way.


```haskell
instance Functor (Cont r) where
    fmap :: (a -> b) -> Cont r a -> Cont r b
    fmap f cont = Cont $ _
```


    <interactive>:3:26: error:
        • Found hole: _ :: (b -> r) -> r
          Where: ‘b’ is a rigid type variable bound by
                   the type signature for:
                     fmap :: forall a b. (a -> b) -> Cont r a -> Cont r b
                   at <interactive>:2:13-44
                 ‘r’ is a rigid type variable bound by
                   the instance declaration
                   at <interactive>:1:10-25
        • In the second argument of ‘($)’, namely ‘_’
          In the expression: Cont $ _
          In an equation for ‘fmap’: fmap f cont = Cont $ _
        • Relevant bindings include
            cont :: Cont r a (bound at <interactive>:3:12)
            f :: a -> b (bound at <interactive>:3:10)
            fmap :: (a -> b) -> Cont r a -> Cont r b (bound at <interactive>:3:5)


The type of our hole is more helpful here. Now we know (if we were previously uncertain) that we somehow need to use `f` to turn the `a` into a `b`. We also know that `Cont` takes a parameter, let's add that in and see if it helps.


```haskell
instance Functor (Cont r) where
    fmap :: (a -> b) -> Cont r a -> Cont r b
    fmap f cont = Cont $ \k -> _
```


    <interactive>:3:32: error:
        • Found hole: _ :: r
          Where: ‘r’ is a rigid type variable bound by
                   the instance declaration
                   at <interactive>:1:10-25
        • In the expression: _
          In the second argument of ‘($)’, namely ‘\ k -> _’
          In the expression: Cont $ \ k -> _
        • Relevant bindings include
            k :: b -> r (bound at <interactive>:3:27)
            cont :: Cont r a (bound at <interactive>:3:12)
            f :: a -> b (bound at <interactive>:3:10)
            fmap :: (a -> b) -> Cont r a -> Cont r b (bound at <interactive>:3:5)


In general, we know all of our definitions will be of the form `Cont $ \k -> _` and that's a safe starting point. We now know that we need to use `k` on the result of applying `f` to some `a` to finally result in an `r`, but where does the `a` come from? The only thing we can do at this point is 'unwrap' the `cont` using `>>-`. What happens when we do that?


```haskell
instance Functor (Cont r) where
    fmap :: (a -> b) -> Cont r a -> Cont r b
    fmap f cont = Cont $ \k -> cont >>- _
```


    <interactive>:3:41: error:
        • Found hole: _ :: a -> r
          Where: ‘a’ is a rigid type variable bound by
                   the type signature for:
                     fmap :: forall a b. (a -> b) -> Cont r a -> Cont r b
                   at <interactive>:2:13-44
                 ‘r’ is a rigid type variable bound by
                   the instance declaration
                   at <interactive>:1:10-25
        • In the second argument of ‘>>-’, namely ‘_’
          In the expression: cont >>- _
          In the second argument of ‘($)’, namely ‘\ k -> cont >>- _’
        • Relevant bindings include
            k :: b -> r (bound at <interactive>:3:27)
            cont :: Cont r a (bound at <interactive>:3:12)
            f :: a -> b (bound at <interactive>:3:10)
            fmap :: (a -> b) -> Cont r a -> Cont r b (bound at <interactive>:3:5)


It looks like we might have everything we need to complete this definition! We can create a function of type `a -> r` by composing `k` and `f`. 


```haskell
instance Functor (Cont r) where
    fmap :: (a -> b) -> Cont r a -> Cont r b
    fmap f cont = Cont $ \k -> cont >>- (k . f)
```

It worked! This definition states that `fmap` works by creating a continuation that expects a callback of the new type. This is pretty exciting! Let's continue to `Applicative`.


```haskell
instance Applicative (Cont r) where
    pure :: a -> Cont r a
    pure a = Cont $ \k -> _
```


    <interactive>:3:27: error:
        • Found hole: _ :: r
          Where: ‘r’ is a rigid type variable bound by
                   the instance declaration
                   at <interactive>:1:10-29
        • In the expression: _
          In the second argument of ‘($)’, namely ‘\ k -> _’
          In the expression: Cont $ \ k -> _
        • Relevant bindings include
            k :: a -> r (bound at <interactive>:3:22)
            a :: a (bound at <interactive>:3:10)
            pure :: a -> Cont r a (bound at <interactive>:3:5)


That was pretty easy. We need an `r` and we have an `a` and a `k` that takes an `a` to an `r`.


```haskell
instance Applicative (Cont r) where
    pure :: a -> Cont r a
    pure a = Cont $ \k -> k a
```

This matches our intuition from above: creating a continuation involves hiding a value behind a function that can access it. On to `<*>`!


```haskell
instance Applicative (Cont r) where
    pure :: a -> Cont r a
    pure a  = Cont $ \k -> k a
    (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
    f <*> a = Cont $ \k -> _
```


    <interactive>:5:28: error:
        • Found hole: _ :: r
          Where: ‘r’ is a rigid type variable bound by
                   the instance declaration
                   at <interactive>:1:10-29
        • In the expression: _
          In the second argument of ‘($)’, namely ‘\ k -> _’
          In the expression: Cont $ \ k -> _
        • Relevant bindings include
            k :: b -> r (bound at <interactive>:5:23)
            a :: Cont r a (bound at <interactive>:5:11)
            f :: Cont r (a -> b) (bound at <interactive>:5:5)
            (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b (bound at <interactive>:5:7)


From above, we know we can 'unwrap' `Cont` values using `>>-`.


```haskell
instance Applicative (Cont r) where
    pure :: a -> Cont r a
    pure a  = Cont $ \k -> k a
    (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
    f <*> a = Cont $ \k -> f >>- _
```


    <interactive>:5:34: error:
        • Found hole: _ :: (a -> b) -> r
          Where: ‘a’, ‘b’ are rigid type variables bound by
                   the type signature for:
                     (<*>) :: forall a b. Cont r (a -> b) -> Cont r a -> Cont r b
                   at <interactive>:4:14-52
                 ‘r’ is a rigid type variable bound by
                   the instance declaration
                   at <interactive>:1:10-29
        • In the second argument of ‘>>-’, namely ‘_’
          In the expression: f >>- _
          In the second argument of ‘($)’, namely ‘\ k -> f >>- _’
        • Relevant bindings include
            k :: b -> r (bound at <interactive>:5:23)
            a :: Cont r a (bound at <interactive>:5:11)
            f :: Cont r (a -> b) (bound at <interactive>:5:5)
            (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b (bound at <interactive>:5:7)


Let's keep going.


```haskell
instance Applicative (Cont r) where
    pure :: a -> Cont r a
    pure a  = Cont $ \k -> k a
    (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
    f <*> a = Cont $ \k -> f >>- \f' -> a >>- \a' -> _
```


    <interactive>:5:54: error:
        • Found hole: _ :: r
          Where: ‘r’ is a rigid type variable bound by
                   the instance declaration
                   at <interactive>:1:10-29
        • In the expression: _
          In the second argument of ‘>>-’, namely ‘\ a' -> _’
          In the expression: a >>- \ a' -> _
        • Relevant bindings include
            a' :: a (bound at <interactive>:5:48)
            f' :: a -> b (bound at <interactive>:5:35)
            k :: b -> r (bound at <interactive>:5:23)
            a :: Cont r a (bound at <interactive>:5:11)
            f :: Cont r (a -> b) (bound at <interactive>:5:5)
            (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b (bound at <interactive>:5:7)


Perfect, we want an `r` and we have

- an `a` (`a'`)
- a function from `a` to `b` (`f'`)
- a function from `b` to `r` (`k`)

Let's put them together.


```haskell
instance Applicative (Cont r) where
    pure :: a -> Cont r a
    pure a  = Cont $ \k -> k a
    (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
    f <*> a = Cont $ \k -> f >>- \f' -> a >>- \a' -> k (f' a')
```

Okay, we unwrap the function and the argument and rewrap them in a fresh continuation, not too differently from how we defined `fmap`. Sweet! On to the big M!


```haskell
instance Monad (Cont r) where
    (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
    a >>= f = Cont $ \k -> _
```


    <interactive>:3:28: error:
        • Found hole: _ :: r
          Where: ‘r’ is a rigid type variable bound by
                   the instance declaration
                   at <interactive>:1:10-23
        • In the expression: _
          In the second argument of ‘($)’, namely ‘\ k -> _’
          In the expression: Cont $ \ k -> _
        • Relevant bindings include
            k :: b -> r (bound at <interactive>:3:23)
            f :: a -> Cont r b (bound at <interactive>:3:11)
            a :: Cont r a (bound at <interactive>:3:5)
            (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b (bound at <interactive>:3:7)


As before, our first order of business is to unwrap the `a`.


```haskell
instance Monad (Cont r) where
    (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
    a >>= f = Cont $ \k -> a >>- \a' -> _
```


    <interactive>:3:41: error:
        • Found hole: _ :: r
          Where: ‘r’ is a rigid type variable bound by
                   the instance declaration
                   at <interactive>:1:10-23
        • In the expression: _
          In the second argument of ‘>>-’, namely ‘\ a' -> _’
          In the expression: a >>- \ a' -> _
        • Relevant bindings include
            a' :: a (bound at <interactive>:3:35)
            k :: b -> r (bound at <interactive>:3:23)
            f :: a -> Cont r b (bound at <interactive>:3:11)
            a :: Cont r a (bound at <interactive>:3:5)
            (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b (bound at <interactive>:3:7)


We can apply `f` to this unwrapped value to get a continuation that we can unwrap again.


```haskell
instance Monad (Cont r) where
    (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
    a >>= f = Cont $ \k -> a >>- \a' -> f a' >>- \f' -> _
```


    <interactive>:3:57: error:
        • Found hole: _ :: r
          Where: ‘r’ is a rigid type variable bound by
                   the instance declaration
                   at <interactive>:1:10-23
        • In the expression: _
          In the second argument of ‘>>-’, namely ‘\ f' -> _’
          In the expression: f a' >>- \ f' -> _
        • Relevant bindings include
            f' :: b (bound at <interactive>:3:51)
            a' :: a (bound at <interactive>:3:35)
            k :: b -> r (bound at <interactive>:3:23)
            f :: a -> Cont r b (bound at <interactive>:3:11)
            a :: Cont r a (bound at <interactive>:3:5)
            (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b (bound at <interactive>:3:7)


We want an `r` and we have `k` and `f'`. Let's put them together!


```haskell
instance Monad (Cont r) where
    (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
    a >>= f = Cont $ \k -> a >>- \a' -> f a' >>- \f' -> k f'
```

And that's it! The [mother of all monads](http://blog.sigfpe.com/2008/12/mother-of-all-monads.html) boils down to some tedious and almost mechanical wrapping and unwrapping. I think it's cool how mundane it is.

Let's have a crack at something more involved. A lot of the magic in continuations is accessed via `callCC`, which takes a function and calls it with the current continuation, hence the name. How would we define it?


```haskell
callCC :: ((b -> Cont r a) -> Cont r b) -> Cont r b
callCC f = Cont $ \k -> _
```


    <interactive>:2:25: error:
        • Found hole: _ :: r
          Where: ‘r’ is a rigid type variable bound by
                   the type signature for:
                     callCC :: forall b r a. ((b -> Cont r a) -> Cont r b) -> Cont r b
                   at <interactive>:1:1-51
        • In the expression: _
          In the second argument of ‘($)’, namely ‘\ k -> _’
          In the expression: Cont $ \ k -> _
        • Relevant bindings include
            k :: b -> r (bound at <interactive>:2:20)
            f :: (b -> Cont r a) -> Cont r b (bound at <interactive>:2:8)
            callCC :: ((b -> Cont r a) -> Cont r b) -> Cont r b (bound at <interactive>:2:1)


Our definition involves `b`, but the only `b` we have available is wrapped up in `f`. We need to provide an argument of a certain type to `f`, and then unwrap the result of that? Time to bring out the big guns: multiple typed holes!


```haskell
callCC :: ((b -> Cont r a) -> Cont r b) -> Cont r b
callCC f = Cont $ \k -> f _1 >>- _2
```


    <interactive>:2:27: error:
        • Found hole: _1 :: b -> Cont r a
          Where: ‘b’, ‘r’, ‘a’ are rigid type variables bound by
                   the type signature for:
                     callCC :: forall b r a. ((b -> Cont r a) -> Cont r b) -> Cont r b
                   at <interactive>:1:1-51
          Or perhaps ‘_1’ is mis-spelled, or not in scope
        • In the first argument of ‘f’, namely ‘_1’
          In the first argument of ‘>>-’, namely ‘f _1’
          In the expression: f _1 >>- _2
        • Relevant bindings include
            k :: b -> r (bound at <interactive>:2:20)
            f :: (b -> Cont r a) -> Cont r b (bound at <interactive>:2:8)
            callCC :: ((b -> Cont r a) -> Cont r b) -> Cont r b (bound at <interactive>:2:1)
    <interactive>:2:34: error:
        • Found hole: _2 :: b -> r
          Where: ‘b’, ‘r’ are rigid type variables bound by
                   the type signature for:
                     callCC :: forall b r a. ((b -> Cont r a) -> Cont r b) -> Cont r b
                   at <interactive>:1:1-51
          Or perhaps ‘_2’ is mis-spelled, or not in scope
        • In the second argument of ‘>>-’, namely ‘_2’
          In the expression: f _1 >>- _2
          In the second argument of ‘($)’, namely ‘\ k -> f _1 >>- _2’
        • Relevant bindings include
            k :: b -> r (bound at <interactive>:2:20)
            f :: (b -> Cont r a) -> Cont r b (bound at <interactive>:2:8)
            callCC :: ((b -> Cont r a) -> Cont r b) -> Cont r b (bound at <interactive>:2:1)
          Valid hole fits include k :: b -> r (bound at <interactive>:2:20)


Great, `k` fits perfectly into the second hole. That was easy.


```haskell
callCC :: ((b -> Cont r a) -> Cont r b) -> Cont r b
callCC f = Cont $ \k -> f _ >>- k
```


    <interactive>:2:27: error:
        • Found hole: _ :: b -> Cont r a
          Where: ‘b’, ‘r’, ‘a’ are rigid type variables bound by
                   the type signature for:
                     callCC :: forall b r a. ((b -> Cont r a) -> Cont r b) -> Cont r b
                   at <interactive>:1:1-51
        • In the first argument of ‘f’, namely ‘_’
          In the first argument of ‘>>-’, namely ‘f _’
          In the expression: f _ >>- k
        • Relevant bindings include
            k :: b -> r (bound at <interactive>:2:20)
            f :: (b -> Cont r a) -> Cont r b (bound at <interactive>:2:8)
            callCC :: ((b -> Cont r a) -> Cont r b) -> Cont r b (bound at <interactive>:2:1)


We're being asked to provide a function that takes one argument and returns a continuation. Let's fill in the boilerplate and see where that takes us.


```haskell
callCC :: ((b -> Cont r a) -> Cont r b) -> Cont r b
callCC f = Cont $ \k -> f (\b -> Cont $ \k' -> _) >>- k
```


    <interactive>:2:48: error:
        • Found hole: _ :: r
          Where: ‘r’ is a rigid type variable bound by
                   the type signature for:
                     callCC :: forall b r a. ((b -> Cont r a) -> Cont r b) -> Cont r b
                   at <interactive>:1:1-51
        • In the expression: _
          In the second argument of ‘($)’, namely ‘\ k' -> _’
          In the expression: Cont $ \ k' -> _
        • Relevant bindings include
            k' :: a -> r (bound at <interactive>:2:42)
            b :: b (bound at <interactive>:2:29)
            k :: b -> r (bound at <interactive>:2:20)
            f :: (b -> Cont r a) -> Cont r b (bound at <interactive>:2:8)
            callCC :: ((b -> Cont r a) -> Cont r b) -> Cont r b (bound at <interactive>:2:1)


And we're done! We can get an `r` by applying `k` to `b`.


```haskell
callCC :: ((b -> Cont r a) -> Cont r b) -> Cont r b
callCC f = Cont $ \k -> f (\b -> Cont $ \k' -> k b) >>- k
```

A closer look at the definition reveals that `k'` is unused, and this function provides `f` with the option to exit early if desired, or continue as normal. There's a good explanation of why and how this works at [the aforementioned article](https://github.com/quchen/articles/blob/master/cont_monad.md#special-api-function-callcc).

Still a bit wary? That's fair. I like to poke at the definitions, [read the source](https://hackage.haskell.org/package/transformers-0.4.3.0/docs/src/Control-Monad-Trans-Cont.html), look at how Gabriella Gonzalez [explains it](http://www.haskellforall.com/2014/04/how-continuation-monad-works.html), and have a cup of tea and think about life for a while. Whatever works for you!

If you looked at the source, you might have noticed something interesting: The definition for the ContT monad transformer is identical! Here it is below.


```haskell
newtype ContT r m a = ContT { (>>-) :: (a -> m r) -> m r }

instance Functor (ContT r m) where
    fmap :: (a -> b) -> ContT r m a -> ContT r m b
    fmap f cont = ContT $ \k -> cont >>- (k . f)

instance Applicative (ContT r m) where
    pure :: a -> ContT r m a
    pure a  = ContT $ \k -> k a
    (<*>) :: ContT r m (a -> b) -> ContT r m a -> ContT r m b
    f <*> a = ContT $ \k -> f >>- \f' -> a >>- \a' -> k (f' a')

instance Monad (ContT r m) where
    (>>=) :: ContT r m a -> (a -> ContT r m b) -> ContT r m b
    a >>= f = ContT $ \k -> a >>- \a' -> f a' >>- \f' -> k f'
    
callCC :: ((b -> ContT r m a) -> ContT r m b) -> ContT r m b
callCC f = ContT $ \k -> f (\b -> ContT $ \k' -> k b) >>- k
```

I love being able to interact with these definitions like this. This is really how I want to program, and I'd encourage you to try it! The notebook is [here](https://github.com/vaibhavsagar/notebooks/blob/master/continuations/Continuation.ipynb) for you to play with if you have IHaskell set up. IHaskell isn't just useful for programming: I even used it to [write this blog post](https://github.com/vaibhavsagar/notebooks/blob/master/continuations/DiscoveringContinuationsWithTypedHoles.ipynb)!

I feel like I should end with something profound about continuations, but I'll instead link you to [this presentation by Tim Humphries](http://teh.id.au/posts/2017/05/10/lambdajam-slides/index.html) and once again nudge you to try typed holes the next time you're in a Haskell bind (pun very much intended).

Thanks to [Iain McCoy](https://twitter.com/imccoy), [Julia Evans](https://jvns.ca/), and [Carl Factora](https://ivanthetricourne.github.io/) for their feedback and suggestions on this post.
