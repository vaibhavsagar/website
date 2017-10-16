--------------------------------------------------------------------------------
title: 'return :: a -> RC a'
published: 2017-10-16
tags: programming, RC
--------------------------------------------------------------------------------

```haskell
instance Functor RC where
    fmap :: (a -> b) -> RC a -> RC b
```

```haskell
instance Applicative RC where
    pure  :: a -> RC a
    (<*>) :: RC (a -> b) -> RC a -> RC b
```

```haskell
instance Monad RC where
    return :: a -> RC a
    (>>=)  :: RC a -> (a -> RC b) -> RC b
```
