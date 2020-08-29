--------------------------------------------------------------------------------
title: Peanut Butter and Jelly Sandwiches
published: 2020-09-01
tags: haskell, programming
--------------------------------------------------------------------------------

I've been trying to figure out exactly what I think the best thing about
statically-typed purely functional programming, Haskell in particular, is. It's
not laziness or monads, although both are wonderful! My current best answer is
that it is a combination of three different features that together encourage
simple, maintainable, and robust code:

1. Pattern matching
2. Algebraic data types
3. Pure functions

I like to think of these as analogous to a peanut butter and jelly sandwich (or
vegemite on toast, which is really well-buttered toast with only a thin layer
of vegemite): pattern matching and algebraic data types pair perfectly, and
pure functions are the foundation that they need to be maximally effective.

What do I mean by this? Allow me to illustrate with some Haskell code for my
imaginary tuck shop that sells only Twistys, Grain Waves, and Shapes:

```haskell
data Snack
    = Twistys TwistysFlavour
    | GrainWaves GrainWavesFlavour
    | Shapes ShapesFlavour

data TwistysFlavour
    = Cheese
    | Chicken

data GrainWavesFlavour
    = Original
    | SweetChilli
    | SourCreamAndChives

data ShapesFlavour
    = Barbecue
    | ChickenCrimpy
    | Pizza
```

More specifically
