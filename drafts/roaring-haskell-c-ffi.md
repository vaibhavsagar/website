--------------------------------------------------------------------------------
title: Roaring with Haskell's C FFI
published: 2023-11-30
tags: haskell, programming
--------------------------------------------------------------------------------

Recently I thought it would be fun to try writing Haskell bindings for
a C library, which for a long time has seemed especially intimidating to me.
I've been intrigued by [roaring bitmaps](https://roaringbitmap.org/) since
I first heard about them and unfortunately there doesn't seem to be a native
Haskell implementation, but there is one in C called
[CRoaring](https://github.com/RoaringBitmap/CRoaring) that I decided to bind
to. The specific library isn't that important here but it turned out that
I accidentally made a good choice because CRoaring can be
[amalgamated](https://github.com/RoaringBitmap/CRoaring#amalgamating) into
a `roaring.c` and a `roaring.h` file for easy inclusion in other projects, and
it's usable without trying to marshall the data structure itself between
Haskell and C land so I was able to avoid opening that can of worms.

[Here's](https://github.com/vaibhavsagar/experiments/tree/cd99b53874efd8a27e256ff5eb012060277ae473/harimau)
what I came up with ("harimau" is Indonesian for "tiger").

# Bad ideas

While looking for references to FFI best practices and working on my own
I found myself going against the grain on numerous occasions. I thought it
might be useful to mention a few of these instances, why I chose to do things
the way I did, and why I wouldn't recommend my approach to most people using
the FFI for Real Work™.

## `ForeignFunctionInterface` instead of `CApiFFI`

A recent post on the [GHC developers'
blog](https://www.haskell.org/ghc/blog/20210709-capi-usage.html) has a simple,
clear summary:

> tl;dr: When importing system libraries we strongly recommend that users use GHC’s `capi` calling convention.

However, the actual recommendation is a little more nuanced:

> It is justifiable to use `ccall` to avoid this runtime cost in cases where the foreign function is shipped with a package’s `cbits`, where the calling convention is clear.

(The runtime cost mentioned is because the `capi` calling convention works by
constructing a C source file which defines a stub function over the desired
foreign import.)

As it happens, I *do* include the foreign function in my `cbits`, but in most
other situations it would make sense to follow this advice.

## `unsafe` instead of `safe` everywhere

## Writing bindings by hand instead of using a tool

# Good ideas

## Foreign pointers

## Faithful low-level bindings

## `coerce`
