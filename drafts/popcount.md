--------------------------------------------------------------------------------
title: You Won’t Believe This One Weird CPU Instruction!
published: 2019-07-04
tags: programming
--------------------------------------------------------------------------------

_This is a pseudo-transcript of [a presentation I did at !!Con
2019](https://www.youtube.com/watch?v=bLFqLfz2Fmc)._

Most CPU architectures in use today have an instruction called `popcount`,
short for "population count".  Here's what it does: it counts the number of set
bits in a machine word. For example (assuming 8-bit words for simplicity),
`popcount(00100110)` is `3` and `popcount(01100000)` is `2`.

You might be wondering, like I was, if there's more to this instruction, but
that's all it does! This doesn't seem very useful, right?

I thought this might be a recent addition for some hyperspecialised use case,
but it has in fact been present in CPU architectures since at least 1961:

- 1961: [IBM Stretch](https://en.wikipedia.org/wiki/IBM_7030_Stretch)
- 1964: [CDC 6000](https://en.wikipedia.org/wiki/CDC_6000_series)
- 1975: [Cray-1](https://en.wikipedia.org/wiki/Cray-1)
- 2005: [SPARC](https://en.wikipedia.org/wiki/SPARC)
- 2005: [ARM NEON](https://en.wikipedia.org/wiki/ARM_architecture#Advanced_SIMD_(NEON))
- 2007: [AMD K10](https://en.wikipedia.org/wiki/AMD_10h)
- 2008: [Intel Nehalem](https://en.wikipedia.org/wiki/Nehalem_(microarchitecture))

So what's going on?

#### The NSA Instruction

`popcount` is also known as "The NSA Instruction", and a [very entertaining
thread on
`comp.arch`](https://groups.google.com/forum/#!msg/comp.arch/UXEi7G6WHuU/Z2z7fC7Xhr8J)
discusses its uses inside and outside cryptography. It is rumoured that it was
originally added to CPU instructions at the behest of the NSA. As [this
archived email thread](http://cryptome.org/jya/sadd.htm) puts it:

> It was almost a tradition that one of the first of any new faster CDC machine was delivered to a “good customer” - picked up at the factory by an anonymous truck, and never heard from again.

This makes for a great story, but what were they using it for?
