--------------------------------------------------------------------------------
title: You Won’t Believe This One Weird CPU Instruction!
published: 2019-09-08
tags: programming
--------------------------------------------------------------------------------

_Translated to [Russian](https://pngset.com/ru-weird-cpu-instruction) by [Babur
Muradov](https://pngset.com/) and [Uzbek](https://pngflare.com/uz-popcount) by
[Leonid Popov](https://pngflare.com/)._

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

> It was almost a tradition that one of the first of any new faster CDC machine
> was delivered to a “good customer” - picked up at the factory by an anonymous
> truck, and never heard from again.

This makes for a great story, but what were they using it for?

One measure of information content is the [Hamming
weight](https://en.wikipedia.org/wiki/Hamming_weight), which is the number of
symbols in a string that are different from the zero-symbol of the alphabet.
For a binary string, this is exactly `popcount`!

[As explained here](http://www.talkchess.com/forum3/viewtopic.php?t=38521), the
NSA wanted to do cryptanalysis on intercepted messages, and since the CDC 6000
had 60-bit words, one word was enough to store most alphabets they were
interested in. They were able to:

1. Split a message into lines
2. Set a bit for each unique character they encountered per line
3. Use `popcount` to count the distinct characters
4. Use the count as a hash for further cryptanalysis

Curiously, `popcount` seems to have disappeared from instruction sets between
the mid-1970s and the mid-2000s, so there has to be more to it than
cryptographic applications to explain its return. What else can it be used for?

#### Error Correction

Related to the concept of Hamming weight is [Hamming
distance](https://en.wikipedia.org/wiki/Hamming_distance), which is the number
of differing positions between two strings of identical length. For two binary
strings `x` and `y`, this is just the `popcount` of them XORed together. For
example:

```default
00100110
01100000 ^
--------
01000110

popcount(01000110) = 3
```

For telecommunications applications, this helps us calculate the signal
distance, where a known word is sent over the wire and the number of flipped
bits are counted to provide an estimate of the error introduced by transmission.

We can then design an [error-correcting
code](https://en.wikipedia.org/wiki/Hamming_distance#Error_detection_and_error_correction)
accordingly, e.g. if we want to be robust against up to 2 flipped bits, our
code words need to differ in Hamming distance by at least 5.

#### Binary Convolutional Neural Networks

And now for something completely different: binary convolutional neural
networks! But first, what are they?

- Binary means that we're using matrices consisting of only the values +1 (coded
as `1`) and -1 (coded as `0`), as opposed to 32-bit floating-point values.
- Convolutional means matrix multiplication is involved?
- Neural networks are systems inspired by animal brains (I'm a bit hazy on
this part).

In summary, we have to do binary matrix multiplication. But what's special
about binary matrices?

Ordinary matrix multiplication on 32-bit values is a good fit on desktop
computers with powerful CPUs and GPUs, but increasingly we also want to do
useful work on smaller and simpler devices, such as smartphones, routers,
smartwatches, etc. We can decompose these more complex matrices into layers of
binary matrices, and these resulting matrices are so much easier to store and
operate on that we are better off even though there are more layers.

Where does `popcount` come into play? It's used to calculate the dot product of
two binary matrices:

```default
a = xnor(x, y)
b = popcount(a)
c = len(a)
dot(x, y) = 2 × b − c
```

More details are available
[here](https://sushscience.wordpress.com/2017/10/01/understanding-binary-neural-networks/)
and
[here](https://developer.apple.com/documentation/metalperformanceshaders/mpscnnbinaryconvolution).

#### Chess Programming

Many chess programs store data using a
[bitboard](https://www.chessprogramming.org/Bitboards) representation, which
conveniently fits into a 64-bit word. [Population
Count](https://www.chessprogramming.org/Population_Count) has been used to
perform meaningful operations with this representation, such as calculating the
[mobility](https://www.chessprogramming.org/Mobility#Mobility_with_Bitboards)
of a piece.

#### Molecular Fingerprinting

This is related to the notion of Hamming distance above: molecules are hashed
in some way and compared (with `popcount`) to determine how similar they are.
More details on that
[here](http://www.dalkescientific.com/writings/diary/archive/2008/06/26/fingerprint_background.html).

#### Hash Array Mapped Tries

This is where I first learned about `popcount`! The HAMT is a data structure
([pioneered by Phil
Bagwell](https://lampwww.epfl.ch/papers/idealhashtrees.pdf)) that can store a
very large number of values (usually 32 or 64) in an array at each node of the
trie. However, allocating memory for a 32 or 64-element array every time can be
incredibly wasteful, especially if the array only actually contains a handful
of elements. The solution is to add a bitmask in which the number of bits that
are set corresponds to the number of elements in the array, which allows the
array to grow and shrink as required. Calculating the index for a given element
efficiently can then be done using `popcount`. You can learn more about how
they work from [this blog post](/blog/2018/07/29/hamts-from-scratch/), where I
implement them myself.

#### Succinct Data Structures

This is an exciting new area of research that focuses on how to store data in
as little space as possible, without having to decompress it in order to do
useful work. One technique is to think in terms of arrays of bits (bitvectors), which can be
queried using two operations:

- `rank(i)` counts the number of bits set upto the `i`th index in the bitvector
- `select(i)` finds the index where the `i`th ranked bit is set

Making these operations efficient on large bitvectors requires constructing an
index and using it effectively, both involving `popcount`. There's [a good
overview of the RRR index here](https://alexbowe.com/rrr/), and as far as I can
tell the current state-of-the-art approach is described in [Space-Efficient,
High-Performance Rank & Select Structures on Uncompressed Bit
Sequences](http://www.cs.cmu.edu/~./dga/papers/zhou-sea2013.pdf).

#### Compiler Optimisations

`popcount` has become so pervasive that both
[GCC](https://godbolt.org/z/JUzmD8) and [Clang](https://godbolt.org/z/AVqMGl)
will detect an implementation of `popcount` and replace it with the built-in
instruction. Imagine Clippy going "I see you are trying to implement
`popcount`, let me go ahead and fix that for you"! The relevant LLVM code is
[here](https://github.com/llvm-mirror/llvm/blob/f36485f7ac2a8d72ad0e0f2134c17fd365272285/lib/Transforms/Scalar/LoopIdiomRecognize.cpp#L960).
Daniel Lemire points to this as an example of [the surprising cleverness of
modern
compilers](https://lemire.me/blog/2016/05/23/the-surprising-cleverness-of-modern-compilers/).

#### Conclusion

From beginnings shrouded in mystery, `popcount` has emerged as a generally
useful, if slightly unusual, CPU instruction. I love how it ties together such
different fields of computing, and I wonder how many other similarly weird
instructions are out there. If you have a favourite, I'd love to hear about it!
