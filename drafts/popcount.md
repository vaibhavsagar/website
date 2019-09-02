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

Binary means that we're using matrices consisting of only the values +1 (coded
as `1`) and -1 (coded as `0`), as opposed to 32-bit floating-point values.

Convolutional means matrix multiplication is involved?

Neural networks are systems inspired by animal brains (I'm a bit hazy on
this part).

In summary, we have to do binary matrix multiplication. But why?

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


