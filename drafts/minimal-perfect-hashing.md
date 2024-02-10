--------------------------------------------------------------------------------
title: The Real Hash Was the Friends We Made along the Way
published: 2024-02-14
tags: programming
--------------------------------------------------------------------------------

When I lived in Singapore, I attended a fascinating talk at FOSSASIA 2018 about
[Indeed's fast and compact immutable key-value
stores](https://www.youtube.com/watch?v=8Zu-EVjN24s). In fact, if you listen
carefully during the Q&A session at the end, you can hear me ask some
not-very-good questions in an ill-advised and ultimately futile attempt to
relate to the speaker.

This was my first encounter with the concept of minimal perfect hashing.
Unfortunately for me, I found most of the existing literature so impenetrable
that I gave up on learning more. [Hash, displace, and
compress](https://cmph.sourceforge.net/papers/esa09.pdf)? [Hypergraph
peeling](https://cmph.sourceforge.net/papers/wads07.pdf)? Eventually I found
a suitable entry point: [Fast and scalable minimal perfect hashing for massive
key sets](https://arxiv.org/abs/1702.03154).

## Minimal perfect hashing

Let's start with what minimal perfect hashing is:

### Hashing

One definition of hashing is a process that converts some key to a value of
some fixed size (e.g. an integer). We can think of this in terms of a hash
*function* that takes some input and produces an integer as output.

### Perfect

In practice, sometimes these hash functions produce the same output for
different inputs, known as a *hash collision*. This is pretty annoying and
causes lots of problems, and it would be nice if we could guarantee that
distinct inputs always hash to different values, i.e. that the function is
[*injective*](https://en.wikipedia.org/wiki/Injective_function). Hash functions
with this useful property are known as *perfect hash functions*. This requires
all possible inputs to be known in advance.

### *Minimal* perfect hashing

Bringing it all together, a *minimal perfect hash* function is one that has no
gaps in its outputs, i.e. it maps $$n$$ different inputs to $$n$$ consecutive
integers, e.g. $[0..n)$ or $[1..n]$. It's important to note that *minimal*
does not imply anything about the space or time complexity of these functions,
e.g. it would be totally valid to have an internal hashtable that maps each
input to a distinct integer without gaps and then use that to implement our
hash function. In practice, however, we want these functions to be as efficient
as possible to construct, store, and use, and this is an active area of
research.

You'd probably want to use a minimal perfect hash when

- all possible keys are known in advance
- the set of keys doesn't change
- space is at a premium

One attractive property of a minimal perfect hash function is that you can use
it to create a minimal perfect hash *table* by associating it with an array
where each value's index is the hash of the corresponding key.

## How it works

The approach used in the paper is based on cascading collisionless bitarrays,
as illustrated here:

![*Cascading Collisionless Bitarrays*](/images/cascading-collisionless-arrays.svg)

In the example, keys $k_1$ to $k_6$ are hashed and positions where there are no
collisions are recorded. The keys that collide at each level are removed and
retried at the next level until all the keys are used. For the first bitarray
$A_0$, $k_3$ and $k_6$ do not collide when using the hash function $h_0$. For
the next bitarray $A_1$, $k_1$ and $k_5$ do not collide when using $h_1$.
Finally for $A_2$, $k_2$ and $k_4$ do not collide using $h_2$ and we have no
more keys left. To compute the hash for a key, in this example $k_2$, we find
the position where $A_n[h_n(k_2)] \equiv 1$ and count the number of `1`s at or
preceding this position, also known as the *rank*, which will always give us
a number $[1..n]$.

### Prerequisites

To implement this, we'll need

- a family of hash functions
- bitvectors supporting [$rank$ (and
  $select$)](https://en.wikipedia.org/wiki/Succinct_data_structure#Succinct_indexable_dictionaries)
  operations

For the hash functions, I used
[`hashWithSalt`](https://hackage.haskell.org/package/hashable-1.4.3.0/docs/Data-Hashable.html#v:hashWithSalt)
from the [`hashable`](https://hackage.haskell.org/package/hashable) package,
and for the bitvectors I used the
[`bv-little`](https://hackage.haskell.org/package/bv-little) package because
[past Vaibhav asked for `rank` and `select`
support](https://github.com/recursion-ninja/bv-little/issues/3).

### Pseudocode

At a high level, this is what the construction algorithm looks like:

1. Repeat steps 2-4 until the maximum level is reached or we have no more keys
2. Hash each key to a number $i \in [0..n)$
3. If $bitvector[i]$ has not been set this iteration, set it to $1$, otherwise unset it
4. Remove all keys that have been set successfully
5. If there are any leftover keys, store them separately

### Hashing

As I mentioned previously, I used `hashWithSalt`:

```haskell
value = hashWithSalt currentLevel key `mod` (gamma * currentLength)
```

The role of `gamma` is to control the amount of "slack" in the bitvector, since
sometimes making it larger than strictly necessary can reduce the probability
of collisions. More on this later.

### Populating the bitvector
