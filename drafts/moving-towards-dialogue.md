--------------------------------------------------------------------------------
title: Moving Towards Dialogue
published: 2018-09-25
tags: programming, haskell, idris
--------------------------------------------------------------------------------

_This blog post is essentially a transcript of [a 10-minute talk I gave at
!!Con](https://www.youtube.com/watch?v=0oo8wIi2qBE), the slides for which are
available [here](https://vaibhavsagar.com/presentations/typed-holes/)._

Let's talk about typed holes. What's a typed hole? A definition I like is _"a
placeholder for an expression with a known type and an unknown value"_.

Why are typed holes useful? Because they allow the language to help us write
programs! To demonstrate, let's look at some code in Python, a language that
lacks this feature.

Suppose I want to write a function that takes another function and a list and
applies the function to each element of that list. A first stab might look like
this:

```python
>>> def map(f, ls): return []
```

This is obviously wrong: this function ignores its arguments and gives us an
empty list each time. This is no good, but what's worse is that Python will
accept this definition without complaint. It doesn't seem to know (or care).

Let's try again. I hear the way to do things with lists in Python is to use a
list comprehension, so I make sure to use one of those this time:

```python
>>> def map(f, ls): [f(e) for e in ls]
...
>>> plusOne = lambda i: i + 1
>>> print(map(plusOne, [1, 2, 3]))
None
```

But this definition is still wrong, because I forgot to put a `return`
statement in! Again, Python will happily accept this.

I'm getting a bit frustrated at this point, so I look at how `map` is actually
implemented. It uses a generator comprehension instead of a list comprehension,
which makes it more general somehow, so I make sure to use that, and I remember
to put a `return` statement in this time:

```python
>>> def map(f, ls): return (f(e) for e in ls)
...
>>> print(map(plusOne, [1, 2, 3]))
<generator object map.<locals>.<genexpr> at 0x7ffb92103ca8>
```

And now instead of giving me something useful, this function gives me an
address in memory, which seems especially pointless.

I give up. This example seems contrived, but over the course of my relationship
with Python, which is more than a decade long now, I've seen the same dynamic
play out embarrassingly many times. How do I write programs that work? Surely
this is a simple question with a straightforward answer.

Indeed, the most common response I get when I ask this question is to write
tests. That sounds reasonable, so let's do that:

```python
>>> def map(f, ls):
...     print("Where are your tests now?")
...     return [2, 3, 4]
...
>>> map(plusOne, [1, 2, 3]) == [2, 3, 4]
Where are your tests now?
True
```

Okay, my test passes, so everything must be good, right?

I contend that tests are necessary, but not sufficient. They are problematic
for at least two reasons:

1. They are only as good as the specification they imply for how a program
   should behave. If that specification is incomplete or wrong, no amount of
   tests will guarantee software that works.
2. The language doesn't understand tests, by which I mean that it's extremely
   difficult to automatically go from a failing test to the part of the code
   where the error lives. The programmer has to do the legwork of tracking down
   the bug based on essentially one bit of information in many cases.

Fortunately, tests aren't the only specification of how a program should
behave. Types can also serve as specifications, and they have the advantage
that they are extremely well integrated into the language in a way tests
usually are not.

What does this mean in practice? Let's look at Haskell. Haskell is statically
typed, which means it expects to know the types of all the expressions in your
program at compile time. This is nothing special though, lots of other
languages also claim to be statically typed. What is special about Haskell
though is that it has type inference, which means that you don't have to
annotate expressions with their types most of the time because the compiler can
figure it out. If you put these two features together, you get typed holes!

Let's try to write that same program in Haskell:

```haskell

```
