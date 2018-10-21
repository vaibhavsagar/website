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

Let's try to write that same program in Haskell with typed holes. To start,
let's create a file called `Main.hs` with the following contents:

```haskell
import Prelude hiding (map)

map :: (a -> b) -> [a] -> [b]
map f ls = _

plusOne :: Int -> Int
plusOne i = i + 1

main :: IO ()
main = print ((map plusOne [1, 2, 3]) == [2, 3, 4])
```

To automate the cycle of loading this file into GHCi every time we make a
change, we're going to use [`ghcid`](https://github.com/ndmitchell/ghcid),
which does essentially that and not much else. This is the result of running
`ghcid Main.hs`:

```default
Main.hs:4:12: error:
    • Found hole: _ :: [b]
      Where: ‘b’ is a rigid type variable bound by
               the type signature for:
                 map :: forall a b. (a -> b) -> [a] -> [b]
               at Main.hs:3:1-29
    • In the expression: _
      In an equation for ‘map’: map f ls = _
    • Relevant bindings include
        ls :: [a] (bound at Main.hs:4:7)
        f :: a -> b (bound at Main.hs:4:5)
        map :: (a -> b) -> [a] -> [b] (bound at Main.hs:4:1)
  |
4 | map f ls = _
  |            ^
```

I'd suggest ignoring the middle and instead focusing on the top, which tells us
the type of the hole, and the bottom, which tells us which bindings are in
scope that we can use to fill in the hole.

In this case we aren't learning anything new, and we already know that the hole
is of type `[b]`, but it's useful to know that our view of the world agrees
with GHC's. Of the bindings available to us, `ls` looks like the most
promising, and we can split it into one of two cases: an empty list, or some
element and the rest of the list. We can put a typed hole on the right hand
side of each case alternative:

```haskell
import Prelude hiding (map)

map :: (a -> b) -> [a] -> [b]
map f ls = case ls of
    [] -> _1
    x:xs -> _2

plusOne :: Int -> Int
plusOne i = i + 1

main :: IO ()
main = print ((map plusOne [1, 2, 3]) == [2, 3, 4])
```

And our `ghcid` output changes:

```default
Main.hs:5:11-12: error:
    • Found hole: _1 :: [b]
      Where: ‘b’ is a rigid type variable bound by
               the type signature for:
                 map :: forall a b. (a -> b) -> [a] -> [b]
               at Main.hs:3:1-29
      Or perhaps ‘_1’ is mis-spelled, or not in scope
    • In the expression: _1
      In a case alternative: [] -> _1
      In the expression:
        case ls of
          [] -> _1
          x : xs -> _2
    • Relevant bindings include
        ls :: [a] (bound at Main.hs:4:7)
        f :: a -> b (bound at Main.hs:4:5)
        map :: (a -> b) -> [a] -> [b] (bound at Main.hs:4:1)
  |
5 |     [] -> _1
  |           ^^
Main.hs:6:13-14: error:
    • Found hole: _2 :: [b]
      Where: ‘b’ is a rigid type variable bound by
               the type signature for:
                 map :: forall a b. (a -> b) -> [a] -> [b]
               at Main.hs:3:1-29
      Or perhaps ‘_2’ is mis-spelled, or not in scope
    • In the expression: _2
      In a case alternative: x : xs -> _2
      In the expression:
        case ls of
          [] -> _1
          x : xs -> _2
    • Relevant bindings include
        xs :: [a] (bound at Main.hs:6:7)
        x :: a (bound at Main.hs:6:5)
        ls :: [a] (bound at Main.hs:4:7)
        f :: a -> b (bound at Main.hs:4:5)
        map :: (a -> b) -> [a] -> [b] (bound at Main.hs:4:1)
  |
6 |     x:xs -> _2
  |             ^^
```

We haven't learned anything new about the types, but we can make progress
another way. The only sensible thing to put on the right hand side when given
an empty list is another empty list, and after splitting a list apart the most
reasonable thing to do is to combine two other things into a new list:

```haskell
import Prelude hiding (map)

map :: (a -> b) -> [a] -> [b]
map f ls = case ls of
    [] -> []
    x:xs -> _1:_2

plusOne :: Int -> Int
plusOne i = i + 1

main :: IO ()
main = print ((map plusOne [1, 2, 3]) == [2, 3, 4])
```

Now the output gets a little more interesting:

```default
Main.hs:6:13-14: error:
    • Found hole: _1 :: b
      Where: ‘b’ is a rigid type variable bound by
               the type signature for:
                 map :: forall a b. (a -> b) -> [a] -> [b]
               at Main.hs:3:1-29
      Or perhaps ‘_1’ is mis-spelled, or not in scope
    • In the first argument of ‘(:)’, namely ‘_1’
      In the expression: _1 : _2
      In a case alternative: x : xs -> _1 : _2
    • Relevant bindings include
        xs :: [a] (bound at Main.hs:6:7)
        x :: a (bound at Main.hs:6:5)
        ls :: [a] (bound at Main.hs:4:7)
        f :: a -> b (bound at Main.hs:4:5)
        map :: (a -> b) -> [a] -> [b] (bound at Main.hs:4:1)
  |
6 |     x:xs -> _1:_2
  |             ^^
Main.hs:6:16-17: error:
    • Found hole: _2 :: [b]
      Where: ‘b’ is a rigid type variable bound by
               the type signature for:
                 map :: forall a b. (a -> b) -> [a] -> [b]
               at Main.hs:3:1-29
      Or perhaps ‘_2’ is mis-spelled, or not in scope
    • In the second argument of ‘(:)’, namely ‘_2’
      In the expression: _1 : _2
      In a case alternative: x : xs -> _1 : _2
    • Relevant bindings include
        xs :: [a] (bound at Main.hs:6:7)
        x :: a (bound at Main.hs:6:5)
        ls :: [a] (bound at Main.hs:4:7)
        f :: a -> b (bound at Main.hs:4:5)
        map :: (a -> b) -> [a] -> [b] (bound at Main.hs:4:1)
  |
6 |     x:xs -> _1:_2
  |                ^^
```

Our first hole is now of type `b`, and we see that it's possible to get a value
of this type by applying `f` to `x`. The second hole is still of type `[b]`,
and we see that the most reasonable way to get a value of this type is to
recurse, using `map` with `f` and `xs`.

```haskell
import Prelude hiding (map)

map :: (a -> b) -> [a] -> [b]
map f ls = case ls of
    [] -> []
    x:xs -> f x: map f xs

plusOne :: Int -> Int
plusOne i = i + 1

main :: IO ()
main = print ((map plusOne [1, 2, 3]) == [2, 3, 4])
```

And `ghcid` is satisfied:

```default
All good (1 module, at <time>)
```

It compiles! But does it work?

```bash
$ runhaskell Main.hs
True
```

Fantastic! We were able to ask the compiler for hints and get useful answers
back. This is a gigantic improvement over anything I'm aware of in Python land.

Unfortunately, there is a catch: this program is too easy to break. Let me
demonstrate my favourite way:

```haskell
import Prelude hiding (map)

map :: (a -> b) -> [a] -> [b]
map f ls = []

plusOne :: Int -> Int
plusOne i = i + 1

main :: IO ()
main = print ((map plusOne [1, 2, 3]) == [2, 3, 4])
```

The astute reader will notice that this is the Haskell equivalent of the
obviously broken Python program we started with. Here is what `ghcid` has to
say:

```default
All good (1 module, at <time>)
```

So what's going on here? It turns out an empty list is a valid list of any
type. Is it a list of Strings? Yup. Is it a list of Ints? Sure!

One way of avoiding this class of incorrect program is to specify that the
input and the output lists should be of the same length. It's possible to do
this in Haskell, but it is a lot of work. Can we do better?

Enter Idris.

Idris is a functional programming language with a more sophisticated type
system than Haskell's. Unfortunately it trades off some type inference to
achieve this, so it's not strictly better than Haskell in every way. It has
excellent built-in editor support, and therefore has fancier typed holes!

Let's try to implement `map` again:

```idris
data Vect : Nat -> Type -> Type where
    Nil  : Vect 0 a
    (::) : a -> Vect length a -> Vect (1 + length) a

implementation (Eq a) => Eq (Vect l a) where
    (==) []      []      = True
    (==) (x::xs) (y::ys) = x == y && xs == ys

map : (a -> b) -> Vect length a -> Vect length b

plusOne : Int -> Int
plusOne i = i + 1

main : IO ()
main = printLn ((map plusOne [1, 2, 3]) == [2, 3, 4])
```

Instead of implementing `map` on lists, we'll use a more interesting type
called `Vect` which is essentially a list that knows about its length. A `Vect`
can either be `Nil` of length `0` or an element on the front of another `Vect`
of some length `length` giving us a new `Vect` of length `length + 1`. I've
also gone ahead defined equality on these `Vect`s because I use it in `main`.

Loading the file into the Idris REPL gives us access to the editor integration:

```bash
$ idris Main.idr
     ____    __     _
    /  _/___/ /____(_)____
    / // __  / ___/ / ___/     Version 1.3.0
  _/ // /_/ / /  / (__  )      http://www.idris-lang.org/
 /___/\__,_/_/  /_/____/       Type :? for help

Idris is free software with ABSOLUTELY NO WARRANTY.
For details type :warranty.
Type checking ./Main.idr
Holes: Main.map
*Main>
```

In vim, I can use `<localleader>d` with the cursor on `map` to fill in a
skeleton definition:

```idris
data Vect : Nat -> Type -> Type where
    Nil  : Vect 0 a
    (::) : a -> Vect length a -> Vect (1 + length) a

implementation (Eq a) => Eq (Vect l a) where
    (==) []      []      = True
    (==) (x::xs) (y::ys) = x == y && xs == ys

map : (a -> b) -> Vect length a -> Vect length b
map f x = ?map_rhs

plusOne : Int -> Int
plusOne i = i + 1

main : IO ()
main = printLn ((map plusOne [1, 2, 3]) == [2, 3, 4])
```
