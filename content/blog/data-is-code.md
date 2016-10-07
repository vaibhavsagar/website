Title: Data Is Code
Date: 2016-09-29
Category: programming

> Code is just smart data, and data is just dumb code.
>
> - _Structure and Interpretation of Computer Programs_

I'm going to try to convince you that data is code. Strap yourselves in!

A cons cell is a way of representing a pair, and it has two operations, [`car`
and `cdr`](https://en.wikipedia.org/wiki/CAR_and_CDR), to access the first and
second elements of the pair respectively. In Python, we could construct and
access cons cells as follows:

```python
cons = lambda a: lambda b: (a, b)
car  = lambda cell: cell[0]
cdr  = lambda cell: cell[1]
```

Let's create a pair and get the second element:

```python
>>> p = cons(1)(cons(2)(()))
>>> car(cdr(p))
2
```

It turns out that cons cells are all you need to implement singly linked lists.
For example, lists in Lisp are implemented as a series of nested cons cells. A
list is either empty (represented above by `()`) or a cons cell where the first
element of the pair is the head of the list and the second element of the pair
is the rest of the list. Using our above definitions in Python, the list `[0,
1, 2, 3]` would be represented as (0, (1, (2, (3, ())))).  In code, this would
be:

```python
cons(0)(cons(1)(cons(2)(cons(3)(()))))
```

One way of visualising the above list is as follows:

```
  cons
  / \
 0 cons
   / \
  1 cons
    / \
   2 cons
     / \
    3  ()
```

Suppose we wanted to access the second element of this list. We could express
this as the `car` of the `cdr` of this list. In code:

```python
>>> example = cons(0)(cons(1)(cons(2)(cons(3)(()))))
>>> car(cdr(example))
1
```

So far we've defined functions that wrap Python's tuples, and we're still in
data land, working with those tuples. But it doesn't have to be this way.

You may have noticed that my definitions above were all functions of one
argument. This was not entirely coincidental: cons cells as described above are
very similar to [Church
pairs](https://en.wikipedia.org/wiki/Church_encoding#Church_pairs), which are a
way of representing pairs in the lambda calculus. Let's make a minor change to
the above definitions:

```python
pair = lambda a: lambda b: lambda f: f(a)(b)
fst  = lambda p: p(lambda a: lambda b: a)
snd  = lambda p: p(lambda a: lambda b: b)
nil  = lambda a: a
```

We can recreate the pair above:

```python
>>> p = pair(1)(pair(2)(nil))
>>> fst(snd(p))
2
```

Although `p` is a function, it is representing the same data as above.

As an aside, the functions we use in `fst` and `snd` are [Church
booleans](https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans) that
encode `true` and `false` in the lambda calculus. We can rewrite our definitions
with this new knowledge:

```python
true  = lambda a: lambda b: a
false = lambda a: lambda b: b
fst   = lambda p: p(true)
snd   = lambda p: p(false)
```

Although our representation has changed, we can still perform the same
operations as before to get the second element of our list:

```python
>>> example = pair(0)(pair(1)(pair(2)(pair(3)(nil))))
>>> fst(snd(example))
1
```

Now we've moved to function land, and there are no tuples in sight. All we need
are functions of one argument, which is convenient because this is all that
lambda calculus gives us. Fortunately, lambda calculus is [Turing
complete](https://en.wikipedia.org/wiki/Turing_completeness) and therefore
enough.

Our code is represented as bits in memory i.e. data, but our data can be
represented as functions i.e. code. You might say that bits and bytes are the
lowest level representation and therefore somehow more fundamental than code,
but even our data is just an undistinguished pattern of 1s and 0s without some
code to make sense of it. It really is turtles all the way down.

There are much more comprehensive treatments of this material available online.
I'm aware of [Matt Might's](http://matt.might.net/articles/js-church/) and
[James Tauber's](http://jtauber.com/blog/2008/11/26/church_encoding_in_python/).
