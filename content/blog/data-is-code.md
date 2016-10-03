Title: Data Is Code
Date: 2016-09-29
Category: programming
Status: draft

This is an attempt to recreate a blog post I read a few years ago that I can't
seem to find again. If you know the one I'm referring to, please link me!

# Cons Cells

A cons cell is a way of representing a pair, and it has two operations, `car`
and `cdr`, to access the first and second elements of the pair respectively. In
Python, we could construct and access cons cells as follows:

```python
cons = lambda a: lambda b: (a, b)
car  = lambda cell: cell[0]
cdr  = lambda cell: cell[1]
```

# Lists

It turns out that cons cells are all you need to implement singly linked lists.
For example, lists in Lisp are implemented as a series of cons cells chained
together. Using our above definitions in Python, the list `[0, 1, 2, 3]` would
be represented as (0, (1, (2, (3, ())))). In code, this would be:

```python
cons(0)(cons(1)(cons(2)(cons(3)(()))))
```

A list is either empty (represented above by `()`) or a cons cell where the
first element of the pair is the head of the list and the second element of the
pair is the rest of the list. Another way of visualising the above list is as
follows:

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
example = cons(0)(cons(1)(cons(2)(cons(3)(()))))
print(car(cdr(example))) #=> 1
```

# Lambda calculus

You may have noticed that my definitions above were all functions of one
argument. This was not entirely coincidental: cons cells as described above are
very similar to Church pairs, which are a way of representing pairs in the
lambda calculus. A minor change to the above definitions and we have Church
pairs:

```python
pair = lambda a: lambda b: lambda f: f(a)(b)
fst  = lambda p: p(lambda a: lambda b: a)
snd  = lambda p: p(lambda a: lambda b: b)
```

As an aside, the functions we use in `fst` and `snd` are Church booleans that
encode true and false in the lambda calculus. We can rewrite our definitions
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
example = pair(0)(pair(1)(pair(2)(pair(3)(()))))
print(fst(snd(example))) #=> 1
```

# Data Is Code

Our code is represented as bits in memory i.e. data, but our data can be
represented as functions i.e. code. You might say that bits and bytes are the
lowest level representation and therefore somehow more fundamental than code,
but even our data is just an interesting pattern of 1s and 0s without some code
to make sense of it. It really is turtles all the way down.
