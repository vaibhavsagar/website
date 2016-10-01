Title: Code Is Data
Date: 2016-09-29
Category: programming
Status: draft

This is an attempt to recreate a blog post I read a few years ago that I can't
seem to find again. If you know the one I'm referring to, please link me!

# Cons Cells

A cons cell is a way of representing a pair in memory, and it has two
operations, `car` and `cdr`, to access the first and second elements of the
pair respectively. In Python, this could be represented as follows:

```python
cons = lambda a: lambda b: (a, b)
car  = lambda cell: cell[0]
cdr  = lambda cell: cell[1]
```

# Lists

It turns out that cons cells are all you need to implement singly linked lists.
Most famously, lists in Lisp are implemented as a series of cons cells chained
together. Using our above definitions in Python, the list `[0, 1, 2, 3]` would
be represented as (0, (1, (2, (3, ())))). In code, this would be:

```python
cons(0)(cons(1)(cons(2)cons(3)(())))
```

A list in Lisp is either empty (represented above by `()`) or a cons cell where
the first element of the pair is the head of the list and the second element of
the pair is the rest of the list. Another way of visualising the above list is
as follows:

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
