--------------------------------------------------------------------------------
title: The Rent Is Too Damn High! Succinct Data Structures for Fun and Profit
published: 2019-04-01
tags: programming
--------------------------------------------------------------------------------

Our data takes up too much space! Consider the linked list:

(picture of a linked list)

No matter what it stores, we need to allocate another 64 bits per node in order
to keep track of where the next one is! This is especially absurd if we
consider the case of a linked list of booleans. We only need one bit to store
each element, but (after taking into account the overhead) storage requirements
of each node are at least 65 bits, which is 65x more space than we would
ideally use.

(picture of the same data as an array)

Fortunately, we don't have to use a linked list to store this sequence. Instead
we can use an array, which is a contiguous series of memory addresses. Now
instead of explicitly keeping track of where the next element is, that
information is implicit in the memory layout of the data. We have achieved
ideal space usage! On the other hand, modifying our data is tricky, whereas
with a linked list representation all we have to do is change a few pointers.

Replacing a linked list with an array is pretty straightforward, but what do we
do if we have a more interesting data structure, such as a tree?

(picture of a tree with at least one node that has >2 children)

If we had a complete binary tree, we could store it in breadth-first order,
again implicitly encoding the parent-child relationships in the memory layout.
With any other kind of tree, though, we can't do this.

The existence of Lisp suggests one approach. We could perform a depth-first
preorder traversal of the tree, producing a `(` when going from a parent to a
child, and a `)` when returning to a parent from a child. This results in `2n`
parentheses for a tree with `n` nodes, and is known as a Balanced Parentheses
representation.

(balanced parentheses representation of above tree)

The fun really begins when we replace `(` with 1 and `)` with 0 to get a
bitvector. This can be stored extremely efficiently as machine words. We can
operate on this bitvector using `rank` and `select` operations. `rank(i)`
counts the number of occurrences of a symbol upto and including a particular
index `i`, and `select(i)` provides the index where the `i`th symbol is
located.
