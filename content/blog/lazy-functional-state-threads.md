Title: Lazy Functional State Threads
Date: 2017-06-19
Category: programming
Status: draft

A funny thing happened when I was writing my Imperative Haskell post: after
railing against Haskell's tendency to tell you to go off and read a paper when
introducing a library, I went off and read 'Lazy Functional State Threads' and
was gobsmacked by how accessible I found it. I'd like to try and demystify it
for other people!

The paper begins by stating that the authors want to express strict stateful
computations in a purely-functional language efficiently. They want to do this
[because of reasons](http://www.threewordphrase.com/pardonme.gif). Some
examples of algorithms they'd like to express are those based on mutable hash
tables, union find, and especially input/output. However, the language they are
working with is renowned for its laziness, which means the order of evaluation
can be counterintuitive, and its referential transparency, which means a lack
of side effects.

The authors manage to square this circle by leveraging the type system, which
simultaneously allows most of the features we take for granted in imperative
languages (multiple named variables, in-place updates) as well as being
encapsulated and referentially transparent (in a sense).
