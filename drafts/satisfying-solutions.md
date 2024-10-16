--------------------------------------------------------------------------------
title: SATisfying Solutions to Difficult Problems!
published: 2024-09-10
tags: programming
--------------------------------------------------------------------------------

_This post covers the same material as [my !!Con 2024
talk](https://www.youtube.com/watch?v=J0ZLCHHku6U)._

What are SAT solvers, and how are they useful? Let's start by briefly touching on
[NP-complete problems](https://en.wikipedia.org/wiki/NP-completeness)!

# NP-complete problems

NP-complete problems are [decision
problems](https://en.wikipedia.org/wiki/Decision_problem), i.e. the solution to
them is "yes" or "no". When these solutions exist, they can be verified in
[polynomial
time](https://en.wikipedia.org/wiki/Time_complexity#Polynomial_time), but we
don't know how to find solutions in polynomial time, or even if this is
possible at all (this is the [P versus NP
problem](https://en.wikipedia.org/wiki/P_versus_NP_problem)). An important
characteristic of NP-complete problems is that any NP-complete problem can be
reduced to any other NP-complete problem. Examples of NP-complete problems include:

- [Knapsack problem](https://en.wikipedia.org/wiki/Knapsack_problem)
- [Travelling salesman problem](https://en.wikipedia.org/wiki/Travelling_salesman_problem)
- [Subset sum problem](https://en.wikipedia.org/wiki/Subset_sum_problem)
- [Graph colouring problem](https://en.wikipedia.org/wiki/Graph_coloring)
- [Sudoku](https://en.wikipedia.org/wiki/Sudoku)
- [Boolean satisfiability problem](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem)

We're specifically interested in the boolean satisfiability problem.

## Boolean satisfiability problem

One definition of the boolean satisfiability problem is

> Given a propositional logic formula, can we assign truth values to each
  variable such that the formula is satisfied?

When working with these formulas, we commonly express them in [Conjunctive
normal form](https://en.wikipedia.org/wiki/Conjunctive_normal_form) as
a conjunction (ANDed together) of clauses that consist of a series of
disjunctions (ORed together) of literals, e.g.

$$(x \vee y \vee z) \wedge (x \vee \neg y) \wedge (\neg y \vee \neg x) \wedge (\neg z)$$

With this context, I can finally tell you what SAT solvers are!

# SAT solvers

SAT solvers are programs that solve boolean satisfiability problems by
providing satisfying assignments (when they exist)! In other words, they are
programs for solving NP-complete problems expressed as instances of the boolean
satisfiability problem(!!)

# Sudoku

To demonstrate how this works in practice, let's look at how to *reduce* Sudoku
to a Boolean satisfiability problem.

The rules of Sudoku are as follows:

- Each cell contains exactly one digit
- Each digit occurs once per row
- Each digit occurs once per column
- Each digit occurs once per sub-grid
- The solution must use the filled-in cells

A useful insight is that we can use $n$ boolean variables to represent each
digit when we add the constraint that at most one of those variables can be
true.

Now we can express those rules in propositional logic:

## Each cell has at least one value

$$\displaylines{
\definecolor{comment}{RGB}{161,161,180}
{\color{comment}\textit{\small{row 1, column 1 is one of 1,2,...9}}} \\
(x_{1,1,1} \vee x_{1,1,2} \vee \dots \vee x_{1,1,9}) \wedge \\
{\color{comment}\textit{\small{row 1, column 2 is one of 1,2,...9}}} \\
(x_{1,2,1} \vee x_{1,2,2} \vee \dots \vee x_{1,2,9}) \wedge \\
\dots \\
{\color{comment}\textit{\small{row 9, column 9 is one of 1,2,...9}}} \\
(x_{9,9,1} \vee x_{9,9,2} \vee \dots \vee x_{9,9,9})
}$$

## Each cell has at most one value

$$\displaylines{
\definecolor{comment}{RGB}{161,161,180}
{\color{comment}\textit{\small{row 1, column 1 is not both 1 and 2}}} \\
(\neg x_{1,1,1} \vee \neg x_{1,1,2}) \wedge \\
{\color{comment}\textit{\small{row 1, column 1 is not both 1 and 3}}} \\
(\neg x_{1,1,1} \vee \neg x_{1,1,3}) \wedge \\
 \dots \\
{\color{comment}\textit{\small{row 9, column 9 is not both 8 and 9}}} \\
(\neg x_{9,9,8} \vee \neg x_{9,9,9})}$$

## Each row has all values

$$\displaylines{
\definecolor{comment}{RGB}{161,161,180}
{\color{comment}\textit{\small{row 1 has a 1}}} \\
(x_{1,1,1} \vee x_{1,2,1} \vee \dots \vee x_{1,9,1}) \wedge \\
{\color{comment}\textit{\small{row 1 has a 2}}} \\
(x_{1,1,2} \vee x_{1,2,2} \vee \dots \vee x_{1,9,9}) \wedge \\
\dots \\
{\color{comment}\textit{\small{row 9 has a 9}}} \\
(x_{9,1,9} \vee x_{9,2,9} \vee \dots \vee x_{9,9,9})}$$

## Each column has all values

$$\displaylines{
\definecolor{comment}{RGB}{161,161,180}
{\color{comment}\textit{\small{column 1 has a 1}}} \\
(x_{1,1,1} \vee x_{2,1,1} \vee \dots \vee x_{9,1,1}) \wedge \\
{\color{comment}\textit{\small{column 1 has a 2}}} \\
(x_{1,1,2} \vee x_{2,1,2} \vee \dots \vee x_{9,1,2}) \wedge \\
\dots \\
{\color{comment}\textit{\small{column 9 has a 9}}} \\
(x_{1,9,9} \vee x_{2,9,9} \vee \dots \vee x_{9,9,9})}$$

## Each sub-grid has all values

$$\displaylines{
\definecolor{comment}{RGB}{161,161,180}
{\color{comment}\textit{\small{sub-grid 1 has a 1}}} \\
(x_{1,1,1} \vee x_{1,2,1} \vee \dots \vee x_{3,3,1}) \wedge \\
{\color{comment}\textit{\small{sub-grid 1 has a 2}}} \\
(x_{1,1,2} \vee x_{1,2,2} \vee \dots \vee x_{3,3,2}) \wedge \\
\dots \\
{\color{comment}\textit{\small{sub-grid 9 has a 9}}} \\
(x_{7,7,9} \vee x_{7,8,9} \vee \dots \vee x_{9,9,9})}$$

## The solution must use the filled-in cells

For a puzzle such as

<img src="https://upload.wikimedia.org/wikipedia/commons/e/e0/Sudoku_Puzzle_by_L2G-20050714_standardized_layout.svg" />

_Tim Stellmach, CC0, via Wikimedia Commons_

This looks like

$$\displaylines{
x_{1,1,5} \wedge x_{1,2,3} \wedge x_{1,5,7} \wedge \\
x_{2,1,6} \wedge x_{2,4,1} \wedge x_{2,5,9} \wedge x_{2,6,5} \wedge \\
\dots \\
x_{9,5,8} \wedge x_{9,8,7} \wedge x_{9,9,9}}$$

## Solving

To solve this Sudoku (or indeed any NP-complete problem that we have expressed
as a Boolean satisfiability problem), all we need to do is provide the
resulting propositional logic formula as input to a SAT solver!

How do these marvellous programs work?

# DPLL

One algorithm is known as DPLL. To explain how it works, let's look at an
example. Not so coincidentally, this is the same propositional logic formula
from earlier! At the beginning, we don't know the values of $x$, $y$, or $z$.

$$(x \vee y \vee z) \wedge (x \vee \neg y) \wedge (\neg y \vee \neg x) \wedge (\neg z)$$

$x$: 🤷<br>
$y$: 🤷<br>
$z$: 🤷<br>

We begin by picking a variable and assigning it a truth value, preferring unit
clauses (clauses with a single literal). In this case, let's set $z$ to $False$.
Now we can perform [unit
propagation](https://en.wikipedia.org/wiki/Unit_propagation).

## Unit propagation

When performing unit propagation, we assign the appropriate truth value to
a literal, which is obvious when it occurs in a unit clause. Then we remove all
clauses that are satisfied, since we don't need to consider them going forward.
Next we remove the literal where it is $False$, since it cannot contribute to
that clause being satisfied.

$$(x \vee y \vee \cancel{{\color{red} z}}) \wedge (x \vee \neg y) \wedge (\neg y \vee \neg x) \wedge \cancel{{\color{green} (\neg z)}}$$

$x$: 🤷<br>
$y$: 🤷<br>
$z$: `False`<br>

Next we pick another variable and continue. Let's set $y$ to $True$.

$$\cancel{{\color{green}(x \vee y \vee z)}} \wedge (x \vee \cancel{{\color{red} \neg y}}) \wedge (\cancel{{\color{red} \neg y}} \vee \neg x) \wedge \cancel{{\color{green}(\neg z)}}$$

$x$: 🤷<br>
$y$: `True`<br>
$z$: `False`<br>

Unfortunately, we now have a conflict, since two of the remaining clauses are $x$ and $\neg x$.

$$\cancel{{\color{green}(x \vee y \vee z)}} \wedge ({\color{blue} x} \vee \cancel{{\color{red} \neg y}}) \wedge (\cancel{{\color{red} \neg y}} \vee {\color{blue}\neg x}) \wedge \cancel{{\color{green}(\neg z)}}$$

$x$: 🤷<br>
$y$: `True`<br>
$z$: `False`<br>

The only appropriate thing to do here is backtrack, so we undo our previous assignment and try the other truth value, setting $y$ to $False$.

$$(x \vee \cancel{{\color{red} y}} \vee \cancel{{\color{red} z}}) \wedge \cancel{{\color{green}(x \vee \neg y)}} \wedge \cancel{{\color{green} (\neg y \vee \neg x)}} \wedge \cancel{{\color{green} (\neg z)}}$$

$x$: 🤷<br>
$y$: `False`<br>
$z$: `False`<br>

Now we can perform [pure literal elimination](https://en.wikipedia.org/wiki/DPLL_algorithm#The_algorithm).

## Pure literal elimination

When literals involving a variable in a propositional logic formula are either
always $True$ ($x$) or always $False$ ($\neg x$), then these are called _pure_
literals, and it's obvious what truth value to assign to them. In this case we set $x$ to $True$.

$$\cancel{{\color{green}(x \vee y \vee z)}} \wedge \cancel{{\color{green}(x \vee \neg y)}} \wedge \cancel{{\color{green} (\neg y \vee \neg x)}} \wedge \cancel{{\color{green} (\neg z)}}$$

$x$: `True`<br>
$y$: `False`<br>
$z$: `False`<br>

And that's DPLL (Davis-Putnam-Logemann-Loveland)!

## Davis-Putnam-Logemann-Loveland

Davis-Putnam-Logemann-Loveland is exhaustive backtracking search with unit
propagation and pure literal elimination. Although it works reasonably well for
small numbers of clauses, it has a tendency to repeatedly run into the same
conflicts, and when it does it only backtracks one level at a time. It would be
great to somehow remember and learn from these conflicts when we encounter
them, so we can scale up to more complex problems. Does such an algorithm
exist? It does.

# CDCL

CDCL starts out very similarly to DPLL in that it still features unit
propagation and pure literal elimination. However it also involves additional
bookkeeping, distinguishing between decisions (when we choose a truth value for
a variable) and implications (truth values determined through unit propagation
and pure literal elimination). It also keeps track of the [implication
graph](https://en.wikipedia.org/wiki/Implication_graph) created by decisions
and implications.

Let's look at a more complex example.

$$\begin{align}
& (a \vee d) \wedge \\
& (a \vee \neg c \vee \neg f) \wedge \\
& (a \vee f \vee j) \wedge \\
& (b \vee i) \wedge \\
& (\neg e \vee \neg c \vee g) \wedge \\
& (\neg e \vee f \vee \neg g) \wedge \\
& (e \vee f \vee \neg h) \wedge \\
& (e \vee h \vee \neg j)
\end{align}$$

We start by setting $a$ to $False$, which implies $d$ through the clause $a \vee d$.

<div style="display: flex">
<div style="flex: 33%"><img src="/images/tree1x1.svg"></div>
<div style="flex: 33%"><img src="/images/graph1x1.svg"></div>
<div style="flex: 33%">
$$\begin{align}
& {\color{red} a} \vee {\color{green} d} \\
& {\color{red} a} \vee \neg c \vee \neg f \\
& {\color{red} a} \vee f \vee j \\
& b \vee i \\
& \neg e \vee \neg c \vee g \\
& \neg e \vee f \vee \neg g \\
& e \vee f \vee \neg h \\
& e \vee h \vee \neg j
\end{align}$$
</div>
</div>


Next we set $c$ to $True$, which implies $\neg f$ through the clause $a \vee
\neg c \vee \neg f$ and $j$ through the clause $a \vee f \vee j$.

<div style="display: flex">
<div style="flex: 33%"><img src="/images/tree1x3.svg"></div>
<div style="flex: 33%"><img src="/images/graph1x3.svg"></div>
<div style="flex: 33%">
$$\begin{align}
& {\color{red} a} \vee {\color{green} d} \\
& {\color{red} a} \vee {\color{red}\neg c} \vee {\color{green} \neg f} \\
& {\color{red} a} \vee {\color{red} f} \vee {\color{green} j} \\
& b \vee i \\
& \neg e \vee {\color{red}\neg c} \vee g \\
& \neg e \vee {\color{red} f} \vee \neg g \\
& e \vee {\color{red} f} \vee \neg h \\
& e \vee h \vee {\color{red}\neg j}
\end{align}$$
</div>
</div>

## $b$

<div style="display: flex">
<div style="flex: 33%"><img src="/images/tree1x2.svg"></div>
<div style="flex: 33%"><img src="/images/graph1x2.svg"></div>
<div style="flex: 33%">
$$\begin{align}
& {\color{red} a} \vee {\color{green} d} \\
& {\color{red} a} \vee {\color{red}\neg c} \vee {\color{green} \neg f} \\
& {\color{red} a} \vee {\color{red} f} \vee {\color{green} j} \\
& {\color{red} b} \vee {\color{green} i} \\
& \neg e \vee {\color{red}\neg c} \vee g \\
& \neg e \vee {\color{red} f} \vee \neg g \\
& e \vee {\color{red} f} \vee \neg h \\
& e \vee h \vee {\color{red}\neg j}
\end{align}$$
</div>
</div>

## $e$

<div style="display: flex">
<div style="flex: 33%"><img src="/images/tree1.svg"></div>
<div style="flex: 33%"><img src="/images/graph1.svg"></div>
<div style="flex: 33%">
$$\begin{align}
& {\color{red} a} \vee {\color{green} d} \\
& {\color{red} a} \vee {\color{red}\neg c} \vee {\color{green} \neg f} \\
& {\color{red} a} \vee {\color{red} f} \vee {\color{green} j} \\
& {\color{red} b} \vee {\color{green} i} \\
& {\color{red}\neg e} \vee {\color{red}\neg c} \vee {\color{blue} g} \\
& {\color{red}\neg e} \vee {\color{red} f} \vee {\color{blue}\neg g} \\
& {\color{green} e} \vee {\color{red} f} \vee \neg h \\
& {\color{green} e} \vee h \vee {\color{red}\neg j}
\end{align}$$
</div>
</div>

## Unique Implication Point

<div style="display: flex">
<div style="flex: 50%"><img src="/images/graph2.svg"></div>
<div style="flex: 50%">
$$\displaylines{\neg (\neg f \wedge c \wedge e) \\
\iff \\
(f \vee \neg c \vee \neg e)}$$
</div>
</div>

## Learned clause

$$(f \vee \neg c \vee \neg e)$$

## Learned clause

$$\begin{align}
\definecolor{comment}{RGB}{161,161,180}
\definecolor{emphasis}{RGB}{88,110,117}
& {\color{comment}(a \vee d) \wedge} \\
& {\color{comment}(a \vee \neg c \vee \neg f) \wedge} \\
& {\color{comment}(a \vee f \vee j) \wedge} \\
& {\color{comment}(b \vee i) \wedge} \\
& {\color{comment}(\neg e \vee \neg c \vee g) \wedge} \\
& {\color{comment}(\neg e \vee f \vee \neg g) \wedge} \\
& {\color{comment}(e \vee f \vee \neg h) \wedge} \\
& {\color{comment}(e \vee h \vee \neg j) \wedge} \\\
& (f \vee \neg c \vee \neg e)
\end{align}$$

## Backjumping

<img src="/images/tree2.svg">

## Conflict-driven Clause Learning

- Learned clauses!
- Non-chronological backtracking!
- Basis of most modern SAT solvers

## Okay now let's do a silly one

# SLS

## What if we just guessed?

1. Generate a random assignment
2. Pick a random clause
3. Probabilistically flip a variable in the clause
4. Repeat until you solve it or get tired!

## Stochastic Local Search

- Surprisingly effective!
- WalkSAT
- Can be done in parallel
- Can use a form of clause learning
- Can't conclusively determine unsatisfiability(!)
- Reminds me of simulated annealing in some ways

# SMT

## Problem

$$
\begin{align}
  SEND &\\
+ MORE &\\
\hline
MONEY
\end{align}
$$

## How?

We'd have to teach the SAT solver arithmetic!

## Satisfiability Modulo Theories

- SAT solvers extended
- bitvectors, arrays, algebraic datatypes, etc.
- Z3, CVC, Yices, Boolector

# Recap

## SAT solvers

Solve NP-complete problems expressed as CNF Boolean formulas

## DPLL

- Backtracking search
- Unit propagation
- Pure literal elimination

## CDCL

- DPLL++
- Learned clauses
- Non-chronological backtracking

## SLS

- Random guessing
- Probabilistic variable flipping

## SMT solvers

SAT extended with theories

# That's all!

## Resources

- [Lindsey Kuper - Reasoning Under Uncertainty in SMT Solving, Research, and
  Life](https://www.youtube.com/watch?v=6K6HFl7UhQk)
- [The Science of Brute
  Force](https://cacm.acm.org/research/the-science-of-brute-force/)
- [Handbook of
  Satisfiability](https://www.iospress.com/catalog/books/handbook-of-satisfiability-2)
- [GRASP - A New Search Algorithm for Satisfiability (1996)](https://www.cs.cmu.edu/~emc/15-820A/reading/grasp_iccad96.pdf)
- [An Extensible SAT-solver (2003)](http://minisat.se/downloads/MiniSat.pdf)
- [CS-E3220: Propositional satisfiability and SAT
  solvers](https://users.aalto.fi/~tjunttil/2020-DP-AUT/notes-sat/overview.html)

# Thank you!!
