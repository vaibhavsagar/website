--------------------------------------------------------------------------------
title: Easy Pull Requests
published: 2017-07-31
tags: programming
--------------------------------------------------------------------------------

There are many reasons to start contributing to open source:

* to give back to the community
* to fix a bug in software you depend on
* warm fuzzies

But you don't always have the time or energy to understand a new codebase well
enough to make a meaningful change to it. And even if you do, it's not clear
that this is the best use of your time.

Your pull request might linger untouched for months, like the source control
equivalent of shouting into the void. Alternatively, the maintainer might be
having a bad day and dismiss your pull request or expect you to do some arcane
Git voodoo to bring it up to their exacting standards.  All this can turn the
gift of patches into a boring and unpleasant chore.

For a long time I mistakenly believed the the only **real** contributions were
those that included code to fix bugs or add new features. Eventually I realised
there were simpler ways of getting my name in commit logs and my open source
contributions vastly increased.

I decided to focus on easy pull requests as a precursor to, or instead of,
engaging more fully with a project. What do I mean by this?

## Documentation and typo fixes

Lindsey Kuper has [a great post on
these](http://composition.al/blog/2013/05/31/one-character-patches/). They are
my favourite because I am a pedant. Witness [the absurd number of README
updates](https://github.com/pulls?utf8=%E2%9C%93&q=is%3Apr+author%3Avaibhavsagar+is%3Aclosed+README)
I have to my name. They're as easy as noticing a typo, clicking the pencil
icon, and fixing it. Editing project documentation is similarly
straightforward.

This is the quickest way I know of to contribute and gauge a project's health
at the same time. If your typo fix doesn't get merged quickly, then a more
substantial change probably won't either and you'll have found that out
quickly.

## Linter suggestions

One day I decided to run HLint, a Haskell linter, on a couple of projects and
open PRs accordingly. I had [mixed
results](https://github.com/pulls?q=is%3Apr+author%3Avaibhavsagar+HLint+is%3Aclosed).

This technique isn't Haskell specific, and I think it is especially well-suited
to Python and Go projects where there is a 'blessed' style to work off and
tools like `flake8` and `gofmt` are available.

If your change gets accepted, then you'll be improving code quality and paving
the way for future contributors. If it doesn't, then you might have a
discussion about good style that might lead to a documentation PR! If the
maintainers are hostile, then you're probably better off elsewhere.

## Automated testing

I'm a big fan of automation, and I'd consider it one of the themes of this
blog. A large number of projects don't have good automated test suites, and
even those that do sometimes let their CI setup rot over time. If this is the
case, setting up Travis or Circle CI is a low-effort (if occasionally
high-frustration) way of vastly improving a project. If information about how
to test a specific project isn't readily available, this is also a great
opportunity for a README update (see above).

Again: if you don't get a quick response, or at least a positive one, I think
it's a good sign that this project is not worth your time and at least you
won't have put a ton of effort into finding that out.

Of course, these aren't the only types of easy contributions you can make:
other ideas are documenting the undocumented, adding or refactoring tests, and
using the issue tracker to call attention to bugs. Neil Mitchell has a great
presentation on ['Drive-by Haskell
Contributions'](http://ndmitchell.com/downloads/slides-drive-by_haskell_contributions-09_jun_2017.pdf),
although the general approach is applicable to your programming language of choice.

I hope I've given you a few ideas about contributing to open source and how you
can make effective use of your time, and I'd love to see more easy pull
requests by you!

Thanks to [Julia Evans](https://jvns.ca/), [Harold
Treen](https://haroldtreen.com/), and [Rudi Chen](http://digitalfreepen.com)
for comments and feedback.
