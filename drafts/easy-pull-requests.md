--------------------------------------------------------------------------------
title: Easy Pull Requests
published: 2017-07-29
tags: programming
--------------------------------------------------------------------------------

There are many reasons to start contributing to open source:

* to give back to the community
* to fix a bug in software you depend on
* warm fuzzies

But if you write software for a living, you don't always have the time or
energy to understand a new codebase well enough to make a meaningful change to
it. And even if you don't write software for a living, this still might not
sound like the best of your leisure time.

Even if you soldier on, your pull request might linger untouched for months,
like the source control equivalent of shouting into the void. Alternatively,
the maintainer might be having a bad day and dismiss your pull request or
expect you to do some Git voodoo to bring it up to their exacting standards.
All this can turn the gift of patches into a boring and unpleasant chore.

To avoid this, I've decided to focus on easy pull requests as a precursor to
engaging more fully with a project. What does this mean?

## Documentation and typo fixes

Lindsey Kuper has [a great post on
these](http://composition.al/blog/2013/05/31/one-character-patches/). They are
my favourite because I am a pedant. Witness [the absurd number of README
updates](https://github.com/pulls?utf8=%E2%9C%93&q=is%3Apr+author%3Avaibhavsagar+is%3Aclosed+README)
I have to my name. They're as easy as noticing a typo, clicking the pencil
icon, and fixing it. Editing project documentation is similarly
straightforward.

## Linter suggestions

One day I decided to run HLint, a Haskell linter, on projects and open PRs
accordingly. I had [mixed
results](https://github.com/pulls?q=is%3Apr+author%3Avaibhavsagar+HLint+is%3Aclosed)
but I think that if a project doesn't have consistent style or a style guide
this is an excellent way of opening a discussion about that.

## Automated testing

I'm a big fan of automation, and I'd consider it one of the themes of this
blog. A large number of projects don't have good automated test suites, and
even those that do sometimes let their CI setup rot over time. If this is the
case, setting up Travis or Circle CI is a low-effort (if occasionally
high-frustration) way of vastly improving a project. If information about how
to test a specific project isn't readily available, this is also a great
opportunity for a README update (see above).

In all three cases, if you don't get a quick response, or at least a positive
one, I think it's a good sign that this project is not a good fit and you'll
have learned this without investing much effort.

For a while I felt like these weren't 'real' contributions, where 'real' meant
writing code to add features or fix bugs, but then I realised that these
approaches focus on essential, if unglamorous, aspects of being a software
engineer. Writing correctly-spelled and accurate documentation, having
consistent style (preferably enforced by a tool to prevent endless
bikeshedding), and automatically testing your changes are important! Focusing
on these practices has made me a better programmer.

I hope I've given you a few ideas about contributing to open source and why you
might want to. Let me know if you have any questions!
