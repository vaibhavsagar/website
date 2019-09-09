--------------------------------------------------------------------------------
title: Git It Going - an Introduction to Git
published: 2015-05-10
tags: programming, git
--------------------------------------------------------------------------------

I gave a talk on Git to the Canberra Python Users Group on the 7th of May. It
was my first ever technical talk, and I think it went fairly well.

When writing my presentation, I decided it would be most useful to demonstrate
`git` to the audience assuming some Subversion experience but minimal Git or
Mercurial experience. I ended up presenting my talk with my slides on one
window to the left, my Github repository configured as the remote origin of my
demonstration repository in another window to the right, and a command line
window that I used to interact with my repository in the middle (which was the
most important). My instructions to myself were in a separate window as
speaker's notes. Although I spent some time window juggling, I think this was
the right approach.

The audience had all types: hardcore Mercurial users, Subversion loyalists,
people using Git as a frontend to other version control systems, and people new
to version control and diffs in general. I spent a fair chunk of the
presentation answering questions or listening to the audience answering each
other's questions, which was exactly how I wanted it.

I really enjoyed giving this talk, and would happily do so again. My
presentation contents are [here](https://github.com/vaibhavsagar/git-it-going),
complete with a `.travis.yml` file which uses `pandoc` to regenerate the slides
on each push. The slides themselves are
[available](/git-it-going) although I think they're
pretty pointless on their own. If you would like to use these materials, you
are more than welcome to do so.

Suggestions for improvement and pull requests are very welcome. There are a
couple of things I would have liked to cover that I didn't: `git commit
--amend`, `git stash`, `git cherry-pick`, `git rebase`, and so on. The
presentation is not very useful to people who want to know more about these
topics, and it might be possible to add more slides and more instructions in
case people are comfortable with basic `git` so I can safely skip the first
couple of sections and instead spend more time talking about branching,
rewriting history and different workflows.

I would highly recommend giving a technical talk if you have the opportunity.
If it is a topic I am familiar with, or if you'd like to run through the
presentation with me then I'm more than happy to help.
