--------------------------------------------------------------------------------
title: "IHaskell on CoCalc!"
published: 2018-04-08
tags: programming, haskell
--------------------------------------------------------------------------------

IHaskell is now available on [CoCalc](https://cocalc.com/)! I'm excited that
there are even more options for creating your own IHaskell notebooks without
having to install anything. 

This is the result of a long collaboration with the lovely people at CoCalc,
particularly [William Stein](https://wstein.org/) and [Harald
Schilly](http://harald.schil.ly/). I'd like to say a little about how this
happened, but if you're not interested you can stop reading here!

During the wonderful and magical time that was my batch at the [Recurse
Center](https://www.recurse.com/scout/click?t=5ac465e5d3396a7e491e42afac4c5c90),
I fulfilled a long-term goal of [speaking at the
NYHUG](https://www.youtube.com/watch?v=wsNnP3we_R4). I was particularly
enthusiastic about IHaskell, and [Gershom](http://gbaz.github.io/) mentioned
that people at SageMathCloud were interested in [getting it
working](https://github.com/sagemathinc/cocalc/issues/125) and offered to put
me in touch.

I immediately encountered an issue where [IHaskell wasn't handling all valid
inputs](https://github.com/gibiansky/IHaskell/issues/694) and complained about
it on [Zulip](https://www.recurse.com/blog/112-how-rc-uses-zulip). I woke up
the next day to find that [Libby Horacek](https://twitter.com/horrorcheck) had
gone ahead and [fixed the
issue](https://github.com/gibiansky/IHaskell/pull/697). What a legend! This was
also my first open-source contribution to a Haskell project ever.

I then tried the obvious thing, `stack build && stack install` which resulted
in a working notebook but an impossible deploy, because they needed the whole
installation to be self-contained whereas Stack stores some things in
`~/.stack` and other things in `<project>/.stack-work`. I (incorrectly) assumed
that having `ihaskell` be a static binary would fix the issue, so then I spent
a few weeks faffing around with
[this](https://www.fpcomplete.com/blog/2016/10/static-compilation-with-stack)
which resulted in a static binary and made no difference otherwise.

Frustrated that my first attempt failed, I went silent for a while. My batch
ended, I was unemployed for a few more months, I started a new role, and I
moved to a new country. Because [I asked at the right
time](https://github.com/gibiansky/IHaskell/pull/716#issuecomment-305934463), I
became a maintainer of IHaskell and we finally got GHC 8 support! We also
dropped GHC 7.10 support, which was
[controversial](https://github.com/gibiansky/IHaskell/pull/747).

In the meantime SageMathCloud rebranded to CoCalc and revamped its
infrastructure, and [they put the feelers out to try
again](https://github.com/gibiansky/IHaskell/issues/731).

I saw that they were still running GHC 7.10 and went silent again, because I
didn't want to use an old version of IHaskell or have the discussion about
installing a newer GHC just so I could fiddle some more. Instead, I worked on
keeping IHaskell up-to-date and making it easier to use, with PRs for [GHC
8.2](https://github.com/gibiansky/IHaskell/pull/735), [GHC
8.4](https://github.com/gibiansky/IHaskell/pull/817) and supporting
installation with Nix.

Despite my best efforts, I was still getting lots of questions about installing
IHaskell and I got fed up and [whinged about it on
Twitter](https://twitter.com/vbhvsgr/status/975388161898561536). In response,
Gershom DMed me to ask how the CoCalc stuff was progressing, after which I got
back in touch and asked for a newer version of GHC. They obliged with GHC 8.4.

By this time we had realised that Stack wasn't going to work, so we tried
old-style Cabal and new-style Cabal, which both have similar issues with
dependencies in `~/.cabal`. Finally Harald suggested using `cabal sandbox` and
we were able to come up with a working, self-contained IHaskell install.

This would have been impossible without the patience and generosity of everyone
involved. All told, it took almost a year and a half to get this working, which
doesn't come across in the
[announcement](https://twitter.com/cocalc_com/status/982650432928583680).
