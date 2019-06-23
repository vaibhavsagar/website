--------------------------------------------------------------------------------
title: Hacking on GHC Has Never Been Easier!
published: 2019-06-22
tags: haskell, nix, programming
--------------------------------------------------------------------------------

At ZuriHac 2019 [Matthew Pickering](https://github.com/mpickering) talked about
[tools for working on GHC](https://www.youtube.com/watch?v=Q2ZDovqIxCw).
There's [an associated blog
post](https://mpickering.github.io/posts/2019-06-11-ghc-tools.html) which is
also well worth reading.

I wanted to focus on a small part of his presentation, which is about loading
[GHC into GHCi](https://gitlab.haskell.org/ghc/ghc/wikis/building/in-ghci) and
using [`ghcid`](https://github.com/ndmitchell/ghcid) to automatically reload
GHC on changes. This has been possible for almost a year now, [as described in
this blog post by Michael Sloan](https://mgsloan.com/posts/ghcinception/). It
has since been improved to be even easier to use!

Putting this together with [Alp Mestanogullari's
`ghc.nix`](https://github.com/alpmestan/ghc.nix), it's now possible to
provision a GHC development environment from scratch with just a few commands,
assuming that you have `nix-shell`, `cabal-install`, and `ghcid` installed.
Here they are:

```bash
$ git clone --recursive https://gitlab.haskell.org/ghc/ghc/
$ cd ghc
$ git clone https://github.com/alpmestan/ghc.nix
$ cabal update
$ nix-shell ghc.nix/ --run './boot && ./configure && ghcid'
```

This will take a while the first time, but `ghcid` will cache generated
artifacts under `./hadrian_ghci` so even quitting and reloading will be
significantly faster.

Congratulations, you are now a GHC developer!
