--------------------------------------------------------------------------------
title: Updating IHaskell to a Newer GHC
published: 2021-05-02
tags: haskell, programming, nix
--------------------------------------------------------------------------------

As the current maintainer of IHaskell, I see myself as having one primary
responsibility: keeping it up-to-date with newer GHC releases. The chain of
events that led to me becoming a maintainer started with the then-latest
version of IHaskell not having support for GHC 8.0, and I still remember how
frustrated I felt when dealing with this limitation.

Since then I've had the opportunity to add GHC 8.2, 8.4, 8.6, 8.8, 8.10, and
now 9.0 support, but because I only have to do this every 6 months or so (at
the earliest) I promptly forget the details of this work afterwards and have to
spelunk through old, often heavily amended, commits to rediscover what
past me (who is notoriously bad at documentation) did.

At the time of writing, GHC 9.2 is expected to be released soon and I don't
want to forget everything I've just (re)learned when that happens.
Additionally, it is conceivable that at some point someone other than me would
like to take a crack at updating IHaskell to the newest version of GHC. This
blog post details the steps I took to make these tasks easier in the future.

## Building IHaskell's dependencies

I should start by saying that my current approach relies heavily on Nix and the
infrastructure available in [`nixpkgs`](https://github.com/NixOS/nixpkgs). If
you don't want to use Nix for whatever reason the general ideas might still
translate to whatever method you use instead but the details will almost
certainly vary widely.

The objective of this first step is to get us to the point where all of
IHaskell's dependencies are building, so that we can then focus on
`ghc-parser`, `ipython-kernel`, and `ihaskell` exclusively.

I start with a version of Nixpkgs that has the necessary GHC version and
package overrides to minimise work. As of this writing, the Nixpkgs maintainers
base the Haskell package set they use on [Stackage](https://www.stackage.org/)
Nightlies with overrides added from
[`head.hackage`](https://gitlab.haskell.org/ghc/head.hackage). Updates seem to
go into the
[`haskell-updates`](https://github.com/NixOS/nixpkgs/tree/haskell-updates)
branch first and are then periodically merged into `master`. I started with
[this
commit](https://github.com/NixOS/nixpkgs/commit/64c6086db4a6c19bb9960baf165c867c1774ab3d)
but it had issues with building `alex` that I sent pull requests for [to
Nixpkgs](https://github.com/NixOS/nixpkgs/pull/120535) and [to the
project](https://github.com/simonmar/alex/pull/185). In the meantime it's very
easy to make any required changes to a fork or a local copy of Nixpkgs. I start
by copying `release.nix` from the IHaskell project root and changing the
reference to Nixpkgs:

<details>
<summary style="cursor: pointer">Changing Nixpkgs</summary>

```nix
let
  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/8795d39ce70f04e3fd609422d522e5b2594f3a70";
    sha256 = "01w7q0nqydippj0ygbg77byb770snhc5rnqzc6isws58642l8z4s";
  };
in
{ compiler ? "ghc901"
, jupyterlabAppDir ? null
, nixpkgs ? import nixpkgs-src {}
, packages ? (_: [])
, pythonPackages ? (_: [])
, rtsopts ? "-M3g -N2"
, systemPackages ? (_: [])
}:
```

</details>

Then it's possible to build this and see how many packages need changes:

```shell
$ nix-build release-9.0.nix --keep-going 2>&1 | wc -l
```

Fixing the affected packages might involve patching, jailbreaking it so that
its dependency bounds are relaxed, using a newer version that is not included
in the package set by default, or any number of other changes. Here's what I ended up with this time:

<details>
<summary style="cursor: pointer">Package set overrides</summary>

```nix
      cryptohash-md5    = nixpkgs.haskell.lib.doJailbreak super.cryptohash-md5;
      cryptohash-sha1   = nixpkgs.haskell.lib.doJailbreak super.cryptohash-sha1;
      basement          = super.basement_0_0_12;
      foundation        = super.foundation_0_0_26_1;
      memory            = nixpkgs.haskell.lib.appendPatch super.memory (nixpkgs.fetchpatch {
        url = "https://gitlab.haskell.org/ghc/head.hackage/-/raw/c89c1e27af8f180b3be476e102147557f922b224/patches/memory-0.15.0.patch";
        sha256 = "0mkjbrzi05h1xds8rf5wfky176hrl03q0d7ipklp9x4ls3yyqj5x";
      });
      cryptonite        = nixpkgs.haskell.lib.appendPatch super.cryptonite (nixpkgs.fetchpatch {
        url = "https://gitlab.haskell.org/ghc/head.hackage/-/raw/6a65307bbdc73c5eb4165a67ee97c7b9faa818e1/patches/cryptonite-0.28.patch";
        sha256 = "1wq9hw16qj2yqy7lyqbi7106lhk199hvnkj5xr7h0ip854gjsr5j";
      });
      profunctors       = self.callCabal2nix "profunctors" profunctors-src {}; # `profunctors-src` is defined above
      mono-traversable  = nixpkgs.haskell.lib.dontCheck super.mono-traversable;
```

</details>

After every few changes, I like to rerun `nix-build` and watch the number go
down. It's also possible to build an individual package, e.g. to build
`foundation` (and any dependencies) one would run

```shell
$ nix-build release-9.0.nix -A passthru.haskellPackages.foundation
```

[This is what the final `release-9.0.nix` looked
like](https://github.com/gibiansky/IHaskell/pull/1215/commits/12f50f34d9cf6dceb3ca5adc9fa450cee6e7dcee).

Eventually only `ghc-parser`, maybe `ipython-kernel`, and `ihaskell` should fail to
build.

## Updating `ghc-parser`

`ghc-parser` has the fewest dependencies of the three packages we are changing
so it makes sense to start there. I'm relatively low-tech as far as development
workflow goes and I prefer `ghcid` and a text editor, mostly because I haven't
yet figured out how to get anything more advanced to work. To get `ghcid`
running, assuming you have it installed globally like I do, you relax the
version bounds in `ghc-parser.cabal` and run

```shell
$ nix-shell release-9.0.nix -A passthru.haskellPackages.ghc-parser.env
$ cd ghc-parser
$ runhaskell Setup.hs configure
$ ghcid -c runhaskell Setup.hs repl lib:ghc-parser
```

Most of the compilation errors are related to [this GHC module
restructuring](https://gitlab.haskell.org/ghc/ghc/-/issues/13009) that started
in GHC 8.10 and continued in GHC 9.0. If I had kept better notes from last time
I would have looked at
[`ghc-api-compat`](https://github.com/hsyl20/ghc-api-compat/) which offers
a compatibility shim and whose
[`.cabal`](https://github.com/hsyl20/ghc-api-compat/blob/master/ghc-api-compat.cabal)
file makes translating between old and new module names very easy. Instead
I ended up looking at the [GHC 9.0 API
Haddocks](https://downloads.haskell.org/ghc/9.0.1/docs/html/libraries/ghc-9.0.1/index.html)
and the [GHC 8.10 API
Haddocks](https://hackage.haskell.org/package/ghc-8.10.2). As an aside, I am
irritated that the most recently released GHC API documentation isn't available
on Hackage. I also like to have a local checkout of the GHC source so that
I can look at the code across different commits if required.

[These are the changes I needed to make to
`ghc-parser`](https://github.com/gibiansky/IHaskell/pull/1215/commits/063e6bb0459b7ff8d9a2e92090332bf7a1e92a63).

## Updating `ipython-kernel`

`ipython-kernel` doesn't depend on the GHC API directly, so changes to it are
usually related to breaking API changes in other dependencies. In this case, no
changes were required!

## Updating `ihaskell`

This is usually the most involved package to update, as its operation is
intimately tied with the details of the GHC API. Most of the changes required
were for three reasons:

1. The aforementioned module hierarchy change

2. Changing the terminology from
"packages" to "units" as described in [this
commit](https://gitlab.haskell.org/ghc/ghc/-/commit/10a2ba90aa6a788677104cc43318c66f46e2e2b0)

3. Removing specialised `gcatch`, `gtry`, etc. functions in favour of the more
general versions in `exceptions`, as detailed in [this section of the release
notes](https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/9.0.1-notes.html#ghc-library)

As before, it's possible to get `ghcid` running with

```shell
$ nix-shell release-9.0.nix -A passthru.haskellPackages.ghc-parser.env
$ runhaskell Setup.hs configure --enable-tests
$ ghcid -c runhaskell Setup.hs repl lib:ihaskell
```

After getting everything compiling, I like to build the `ihaskell` package by
running

```shell
$ nix-build release-9.0.nix -A passthru.haskellPackages.ihaskell
```

because this sets up the test environment correctly (i.e. putting the built
`ihaskell` executable in the `$PATH`) before running tests, although course you
could do this manually. This usually catches any issues that have slipped
through and [small formatting changes in GHC output across
versions](https://github.com/gibiansky/IHaskell/pull/1215/commits/1796c35119ced7a564e75fe07067797fb182149d#diff-409dc396158ef9f7f39928cb144c6c3037072f0d9932499d2213862e0f5fbae6).

[Here are the changes I made to
`ihaskell`](https://github.com/gibiansky/IHaskell/pull/1215/commits/1796c35119ced7a564e75fe07067797fb182149d).

## Acceptance testing

Since IHaskell bridges the Jupyter and GHC ecosystems, we have an acceptance
test that essentially runs an IHaskell notebook through
[`nbconvert`](https://nbconvert.readthedocs.io/en/latest/) and ensures that the
output is identical to the input. Because GHC output (amongst other things)
differs across GHC versions, this acceptance test was frequently broken and/or
a bad indicator of whether any changes were correct. Recently [James
Brock](https://github.com/jamesdbrock) simplified and greatly improved the
acceptance test to be more reliable. Unfortunately the latest releases of
Jupyter now include additional metadata with each response including the time
of reply, which cannot be expected to be the same across runs. In the past it's
been possible to filter the offending fields out using `grep -e` but a more
sophisticated approach was required this time so I took the opportunity to
learn a little more about [`jq`](https://stedolan.github.io/jq/) and used that
instead. This new approach should also be more flexible and better at
accommodating future output changes.

[Here are the changes I made to the acceptance
tests](https://github.com/gibiansky/IHaskell/pull/1215/commits/4b62c964fb8937353d39a8798dc13d06260c9257).

## Using the updated IHaskell

We're done! I like to quickly try out a new notebook, as a quick test that
everything works as expected (and also for the novelty of being the first
person to try IHaskell on the newest GHC). To do this, I run

```shell
$ nix-build release-9.0.nix
$ result/bin/ihaskell-notebook
```
