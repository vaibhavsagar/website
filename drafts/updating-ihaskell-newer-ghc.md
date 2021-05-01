--------------------------------------------------------------------------------
title: Updating IHaskell to a Newer GHC
published: 2021-05-10
tags: programming, nix, haskell
--------------------------------------------------------------------------------

As the current maintainer of IHaskell, I see myself as having one primary
responsibility: keeping it up-to-date with newer GHC releases. The chain of
events that led to me becoming a maintainer started with the then-latest
version of IHaskell not having support for GHC 8.0, and I still remember how
frustrated I felt when dealing with this limitation, which drives me to make
sure nobody else has to experience this.

Since then I've had the opportunity to add GHC 8.2, 8.4, 8.6, 8.8, 8.10, and
now 9.0 support, but because I only have to do this every 6 months or so (at
the earliest) I promptly forget the details of this work afterwards and have to
spelunk through old (often retroactively amended) commits to rediscover what
past me (who is notoriously bad at documentation) did.

At the time of writing, GHC 9.2 is expected to be released soon and I don't
want to forget everything I've just (re)learned when that happens.
Additionally, it is conceivable that at some point someone other than me would
like to take a crack at updating IHaskell to the newest version of GHC. This
blog post exists to make these tasks easier.

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
running, assuming you have it installed globally like I do, you can run

```shell
$ nix-shell release-9.0.nix -A passthru.haskellPackages.ghc-parser.env
$ cd ghc-parser
$ runhaskell Setup.hs configure
Warning: ghc-parser.cabal:17:28: Packages with 'cabal-version: 1.12' or later
should specify a specific version of the Cabal spec of the form
'cabal-version: x.y'. Use 'cabal-version: 1.16'.
Configuring ghc-parser-0.2.2.0...
Setup.hs: Encountered missing or private dependencies:
ghc >=8.0 && <8.11
$ # After fixing the error above by editing `ghc-parser.cabal`
$ runhaskell Setup.hs configure
$ ghcid -c runhaskell Setup.hs repl lib:ghc-parser
```
