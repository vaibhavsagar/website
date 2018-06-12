--------------------------------------------------------------------------------
title: Building Static Haskell Binaries with Nix
published: 2018-01-03
tags: haskell, nix, programming
--------------------------------------------------------------------------------

_Skip to the end for a faster and easier way of getting this working. All you
have to do is clone the linked repo and run `nix-build default.nix`!_

The section of the Nixpkgs manual that talks about [creating statically linked
binaries](https://nixos.org/nixpkgs/manual/#creating-statically-linked-binaries)
with Haskell ends with the caveat:

> It’s important to realize, however, that most system libraries in Nix are built as shared libraries only, i.e. there is just no static library available that Cabal could link!

That sounds like a challenge. Especially when doing it on other platforms is
[so easy](http://www.kuznero.com/posts/haskell/building-statically-linked-binaries.html).

On other platforms, building a static binary is meant to be as simple as

```bash
$ cabal update
$ cabal install --only-dependencies
$ cabal configure --disable-executable-dynamic --disable-shared --ghc-option=-optl=-static
$ cabal build
```

with the magic happening in the second step. On Nix, we do in fact have the
necessary static libraries and we can provide them as build inputs but keeping
track of the library paths gets hairy quickly. Fortunately Nix has an escape
hatch called `buildFHSUserEnv` that we can use to simulate an environment that
`cabal` is more familiar with.

Let's put it through its paces by building a simple Scotty web app:

*blank-me-up.cabal*
```haskell
name:                blank-me-up
version:             0.1.0.0
license:             BSD3
build-type:          Simple
cabal-version:       >=1.10

executable blank-me-up
  main-is:             Main.hs
  build-depends:       base >=4.9 && <5
                     , scotty
  default-language:    Haskell2010
```

*Main.hs*
```haskell
 {-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Data.Monoid (mconcat)

main = scotty 3000 $ do
    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```

We create `static.nix`:

*static.nix*
```nix
let
  pkgs = import <nixpkgs> {};
in pkgs.buildFHSUserEnv {
  name = "fhs";
  targetPkgs = pkgs: [
    (pkgs.haskellPackages.ghcWithPackages (p: with p; [ cabal-install ]))
    pkgs.gmp5.static
    pkgs.glibc.static
    pkgs.zlib.static
    pkgs.zlib.dev
  ];
}
```

This defines a chroot where statically linked versions of `gmp`, `glibc`, and
`zlib` are available, as well as `zlib.h`. We enter this environment by running

```bash
$ $(nix-build static.nix)/bin/fhs
```

and then we can run the commands above with only slight modifications:

```bash
$ cabal update
$ cabal install --only-dependencies --extra-include-dirs=/usr/include --extra-lib-dirs=/usr/lib
$ cabal configure --disable-executable-dynamic --disable-shared --ghc-option=-optl=-pthread --ghc-option=-optl=-static --ghc-option=-optl=-L/usr/lib
$ cabal build
```

The difference is the extra options passed to the linker. After the last
command, I get a whole bunch of warnings about 
```
"Using '<function>' in statically linked applications requires at runtime the shared libraries from the glibc version used for linking"
```

which is definitely something to watch out for if you plan on deploying these
executables to a machine that might be running a different version of `glibc`.
You can confirm that the executable has been statically linked by running

```bash
$ ldd dist/build/blank-me-up/blank-me-up
        not a dynamic executable
```

I've made this project available
[here](https://github.com/vaibhavsagar/experiments/tree/master/static-haskell-nix)
if you'd like to tweak it. Since this was relatively straightforward, I think
it might be possible to do this without `buildFHSUserEnv`. Maybe I will try
that next.

Happy static linking!

**Edit 1:** This turned out to be fairly easy. I took the output of

```bash
$ cabal2nix --shell . > default.nix
```

and changed `enableSharedExecutables`, `enableSharedLibraries`, and
`configureFlags` as follows:

```nix
enableSharedExecutables = false;
enableSharedLibraries = false;
configureFlags = [
  "--ghc-option=-optl=-static"
  "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
  "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
  "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
];
```

This is also available in the linked repository.

**Edit 2**: [Moritz Angermann](https://github.com/angerman) and [Niklas
Hambüchen](https://github.com/nh2) improved these instructions to be more
robust. Thanks Moritz and Niklas!
