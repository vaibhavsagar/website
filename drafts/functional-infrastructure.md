--------------------------------------------------------------------------------
title: Functional Infrastructure in a Dysfunctional World
published: 2017-11-10
tags: programming, nix
--------------------------------------------------------------------------------

I've been using Nix and NixOS at work for the last couple of months, and I
really like it! It's made my job easier and less stressful. The website markets
it as 'declarative', 'reliable', and 'DevOps-friendly', and I'd like to
demonstrate what that means and make a case for using it in your
infrastructure.

To make things easier, I'm not assuming that you already run NixOS. Any Linux
distro should do, as long as you've (installed
Nix)[https://nixos.org/nix/download.html]. macOS users will be able to follow
along until I get to the NixOps section.

Suppose we are in ops, and have been given a small Haskell app to get up and
running:

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

(Any resemblance to [the first example in Scotty's
README](https://github.com/scotty-web/scotty/blob/306fee7121dc41a55bd4e9b785f8366198de7e3c/README.md#scotty-)
is purely coincidental.)

Our first step is to build this app and quickly check that it works. We'll need
Nix and `cabal2nix`, which turns `.cabal` files into configuration for the Nix
package manager. Assuming we've

```bash
$ nix-env -i cabal2nix
<a lot of output>
created <number> symlinks in user environment
```

How do we know it worked? Try `nix-env -q` (short for `--query`):

```bash
$ nix-env -q
cabal2nix
```

Okay, assuming the app is in the `app` subdirectory, let's create a directory
called `nix` to store our `.nix` files and begin:

```bash
$ cd nix
$ cabal2nix ../app/ --shell > default.nix
```

`default.nix` should look something like 

```nix
{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, scotty, stdenv }:
      mkDerivation {
        pname = "blank-me-up";
        version = "0.1.0.0";
        src = ../app;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base scotty ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
```

Now we can build our project by running `nix-build`, which tries to build
`default.nix` in the current directory if no arguments are provided:

```bash
$ nix-build
<lots of output>
/nix/store/<hash>-blank-me-up-0.1.0.0
```

There should also be a new `result` symlink in the current directory, which
points to the path above:

```bash
$ readlink result
/nix/store/<hash>-blank-me-up-0.1.0.0
```

What happens if we run `nix-build` again without changing anything?

```bash
$ nix-build
/nix/store/<hash>-blank-me-up-0.1.0.0
```

It should be nearly instantaneous and not require rebuilding anything. Nix
tries to think of build outputs as a pure function of its inputs, and since our
inputs are unchanged, it is able to give us back the same path that it did
before. This is what we mean when we say Nix is declarative.

Okay, now that we're able to successfully build the app, let's configure a
service file so that `systemd` can manage our app. I don't know of any tools
that automatically generate this so I always find myself copying and pasting
from an existing service file. Here's one I prepared earlier.

*service.nix*
```nix
{ config, lib, pkgs, ... }:                                               #1

let                                                                       #2
  cfg = config.services.blank-me-up;
  blank-me-up = pkgs.callPackage ./default.nix { nixpkgs = pkgs; };       #3
in {
  options.services.blank-me-up.enable = lib.mkEnableOption "Blank Me Up"; #4

  config = lib.mkIf cfg.enable {                                          #5
    networking.firewall.allowedTCPPorts = [ 3000 ];                       #6

    systemd.services.blank-me-up = {                                      #7
      description = "Blank Me Up";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${blank-me-up}/bin/blank-me-up";
        Restart = "always";
        KillMode = "process";
      };
    };
  };
}
```

This isn't intended to be a Nix language tutorial, but there are a few
interesting things that I want to point out. For a more comprehensive overview
of the language, see
[here](https://medium.com/@MrJamesFisher/nix-by-example-a0063a1a4c55) or
[here](https://nixos.org/nix/manual/#ch-expression-language).

1. These are the arguments to this expression that the caller will pass.
1. `let` expressions work similarly to Haskell.
1. This is the equivalent of our `nix-build` from before.
1. We define a single option that enables our service.
1. The `config` attribute contains service configuration.
1. We expose port 3000.
1. If you squint this looks a lot like a regular unit file. More on this later.
