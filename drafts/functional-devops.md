--------------------------------------------------------------------------------
title: Functional DevOps in a Dysfunctional World
published: 2019-05-26
tags: programming, nix, devops
--------------------------------------------------------------------------------

What is DevOps about? For me it's about the phrase

> It works on my machine.

I've been guilty of saying this in the past, and quite frankly, it isn't good
enough. After the development team has written their last line of code, some
amount of work still needs to happen in order for the software to deliver value.

To make things easier, I'm not assuming that you already run NixOS. Any Linux
distro should do, as long as you've [installed
Nix](https://nixos.org/nix/download.html). macOS users will be able to follow
along until I get to the NixOps section.

# Shippping it

## Packaging

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

(This example is taken straight from [Scotty's
README](https://github.com/scotty-web/scotty/blob/306fee7121dc41a55bd4e9b785f8366198de7e3c/README.md#scotty-).)

Our first step is to build this app and quickly check that it works. We'll need
Nix and `cabal2nix`, which turns `.cabal` files into configuration for the Nix
package manager. Assuming we've installed `cabal2nix`:

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

## Service Configuration

Okay, now that we're able to successfully build the app, let's configure a
service file so that `systemd` can manage our app. I don't know of any tools
that automatically generate this so I always find myself copying and pasting
from an existing service file. Here's one I prepared earlier.

*nix/service.nix*
```nix
{ config, lib, pkgs, ... }:                                               #1

let                                                                       #2
  blank-me-up = pkgs.callPackage ./default.nix {};                        #3
in {
  options.services.blank-me-up.enable = lib.mkEnableOption "Blank Me Up"; #4

  config = lib.mkIf config.services.blank-me-up.enable {                  #5
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
   Another way to think of this is as a form of dependency injection.
1. `let` expressions work similarly to Haskell.
1. This is the equivalent of our `nix-build` from before.
1. We define a single option that enables our service.
1. The `config` attribute contains service configuration.
1. We expose port 3000.
1. If you squint this looks a lot like a regular unit file. More on this below.

It would be useful to look at the systemd service file that gets generated
from this configuraation. To do this, we'll need one more file:

*ops/webserver.nix*
```nix
{ ... }: {
  imports = [ ../nix/service.nix ];
  services.blank-me-up.enable = true;
}
```

This is a function that imports the above configuration and enables the
`blank-me-up` service. With this in place, we can do

```bash
$ nix-instantiate --eval -E '(import <nixpkgs/nixos/lib/eval-config.nix> { modules = [./ops/webserver.nix]; }).config.systemd.units."blank-me-up.service".text'
```

We're using `nix-instantiate` to evaluate (`--eval`) an expression (`-E`) that
uses `eval-config.nix` from the library to import the file we created and
output the text of the final unit file. The output of this is pretty messy, but
we can use `jq` to clean it up:

```bash
$ nix-instantiate --eval -E '(import <nixpkgs/nixos/lib/eval-config.nix> { modules = [./ops/webserver.nix]; }).config.systemd.units."blank-me-up.service".text' | jq -r
```

Hopefully at this point you're convinced that Nix can take some quasi-JSON and
turn it into a binary and a systemd service file. Let's deploy this!

## Deploying

First, we install NixOps:

```bash
$ nix-env -i nixops
```

We also have to set up VirtualBox, which I'll be using as my deploy target. If
you're using NixOS this is as simple as adding the following lines to
`configuration.nix`:

```nix
virtualisation.virtualbox.host.enable = true;
virtualisation.virtualbox.guest.enable = true;
```

and running `sudo nixos-rebuild switch`. If you're using another Linux distro,
install VirtualBox and set up a host-only network called `vboxnet0`.

We'll be using the [instructions from the
manual](https://nixos.org/nixops/manual/#idm140737318606176) as our starting
point. Create two files:

*ops/trivial.nix*
```nix
{
  network.description = "Web server";
  network.enableRollback = true;

  webserver = import ./webserver.nix;
}
```

*ops/trivial-vbox.nix*
```nix
{
  webserver =
    { config, pkgs, ... }:
    { deployment.targetEnv = "virtualbox";
      deployment.virtualbox.headless = true; # don't show a display
      deployment.virtualbox.memorySize = 1024; # megabytes
      deployment.virtualbox.vcpu = 2; # number of cpus
    };
}
```

We should now be able to create a new deployment:

```bash
$ cd ops
$ nixops create trivial.nix trivial-vbox.nix -d trivial
```

and deploy it:

```bash
$ nixops deploy -d trivial
```

and assuming that everything goes well, we should see a lot of terminal output
and at least one mention of `ssh://root@<ip>`, which is the IP of our target.

We should then be able to go to `http://<ip>:3000` and see our web app in
action!

NixOps also allows us to SSH in for troubleshooting purposes or to view logs:

```bash
$ nixops ssh -d trivial webserver
<...>
[root@webserver:~]# systemctl status blank-me-up
```

## Responding to change

This is fantastic, but deployments are rarely fire-and-forget. How does
