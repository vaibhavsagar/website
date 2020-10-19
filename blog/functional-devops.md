--------------------------------------------------------------------------------
title: Functional DevOps in a Dysfunctional World
published: 2019-07-04
tags: programming, nix, devops
--------------------------------------------------------------------------------

_This is a pseudo-transcript of [a presentation I gave at the linux.conf.au
2018 Real World Functional Programming
Miniconf](https://www.youtube.com/watch?v=RsSNEkBGmj0)._

What is DevOps about? For me it's about my relationship to the phrase

> It works on my machine.

I've been guilty of saying this in the past, and quite frankly, it isn't good
enough. After the development team has written their last line of code, some
amount of work still needs to happen in order for the software to deliver value.

A few jobs ago I was at a small web development shop, and my deployment
workflow was as follows:

1. Log on to the development server and take careful notes on how it had diverged from the production server.
1. Carefully set aside some time to 'do the deploy'.
1. Log on to the production server and do a `git pull` to get the latest code changes.
1. Perform database migrations according to the notes you made earlier.
1. Manually make any other required changes.

Despite my best efforts, I would inevitably run into issues whenever I did
this, resulting in site outages and frustrated clients. This was far from
ideal, but I wasn't able to articulate why at the time.

I posit that a better deployment process has the following properties:

- **Automatic**: instead of a manual multi-step process, it has a single step,
  which can be performed automatically.

- **Repeatable**: instead of only being able to deploy to one lovingly
  hand-maintained server, it can deploy reliably multiple times to multiple
  servers.

- **Idempotent**: if the target is already in the desired state, no extra work
  needs to be done.

- **Reversible**: if it turns out I made a mistake, I can go back to the
  previous state.

- **Atomic**: an external observer can only see the new state or the old state,
  not any intermediate state.

I hope to demonstrate how the Nix suite of tools (Nix, NixOS, and NixOps)
fulfill these properties and provide a better DevOps experience.

To make things easier, I'm not assuming that you already run NixOS. Any Linux
distro should do, as long as you've [installed
Nix](https://nixos.org/nix/download.html). macOS users will be able to follow
along until I get to the NixOps section.

## Shipping it

### Packaging

Suppose we have been given a small Haskell app to get up and running:

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

`default.nix` might look something like 

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

Notice that we've built a Haskell executable without having to directly deal
with any Haskell-specific tooling (unless you count `cabal2nix`). Nix works
best if you allow it full control over builds, as we do here.

What happens if we run `nix-build` again without changing anything?

```bash
$ nix-build
/nix/store/<hash>-blank-me-up-0.1.0.0
```

It should be nearly instantaneous and not require rebuilding anything. Nix
tries to think of build outputs as a pure function of its inputs, and since our
inputs are unchanged, it is able to give us back the same path that it did
before. This is what I mean when I say Nix is declarative.

What if we break our app:

```diff
--- a/functional-devops/app/Main.hs
+++ b/functional-devops/app/Main.hs
@@ -4,6 +4,8 @@ import Web.Scotty

 import Data.Monoid (mconcat)

+broken
+
 main = scotty 3000 $ do
     get "/:word" $ do
         beam <- param "word"

```

and try to build again?

```bash
$ nix-build
<...>
Building executable 'blank-me-up' for blank-me-up-0.1.0.0..
[1 of 1] Compiling Main             ( Main.hs, dist/build/blank-me-up/blank-me-up-tmp/Main.o )

Main.hs:7:1: error:
    Parse error: module header, import declaration
    or top-level declaration expected.
  |
7 | broken
  | ^^^^^^
builder for '/nix/store/<hash>-blank-me-up-0.1.0.0.drv' failed with exit code 1
error: build of '/nix/store/<hash>-blank-me-up-0.1.0.0.drv' failed
<...>
```

It fails, as one would hope, but more importantly the previous symlink at
`result` is still available! This is because `nix-build` completes the build
before atomically updating the symlink at `result` to point to the new
artifact. This way, we can move from one known working state to another,
without exposing our users to any intermediate brokenness.

### Service Configuration

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
from this configuration. To do this, we'll need one more file:

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

Here's what that looks like on my machine:

<details>
<summary style="cursor: pointer;">Generated `systemd` service</summary>
```default
[Unit]
After=network.target
Description=Blank Me Up

[Service]
Environment="LOCALE_ARCHIVE=/nix/store/<hash>-glibc-locales-2.27/lib/locale/locale-archive"
Environment="PATH=/nix/store/<hash>-coreutils-8.30/bin:/nix/store/<hash>-findutils-4.6.0/bin:/nix/store/<hash>-gnugrep-3.3/bin:/nix/store/<hash>-gnused-4.7/bin:/nix/store/<hash>-systemd-239.20190219/bin:/nix/store/<hash>-coreutils-8.30/sbin:/nix/store/<hash>-findutils-4.6.0/sbin:/nix/store/<hash>-gnugrep-3.3/sbin:/nix/store/<hash>-gnused-4.7/sbin:/nix/store/<hash>-systemd-239.20190219/sbin"
Environment="TZDIR=/nix/store/<hash>-tzdata-2019a/share/zoneinfo"



ExecStart=/nix/store/<hash>-blank-me-up-0.1.0.0/bin/blank-me-up
KillMode=process
Restart=always
```
</details>

Hopefully at this point you're convinced that Nix can take some quasi-JSON and
turn it into a binary and a `systemd` service file. Let's deploy this!

### Deploying

First, we install NixOps:

```bash
$ nix-env -i nixops
```

We also have to set up VirtualBox, which I'll be using as my deploy target. If
you're using NixOS this is as simple as adding the following line to
`configuration.nix`:

```nix
virtualisation.virtualbox.host.enable = true;
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

```bash
$ curl http://<ip>:3000/help
<h1>Scotty, help me up!</h1>
```

NixOps also allows us to SSH in for troubleshooting purposes or to view logs:

```bash
$ nixops ssh -d trivial webserver
<...>
[root@webserver:~]# systemctl status blank-me-up
```

## Responding to change

This is fantastic, but deployments are rarely fire-and-forget. What happens
when our requirements change? In fact, there's a serious issue with our
application, which is that it hardcodes the port that it listens on. If we
wanted it to listen on a different port, or to run more than one instance of it
on the same machine, we'd need to do something differently.

The correct solution would be to talk to the developers and have them implement
support, but in the meantime, how should we proceed?

### Patching

Nix gives us full control over each part of the build and deployment process,
and we can patch the software as a stopgap measure. Although this scenario is
somewhat contrived, I have in fact had to take matters into my own hands like
this in the past when the development team hasn't been able to prioritise
fixing a production issue.

Our new expression looks like this:

*nix/patched.nix*
```nix
args@{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

(import ./default.nix args).overrideAttrs (old: {
  postPatch = let
    oldImport = ''
      import Web.Scotty
    '';
    newImport = ''
      import Web.Scotty
      import System.Environment (getArgs)
    '';
    oldMain = ''
      main = scotty 3000 $ do
    '';
    newMain = ''
      main = getArgs >>= \(port:_) -> scotty (read port) $ do
    '';
  in ''
    substituteInPlace Main.hs --replace '${oldImport}' '${newImport}'
    substituteInPlace Main.hs --replace '${oldMain}'   '${newMain}'
    cat Main.hs
  '';
})
```

I've added that `cat Main.hs` at the end to

- confirm that the file was correctly patched
- emphasise that arbitrary shell commands can be executed

We can create a new service definition to use this expression:

*nix/service-patched.nix*
```nix
{ config, lib, pkgs, ... }:

let
  blank-me-up = pkgs.callPackage ./patched.nix { nixpkgs = pkgs; };
  cfg = config.services.blank-me-up;
in {
  options.services.blank-me-up.enable = lib.mkEnableOption "Blank Me Up";
  options.services.blank-me-up.port = lib.mkOption {
    default = 3000;
    type = lib.types.int;
  };

  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ cfg.port ];

    systemd.services.blank-me-up = {
      description = "Blank Me Up";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${blank-me-up}/bin/blank-me-up ${toString cfg.port}";
        Restart = "always";
        KillMode = "process";
      };
    };
  };
}
```

We make sure to pass the configured port in on startup and open the firewall
appropriately.

### Deploying (Again)

We update `webserver.nix` to use the patched service and specify a different
port:

```nix
{ ... }: {
  imports = [ ../nix/service-patched.nix ];
  services.blank-me-up.enable = true;
  services.blank-me-up.port = 3001;
}
```

And we can deploy again!

```bash
$ nixops deploy -d trivial
```

The service should now be running on `http://<ip>:3001` instead of
`http://<ip>:3000`.

```bash
$ curl http://<ip>:3001/pull
<h1>Scotty, pull me up!</h1>
```

If we made a mistake, rolling back is easy:

```bash
$ nixops list-generations -d trivial
   1   <timestamp>
   2   <timestamp>   (current)

$ nixops rollback -d trivial 1
switching from generation 2 to 1
webserver> copying closure...
trivial> closures copied successfully
<more output>
```

and in fact nothing needs to be copied to the target, because the previous
deployment is still there.

## Conclusion

As demonstrated, the Nix ecosystem allows us to impose order on the usually
messy and ad-hoc practice of packaging and deploying software at scale. I'm
satisfied that this is the way forward and hope that you will consider using
these tools to tackle problems of your own!
