--------------------------------------------------------------------------------
title: Industrial-strength Deployments in Three Commands
published: 2019-08-19
tags: nix
--------------------------------------------------------------------------------

If your deployment target is running NixOS, a full-system deployment is only
three commands:

```bash
$ nix-copy-closure --to --use-substitutes <target> <path>                                #1
$ ssh <target> -- "sudo nix-env --profile /nix/var/nix/profiles/system --set <path>"     #2
$ ssh <target> -- "sudo /nix/var/nix/profiles/system/bin/switch-to-configuration switch" #3
```

Here's what each command does:

1. Copies the transitive closure of the new system configuration to the target,
   using binary caches (`--use-substitutes`) where possible.
2. Sets the current system profile to the new system configuration. This isn't
   strictly necessary, but allows us to roll back to this configuration later.
3. Switches to the new system configuration.

This workflow has been described before [in
Typeclasses](https://typeclasses.com/nixos-on-aws) and [by Gabriel
Gonzalez](http://www.haskellforall.com/2018/08/nixos-in-production.html), but I
thought one more post demonstrating how to use these commands wouldn't hurt.
Since the AWS use case has been covered so thoroughly by Typeclasses, I'm going
to use the [packet.net](https://www.packet.com/) cloud instead.

#### Provisioning

I logged on to the Packet console and launched a `t1.small.x86` instance
running NixOS 19.03 (the latest as of this writing). It was assigned the IP
address `147.75.38.113`. Since I added my SSH keys when I first created my
Packet account, I was able to SSH into this instance at `root@147.75.38.113`
without any further configuration.

#### Copying the existing configuration

The next step is to copy the existing configuration, especially
instance-specific hardware configuration:

```bash
$ scp -r root@147.75.38.113:/etc/nixos/* .
```

There's probably a better way to do this, but for a quick one-off demonstration
this is fine. [Here's the commit adding those
files](https://github.com/vaibhavsagar/nixos-config/commit/e49e9a980f2d547684bcab3a34a34dba4521b991).

We'll only be making changes to `configuration.nix`, which for me looks like
this (after all commented-out lines have been removed):

```nix
{ config, pkgs, ... }:

{
  imports =
    [
      ./packet.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  system.stateVersion = "19.03";

}
```

#### Building a system closure

The Nix expression to build a whole system is pretty straightforward (as
described in the Typeclasses article):

```nix
let
  nixos = import <nixpkgs/nixos> {
    configuration = import ./configuration.nix;
  };
in
  nixos.system
```

but this doesn't provide any way of pinning `nixpkgs`. Another way ([as
described by Gabriel
Gonzalez](http://www.haskellforall.com/2018/08/nixos-in-production.html#pinning-nixpkgs)),
is to explicitly depend on a particular revision of `nixpkgs`:

```nix
let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/b74b1cdb2fecc31ff7a127c5bc89771f887c93bb.tar.gz";
    sha256 = "0ncr4g29220amqm4riaa1xf4jz55v2nmh9fi16f1gzhww1gplk8h";
  };

in
  import "${nixpkgs}/nixos" {
    configuration = {
      imports = [
        /etc/nixos/configuration.nix
      ];
    };

    system = "x86_64-linux";
  }
```

but the downside there is that there's no automated way (AFAICT) to update the
revision of `nixpkgs`. I have [my own approach to pinning
`nixpkgs`](https://vaibhavsagar.com/blog/2018/05/27/quick-easy-nixpkgs-pinning/),
where I have a `versions.json` that stores version information:

```json
{
  "nixpkgs": {
    "owner": "NixOS",
    "repo": "nixpkgs-channels",
    "branch": "nixos-19.03",
    "rev": "77295b0bd26555c39a1ba9c1da72dbdb651fd280",
    "sha256": "18v866h12xk6l1s37nk1vns869pvzphmnnlhrnm2b1zklg2hd1nq"
  }
}
```

and a script that uses `jq` to update this file. My (slightly more complex)
expression then looks like this:


*default.nix*
```nix
let
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  nixpkgs = fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).nixpkgs;
  nixos = import "${nixpkgs}/nixos" {
    configuration = import ./configuration.nix;
  };
in
  nixos.system
```

and this allows me to be explicit about `nixpkgs` as well as easily update it
when necessary. [Here's the commit that adds those
files](https://github.com/vaibhavsagar/nixos-config/commit/5126d9dba971d6480aeec43c4263c5a7f7b1f1b5).

#### Deploying the system closure

With all of our prerequisites taken care of, deploying the system closure is straightforward:


*deploy.sh*
```bash
#!/usr/bin/env bash

set -euxo pipefail

TARGET="root@147.75.38.113"

PROFILE_PATH="$(nix-build --no-out-link default.nix)"
nix-copy-closure --to --use-substitutes $TARGET $PROFILE_PATH
ssh $TARGET -- "nix-env --profile /nix/var/nix/profiles/system --set $PROFILE_PATH && /nix/var/nix/profiles/system/bin/switch-to-configuration switch"
```

[Here's the commit that adds `deploy.sh`](https://github.com/vaibhavsagar/nixos-config/commit/be6aaa026c8ebf1efd7c44743a8770b921111a2e).

#### Adding a service

Let's deploy the final version of the small Haskell web service from my
[Functional
DevOps](https://vaibhavsagar.com/blog/2019/07/04/functional-devops/) blog post.
The application consists of two files:

*Main.hs*
```haskell
 {-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import System.Environment (getArgs)

import Data.Monoid (mconcat)

main = getArgs >>= \(port:_) -> scotty (read port) $ do
    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```

*blank-me-up.cabal*
```default
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

and the Nix service is one file:

*service.nix*
```nix
{ config, lib, pkgs, ... }:

let
  blank-me-up = pkgs.haskellPackages.callCabal2nix "blank-me-up" ../app {};
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

For more information about what's happening in `service.nix`, see the relevant
section of my [Functional
DevOps](https://vaibhavsagar.com/blog/2019/07/04/functional-devops/#service-configuration)
post.

[Here's the commit that adds these
files](https://github.com/vaibhavsagar/nixos-config/commit/466e0e1867e47346ed8cc706b812a8cb21c76c19).

Enabling the service is as easy as adding two lines to `configuration.nix`:

*configuration.nix*
```nix
{ config, pkgs, ... }:

{
  imports =
    [
      ./packet.nix
      ./deploy/nix/service.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  services.blank-me-up.enable = true;

  system.stateVersion = "19.03";

}

```

[Here's the commit that makes that change](https://github.com/vaibhavsagar/nixos-config/commit/07b163f3c0fe728078bb357841e57c7020bdd4d3).

#### Deploying the service

```bash
$ ./deploy.sh 
+ TARGET=root@147.75.38.113
++ nix-build --no-out-link default.nix
+ PROFILE_PATH=/nix/store/<hash>-nixos-system-nixos-19.03pre-git
+ nix-copy-closure --to --use-substitutes root@147.75.38.113 /nix/store/<hash>-nixos-system-nixos-19.03pre-git
<...>
+ ssh root@147.75.38.113 -- 'nix-env --profile /nix/var/nix/profiles/system --set /nix/store/<hash>-nixos-system-nixos-19.03pre-git && /nix/var/nix/profiles/system/bin/switch-to-configuration switch'
updating GRUB 2 menu...
activating the configuration...
setting up /etc...
reloading user units for root...
setting up tmpfiles

$ curl http://147.75.38.113:3000/beam
<h1>Scotty, beam me up!</h1>
```

#### But this is just a janky bash script!!???

It's definitely the case that `deploy.sh` is short and unsophisticated, but the
three commands it invokes are what's really important here. Once you begin
looking for them, you will find them everywhere! They're used in
[NixOps](https://github.com/NixOS/nixops/blob/c8d3a3ff5fb20e8e4d494de972ebb2a1a1ec1e08/nixops/backends/__init__.py#L339-L367),
[nix-deploy](https://github.com/awakesecurity/nix-deploy/blob/68217cea7ba6746c9a262ddccb11178909841988/src/Main.hs#L159-L229),
and
[obelisk](https://github.com/obsidiansystems/obelisk/blob/1f9f466fc38a37a72afb316cee4f3317af204220/lib/command/src/Obelisk/Command/Deploy.hs#L136-L158),
and a quick GitHub search for
["switch-to-configuration"](https://github.com/search?q=switch-to-configuration&type=Code)
turns up many more examples. At a previous job, our deployment platform used
these three commands as well, and we routinely deployed to hundreds of servers
without any deployment-related issues, so I'm comfortable saying that this is
an industrial-grade deployment solution.
