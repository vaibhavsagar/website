--------------------------------------------------------------------------------
title: Easy IHaskell Docker Images with Nix
published: 2019-08-11
tags: haskell, nix
--------------------------------------------------------------------------------

Today I learned how to turn an IHaskell Nix expression into a Docker image. Here is an example:

```nix
# default.nix
let
  pkgs = {
    ihaskell = builtins.fetchTarball {
      url = "https://github.com/gibiansky/IHaskell/tarball/93bfa3a7a434c1dfe6873c2105c43856c282e183";
      sha256 = "1cvqqmpvz7s3d7zclmkm5igx36clrbdiafs47i9rik3rdzw0gr3d";
    };
    nixpkgs = builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs-channels/tarball/9ca57dc9171ca4547abf076a8987ed73c46f2e15";
      sha256 = "18d01cw6s6k9fnac3vq0k6inybqalkz4ak88pw67q4wqzq9rc07l";
    };
  };
  nixpkgs = import pkgs.nixpkgs {};
  NB_USER = "jovyan";
  NB_UID = "1000";
  dockerEtc = nixpkgs.runCommand "docker-etc" {} ''
    mkdir -p $out/etc/pam.d

    echo "root:x:0:0::/root:/bin/sh" > $out/etc/passwd
    echo "${NB_USER}:x:1000:1000::/home/${NB_USER}:" >> $out/etc/passwd
    echo "root:!x:::::::" > $out/etc/shadow
    echo "${NB_USER}:!:::::::" >> $out/etc/shadow

    echo "root:x:0:" > $out/etc/group
    echo "${NB_USER}:x:1000:" >> $out/etc/group
    echo "root:x::" > $out/etc/gshadow
    echo "${NB_USER}:!::" >> $out/etc/gshadow
  '';
  ihaskell = import "${pkgs.ihaskell}/release.nix" {
    inherit nixpkgs;
    compiler = "ghc864";
    packages = self: with self; [];
  };
in nixpkgs.dockerTools.buildLayeredImage {
    name = "ihaskell-nix";
    tag = "latest";
    contents =  [
      dockerEtc
      ihaskell
      nixpkgs.bashInteractive
    ];
    config = {
      Cmd = ["ihaskell-notebook" "--ip=0.0.0.0"];
      User = NB_USER;
      WorkingDir = "/home/${NB_USER}";
    };
    extraCommands = ''
      mkdir -m 1777 ./tmp
      mkdir -m 777 -p ./home/${NB_USER}
    '';
    maxLayers = 100;
};
```

This is how to use it:

```bash
$ docker load < $(nix-build default.nix)
$ docker run -p8888:8888 -it ihaskell-nix:latest
```

This uses IHaskell's [built-in
`release.nix`](https://github.com/gibiansky/IHaskell/blob/93bfa3a7a434c1dfe6873c2105c43856c282e183/release.nix)
to do most of the heavy lifting for IHaskell itself, and does a couple of other
things:

1. Creates the `/tmp` directory
1. Sets up a `jovyan` user, because Jupyter complains when run as `root`
1. Includes `bash`, which is not strictly necessary but is useful for poking
   around in the image and for using `:!` from within a notebook

Building the image and loading it into Docker are both very slow compared to
using Nix directly (even though I'm using the Nix support for layered images),
so I wouldn't recommend using this approach for local development. I'm
primarily interested in doing this to:

1. Share IHaskell notebooks with people who are less comfortable with Nix
2. Deploy to platforms such as Amazon's Elastic Container Service and Google's
   App Engine, which offer excellent support for Docker and no support for Nix

Unfortunately this isn't quite ready to deploy yet, especially because Jupyter
uses token-based authentication by default and the console output will not
necessarily be available after deployment. It's possible to set a password
instead, so I expect that copying the output of `jupyter notebook
--generate-config` and changing the relevant settings will be enough.  I hope
to post instructions when I get around to trying this myself.

In the meantime, I hope this is useful as a way of making IHaskell even more
widely available, and as a demonstration of using `dockerTools` to bridge the
gap between Nix and Docker!

_Thanks to [Graham Christensen](https://grahamc.com) for improving the Nix
expression to use `buildLayeredImage`._
