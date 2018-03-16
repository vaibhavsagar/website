--------------------------------------------------------------------------------
title: "Faking Non-NixOS for Stack"
published: 2018-03-17
tags: programming, haskell, nix
--------------------------------------------------------------------------------

I like most things about NixOS, but one thing I do not like is the way it
integrates with `stack`. Nix's own Haskell infrastructure works well enough
that this is not an issue for my own projects, but sometimes I want to test
that the Stack workflow is fine for people using less opinionated distros like
Ubuntu.

Fortunately, Nixpkgs includes a handy tool called `buildFHSUserEnv` which will
build a chroot wherein everything is laid out according to the [Filesystem
Hierarcy Standard](https://en.wikipedia.org/wiki/Filesystem_Hierarchy_Standard)
that most software is accustomed to. This means we can provide an environment
with Stack and any dependencies and it will happily run.

Let's walk through doing this for a package like
[IHaskell](https://github.com/gibiansky/IHaskell). We begin by cloning the
IHaskell repository and creating a `fhsenv.nix` with only `stack`:

```nix
let
  pkgs = import <nixpkgs> {};
in pkgs.buildFHSUserEnv {
  name = "fhs";
  targetPkgs = pkgs: [
    pkgs.haskellPackages.stack
  ];
}
```

Entering the chroot and running `stack build` gives us our first error:

```bash
$ $(nix-build fhsenv.nix)/bin/fhs
fhs-chrootenv$ stack build
HttpExceptionRequest Request {
  host                 = "raw.githubusercontent.com"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/fpco/stackage-content/master/stack/stack-setup-2.yaml"
  queryString          = ""
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (ConnectionFailure Network.BSD.getProtocolByName: does not exist (no such protocol name: tcp))
```

Looking through the Nixpkgs issue tracker for similar errors reveals that we
need the `iana-etc` package. Let's add it:

```nix
let
  pkgs = import <nixpkgs> {};
in pkgs.buildFHSUserEnv {
  name = "fhs";
  targetPkgs = pkgs: [
    pkgs.haskellPackages.stack
    pkgs.iana-etc
  ];
}
```

Now it'll start to download GHC, which takes forever for me. This is the wrong
download though, so cancel it and let's move on. More on this in a bit.

If the download had successfully completed, `stack` would then have complained
that `make` was unavailable, so we add `gnumake`. Then it would have complained
about the lack of Perl, a missing C compiler, missing `libgmp`, and no
`pkg-config`, so we add those too. Then it progresses a lot further before it
halts, complaining about `libtinfo` being missing. The closest thing we have is
`ncurses`, so we add that too. Now our expression looks like this:

```nix
let
  pkgs = import <nixpkgs> {};
in pkgs.buildFHSUserEnv {
  name = "fhs";
  targetPkgs = pkgs: [
    pkgs.iana-etc
    pkgs.haskellPackages.stack
    pkgs.gcc
    pkgs.gmp
    pkgs.gnumake
    pkgs.perl
    pkgs.pkgconfig
    pkgs.ncurses
  ];
}

```

This prompts `stack` to download a different GHC, but the whole process should
complete successfully now.

At this point, we're in luck, because IHaskell has been configured to work with
`stack --nix`, which means the dependencies `stack` needs are already specified
under the `nix.packages` key in `stack.yaml`, and we can copy them into
`fhsenv.nix` to speed up the process of building everything. At this point I
found that header files in `/usr/include` weren't being found, but this was
easy to fix by specifying `C_INCLUDE_PATH` in the `profile` attribute. I'd
recommend commenting out `ihaskell-widgets` at this point, because it takes an
absurdly long time to compile and doesn't seem to have any interesting
dependencies. The complete `fhsenv.nix` for `stack build` and `stack test`
looks like this:

```nix
let
  pkgs = import <nixpkgs> {};
in pkgs.buildFHSUserEnv {
  name = "fhs";
  targetPkgs = pkgs: [
    pkgs.blas
    pkgs.cairo.dev
    pkgs.iana-etc
    pkgs.file
    pkgs.haskellPackages.stack
    pkgs.gcc
    pkgs.glib.dev
    pkgs.gmp
    pkgs.gnumake
    pkgs.liblapack
    pkgs.pango.dev
    pkgs.perl
    pkgs.pkgconfig
    pkgs.ncurses
    pkgs.zeromq
    pkgs.zlib.dev
  ];
  profile = ''
    export C_INCLUDE_PATH=/usr/include:$C_INClUDE_PATH
  '';
}
```

Of course, building IHaskell is no fun if we can't install it and see it in
action. Providing the Jupyter notebook environment is an additional line:

```nix
let
  pkgs = import <nixpkgs> {};
in pkgs.buildFHSUserEnv {
  name = "fhs";
  targetPkgs = pkgs: [
    pkgs.blas
    pkgs.cairo.dev
    pkgs.iana-etc
    pkgs.file
    pkgs.haskellPackages.stack
    pkgs.gcc
    pkgs.glib.dev
    pkgs.gmp
    pkgs.gnumake
    pkgs.liblapack
    pkgs.pango.dev
    pkgs.perl
    pkgs.pkgconfig
    (pkgs.python3.withPackages (ps: [ ps.jupyter ps.notebook ]))
    pkgs.ncurses
    pkgs.zeromq
    pkgs.zlib.dev
  ];
  profile = ''
    export C_INCLUDE_PATH=/usr/include:$C_INClUDE_PATH
  '';
}
```

and we can install and run IHaskell as usual:

```bash
fhs-chrootenv$ stack build
fhs-chrootenv$ stack exec -- ihaskell install --stack
fhs-chrootenv$ stack exec -- jupyter notebook
```

Cool!
