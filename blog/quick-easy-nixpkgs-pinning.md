--------------------------------------------------------------------------------
title: Quick and Easy Nixpkgs Pinning
published: 2018-05-27
tags: programming, nix
--------------------------------------------------------------------------------

I love Nix because it makes packaging and using software so easy. For example,
here's a first stab at an expression that makes a recent version of Pandoc
available in a `nix-shell` (be warned, this will take a while the first time!):

```nix
let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      pandoc = self.callHackage "pandoc" "2.2" {};
      pandoc-types = self.callHackage "pandoc-types" "1.17.4.2" {};
    };
  };
in pkgs.runCommand "dummy" {
  buildInputs = [ haskellPackages.pandoc ];
} ""
```

If we save this to `default.nix` we can use it as follows (unless you're
reading this after the release of NixOS 18.09, more on that below):

```bash
$ nix-shell default.nix
<...>
[nix-shell]$ pandoc --version
pandoc 2.2
<...>
```

Pandoc is infamously large, so this will probably take a while the first time.
Fortunately, Nix caches build artifacts and knows to provide the same output
if the inputs are unchanged, so if we immediately try this again a second time
it should be nearly instantaneous.

Barring an event like the garbage collection of the Nix store or a change in
the expression above, we would like to never rebuild this package again.

Unfortunately, there is a serious flaw with this expression that prevents us
from guaranteeing this.

The problem is not immediately obvious, and might only manifest days or weeks
later, or when you upgrade NixOS to the next version. The issue is with the
second line,

```nix
pkgs = import <nixpkgs>;
```

where we import the system-wide `nixpkgs`. If we later update this by running

```bash
$ nix-channel --update
```

and any of the transitive dependencies of our expression are updated, this will
cause a rebuild because Nix will rightly detect that the inputs have changed.

This might be desirable in many cases, but for us it means a lot of waiting for
no benefit. We can avoid this by pinning `nixpkgs` to a known-good commit. One
way to do this is by setting the `NIX_PATH` environment variable, which is
where Nix looks for the location of `nixpkgs`. We could do this as follows:

```bash
$ NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/2f6440eb09b7e6e3322720ac91ce7e2cdeb413f9.tar.gz nix-shell default.nix
```

which takes advantage of the fact that Nix will transparently download a URL
for `nixpkgs` instead of a filepath. This can quickly get tedious and is easy
to forget though. Let's pin `nixpkgs` directly in the expression:

```nix
let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs-channels";
    rev    = "2f6440eb09b7e6e3322720ac91ce7e2cdeb413f9";
    sha256 = "0vb7ikjscrp2rw0dfw6pilxqpjm50l5qg2x2mn1vfh93dkl2aan7";
  };
  pkgs = import nixpkgs {};
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      pandoc = self.callHackage "pandoc" "2.2" {};
      pandoc-types = self.callHackage "pandoc-types" "1.17.4.2" {};
    };
  };
in pkgs.runCommand "dummy" {
  buildInputs = [ haskellPackages.pandoc ];
} ""
```

Now we use the system-wide `nixpkgs` only to provide one function,
`fetchFromGitHub`, which we then use to download a specific version of
`nixpkgs` that we import instead. This is easier to use but computing the
`sha256` is frustrating. One trick to keep in mind is that `fetchFromGitHub` is
equivalent to

```bash
$ nix-prefetch-url --unpack https://github.com/<owner>/<repo>/archive/<rev>.tar.gz
```

which outputs the correct hash at the end.

What happens if we want to update the pinned version? One workflow I've seen
suggested is to update the `rev`, change one character in the `sha256`, and let
the Nix error message tell you the correct hash to use. I think we can do
better than this.

[Joe Hermaszewski](https://github.com/expipiplus1) has a handy tool called
[update-nix-fetchgit](https://github.com/expipiplus1/update-nix-fetchgit) that
parses Nix files and automatically updates any `fetchFromGitHub` calls to the
latest `master` revision and SHA256 of the repository. This is certainly a lot
more convenient, but it doesn't seem to work for repositories that don't have a
`master` branch or that we want to update to the `HEAD` of a different branch.
This seems like an unimportant omission except that `nixpkgs-channels` is one
such repository, and we want to update it to the `HEAD` of e.g. `nixos-18.03`.

So, we have a tedious manual process on one hand and a quick, efficient, and
wrong process on the other. There has to be a better way!

I've settled on a solution that uses two extra files: an `updater` script and
a `versions.json` that stores the arguments to `fetchFromGitHub` as JSON.

My `updater` script looks like

```bash
#! /usr/bin/env nix-shell
#! nix-shell -i bash
#! nix-shell -p curl jq nix

set -eufo pipefail

FILE=$1
PROJECT=$2
BRANCH=${3:-master}

OWNER=$(jq -r '.[$project].owner' --arg project "$PROJECT" < "$FILE")
REPO=$(jq -r '.[$project].repo' --arg project "$PROJECT" < "$FILE")

REV=$(curl "https://api.github.com/repos/$OWNER/$REPO/branches/$BRANCH" | jq -r '.commit.sha')
SHA256=$(nix-prefetch-url --unpack "https://github.com/$OWNER/$REPO/archive/$REV.tar.gz")
TJQ=$(jq '.[$project] = {owner: $owner, repo: $repo, rev: $rev, sha256: $sha256}' \
  --arg project "$PROJECT" \
  --arg owner "$OWNER" \
  --arg repo "$REPO" \
  --arg rev "$REV" \
  --arg sha256 "$SHA256" \
  < "$FILE")
[[ $? == 0 ]] && echo "${TJQ}" >| "$FILE"
```

It uses `curl` and `jq` to interact with the GitHub API and `nix` to calculate
the appropriate hashes.

A simple `versions.json` looks like

```json
{
  "nixpkgs": {
    "owner": "NixOS",
    "repo": "nixpkgs-channels",
    "rev": "2f6440eb09b7e6e3322720ac91ce7e2cdeb413f9",
    "sha256": "0vb7ikjscrp2rw0dfw6pilxqpjm50l5qg2x2mn1vfh93dkl2aan7"
  }
}
```

And a Nix expression using these files looks like

```nix
let
  inherit (import <nixpkgs> {}) fetchFromGitHub lib;
  versions = lib.mapAttrs
    (_: fetchFromGitHub)
    (builtins.fromJSON (builtins.readFile ./versions.json));
  # ./updater versions.json nixpkgs nixos-18.03
  pkgs = import versions.nixpkgs {};
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      pandoc = self.callHackage "pandoc" "2.2" {};
      pandoc-types = self.callHackage "pandoc-types" "1.17.4.2" {};
    };
  };
in pkgs.runCommand "dummy" {
  buildInputs = [ haskellPackages.pandoc ];
} ""
```

And the command to update `nixpkgs` is

```bash
$ ./updater versions.json nixpkgs nixos-18.03
```

The reason I went with this approach is that `jq` is easier and friendlier to
use than most of the Nix tooling available, and Nix fortunately has good JSON
interoperability. I've toyed with the idea of rewriting my updater script in a
language that is more robust (possibly Haskell with
[hnix](https://github.com/haskell-nix/hnix)) but I feel like it's at a local
maximum and I'm happy with the way it works for now.

I hope you find some of the ideas and/or code here useful the next time you're
wondering if you should pin `nixpkgs` and how to do so!

**Appendix 1**

If you use Nix 2.0 or newer, the `builtins.fetchTarball` command takes a
`sha256` which means you can replace `fetchFromGitHub` and bootstrap without an
existing `<nixpkgs>`! The following code snippet is identical to
`fetchFromGitHub`:

```nix
fetcher = { owner, repo, rev, sha256 }: builtins.fetchTarball {
  inherit sha256;
  url = "https://github.com/${owner}/${repo}/tarball/${rev}";
};
```

and an updated expression can look something like:

```nix
let
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  nixpkgs = import (fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).nixpkgs) {};
  lib = nixpkgs.lib;
  versions = lib.mapAttrs
    (_: fetcher)
    (builtins.fromJSON (builtins.readFile ./versions.json));
in versions
```

Thanks to [Ahmad Jarara](https://jarmac.org/), [Chris
Stryczynski](https://twitter.com/@chrisczynski), [Garry
Cairns](https://github.com/garry-cairns), [Harold
Treen](https://haroldtreen.com/), [Renzo Carbonara](https://ren.zone/), [Susan
Potter](http://susanpotter.net/), and [Tobias
Pflug](https://twitter.com/tpflug) for comments and feedback!
