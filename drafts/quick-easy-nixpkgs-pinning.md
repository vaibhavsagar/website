--------------------------------------------------------------------------------
title: Quick and Easy Nixpkgs Pinning
published: 2018-01-19
tags: programming, nix
--------------------------------------------------------------------------------

I love Nix because it makes packaging and using software so easy. For example,
here's an expression that makes a recent version of Pandoc available in a
`nix-shell` at the time of writing:

```nix
let
  pkgs = import <nixpkgs> {};
  haskellPkgs = pkgs.haskellPackages.override {
    overrides = self: super: {
      doctemplates = self.callHackage "doctemplates" "0.2.1" {};
      hslua = self.callHackage "hslua" "0.9.1" {};
      pandoc = self.callHackage "pandoc" "2.0.1" {};
      pandoc-types = self.callHackage "pandoc-types" "1.17.2" {};
      skylighting = self.callHackage "skylighting" "0.4.2" {};
    };
  };
in pkgs.runCommand "dummy" {
  buildInputs = [ haskellPkgs.pandoc ];
} ""
```

If we save this to `default.nix` we can use it as follows:

```bash
$ nix-shell default.nix
<...>
[nix-shell]$ pandoc --version
pandoc 2.0.1
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
later. The issue is with the second line,

```nix
pkgs = import <nixpkgs>;
```

where we import the system-wide `nixpkgs`. If we later update this by running

```bash
$ nix-channel --update
```

and any of the transitive dependencies of our expression are updated, this will
cause a rebuild because Nix will rightly detect that the inputs have changed.

This might be desirable in many cases, but for us it means a lot of waiting. We
can avoid this by pinning `nixpkgs` to a known-good commit. One way to do this
is by setting the `NIX_PATH` environment variable, which is where Nix looks
for the location of `nixpkgs`. We could do this as follows:

```bash
$ NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/d9a2891c32ee452a2cd701310040b660da0cc853.tar.gz nix-shell default.nix
```

which takes advantage of the fact that Nix will transparently download a URL
for `nixpkgs` instead of a filepath. This can quickly get tedious and is easy
to forget though. Let's pin `nixpkgs` directly in the expression:

```nix
let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs-channels";
    rev = "d9a2891c32ee452a2cd701310040b660da0cc853";
    sha256 = "14m6krpv7iga96bjpb4xmdq1fpysryyfvkghn68k6g8gr9y61fqs";
  };
  pkgs = import nixpkgs {};
  haskellPkgs = pkgs.haskellPackages.override {
    overrides = self: super: {
      doctemplates = self.callHackage "doctemplates" "0.2.1" {};
      hslua = self.callHackage "hslua" "0.9.1" {};
      pandoc = self.callHackage "pandoc" "2.0.1" {};
      pandoc-types = self.callHackage "pandoc-types" "1.17.2" {};
      skylighting = self.callHackage "skylighting" "0.4.2" {};
    };
  };
in pkgs.runCommand "dummy" {
  buildInputs = [ haskellPkgs.pandoc ];
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

What happens if we want to update pinned version? One workflow I've seen
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
such repository, and we want to update it to the `HEAD` of e.g. `nixos-17.09`.

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
    "rev": "d9a2891c32ee452a2cd701310040b660da0cc853",
    "sha256": "14m6krpv7iga96bjpb4xmdq1fpysryyfvkghn68k6g8gr9y61fqs"
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
  # ./updater versions.json nixpkgs nixos-17.09
  pkgs = import versions.nixpkgs {};
  haskellPkgs = pkgs.haskellPackages.override {
    overrides = self: super: {
      doctemplates = self.callHackage "doctemplates" "0.2.1" {};
      hslua = self.callHackage "hslua" "0.9.1" {};
      pandoc = self.callHackage "pandoc" "2.0.1" {};
      pandoc-types = self.callHackage "pandoc-types" "1.17.2" {};
      skylighting = self.callHackage "skylighting" "0.4.2" {};
    };
  };
in pkgs.runCommand "dummy" {
  buildInputs = [ haskellPkgs.pandoc ];
} ""
```

And the command to update `nixpkgs` is

```bash
$ ./updater versions.json nixpkgs nixos-17.09
```

The reason I went with this approach is that `jq` is easier and friendlier to
use than most of the Nix tooling available, and Nix fortunately has good JSON
interoperability. I've toyed with the idea of rewriting my updater script in
a language that is more robust but I feel like it's at a local maximum and I'm
happy with the way it works for now.

I hope you find some of the ideas and/or code here useful the next time you're
wondering if you should pin `nixpkgs` and how to do so!
