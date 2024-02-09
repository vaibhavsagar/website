--------------------------------------------------------------------------------
title: Low-effort Dependency Pinning with Nix Flakes
published: 2024-02-09
tags: programming, nix
--------------------------------------------------------------------------------

Back in 2018 I wrote [a blog post about pinning
`nixpkgs`](/blog/2018/05/27/quick-easy-nixpkgs-pinning/) which describes an
approach I've used happily and successfully since then to manage dependencies
(and not just `nixpkgs`) for small projects. In short, it involves

1. `versions.json`, a JSON file storing dependency information
2. `updater`, an updater script
3. `pkgs.nix`, a Nix expression that makes each dependency available

Here's what each of those files might look like:

<details>
<summary style="cursor: pointer">`versions.json`</summary>
```json
{
  "ihaskell": {
    "owner": "gibiansky",
    "repo": "IHaskell",
    "branch": "master",
    "rev": "575b2be1c25e8e7c5ed5048c8d7ead51bb9c67f0",
    "sha256": "148sdawqln2ys0s1rapwj2bwjzfq027dz5h49pa034nmyizyqs4a"
  },
  "nixpkgs": {
    "owner": "NixOS",
    "repo": "nixpkgs",
    "branch": "nixos-23.11",
    "rev": "9dd7699928e26c3c00d5d46811f1358524081062",
    "sha256": "0hmsw3qd3i13dp8jhr1d96xlpkmd78m8g6shw086f6sqhn2rrvv6"
  }
}
```
</details>

<details>
<summary style="cursor: pointer">`updater`</summary>
```bash
#! /usr/bin/env nix-shell
#! nix-shell -i bash
#! nix-shell -p curl jq nix

set -eufo pipefail

FILE=$1
PROJECT=$2

OWNER=$(jq -r '.[$project].owner' --arg project "$PROJECT" < "$FILE")
REPO=$(jq -r '.[$project].repo' --arg project "$PROJECT" < "$FILE")
DEFAULT_BRANCH=$(jq -r '.[$project].branch // "master"' --arg project "$PROJECT" < "$FILE")

BRANCH=${3:-$DEFAULT_BRANCH}

REV=$(curl "https://api.github.com/repos/$OWNER/$REPO/branches/$BRANCH" | jq -r '.commit.sha')
SHA256=$(nix-prefetch-url --unpack "https://github.com/$OWNER/$REPO/archive/$REV.tar.gz")
TJQ=$(jq '.[$project] = {owner: $owner, repo: $repo, branch: $branch, rev: $rev, sha256: $sha256}' \
  --arg project "$PROJECT" \
  --arg owner "$OWNER" \
  --arg repo "$REPO" \
  --arg branch "$BRANCH" \
  --arg rev "$REV" \
  --arg sha256 "$SHA256" \
  < "$FILE")
[[ $? == 0 ]] && echo "${TJQ}" >| "$FILE"
```
</details>

<details>
<summary style="cursor: pointer">`pkgs.nix`</summary>
```nix
let
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  versions = builtins.mapAttrs
    (_: fetcher)
    (builtins.fromJSON (builtins.readFile ./versions.json));
in versions
```
</details>

This approach still works, but in the meantime [Nix
flakes](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake)
have become the primary way to manage dependencies in Nix projects. Although
they're still listed as an experimental feature, the same is also true of the
`nix` command, and I don't think either is going away in the foreseeable
future.

## The fundamental insight

It turns out that you can replace `pkgs.nix`:

```nix
let
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  versions = builtins.mapAttrs
    (_: fetcher)
    (builtins.fromJSON (builtins.readFile ./versions.json));
in versions
```

using the relatively new `fetchTree` builtin:

```nix
let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  versions = builtins.mapAttrs
    (_: node: (builtins.fetchTree node.locked).outPath)
    lock.nodes;
in versions
```

following which you can replace `updater` with [`nix flake
update`](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake-update)
and `versions.json` with `flake.lock`.

## Flakes griping

I've done my best to avoid flakes for as long as possible, since there are
a couple of UI/UX issues that bother me:

### A reliance on new-style `nix` commands

I'm pretty comfortable with `nix-build` and `nix-shell`, and it's an adjustment
to use the newer `nix build` and `nix develop` commands since they don't work
exactly the same (e.g. not printing build logs by default, having to use `.#`
for packages).

### Coupling dependency and systems concerns

The flakes position is that `system` is an impurity (which is reasonable
enough) and so each output is parametrised by the system and there's no
built-in way to ignore or work around this. In practice I've seen most people
use [`flake-utils`](https://github.com/numtide/flake-utils) and its provided
`eachSystem` or `eachDefaultSystem` functions. For my purposes I haven't run
into any issues with `eachDefaultSystem` and if you are shaking your head at
the screen thinking of all the ways this can go wrong then you probably don't
need to read this blog post. Unfortunately `eachDefaultSystem` doesn't save you
from having to supply `system` to `nixpkgs` everywhere you import it, which
makes adapting existing non-flakes projects with multiple imports of `nixpkgs`
tedious to migrate.

### Surprising interactions with `git`

Strange and confusing things can happen when you try to use a file that's
currently untracked by `git`. Often it will tell you it can't find a particular
file, even though it's *right there*, but at other times things will appear to
work but your language-specific build tool will complain. The obvious solution
is to always `git add` everything you care about, but that has the same energy
as "I would simply write code with no bugs at all times" and is equally
non-actionable advice. The only hint you get is the message

`warning: Git tree '<project root>' is dirty`

as your build commences which is more often than not innocuous. I foresee
myself running into this issue over and over again when using flakes.

## Why bother with flakes?

Although I'm still critical of certain aspects of flakes, they do provide one
feature I was missing: the ability to manage and update dependencies without
the use of
[IFD](https://nixos.org/manual/nix/stable/language/import-from-derivation).
I also get the impression that the vast majority of effort being put into Nix
now is in and around the flakes ecosystem, e.g.
[FlakeHub](https://flakehub.com/) and [the `update-flake-lock` GitHub
Action](https://github.com/DeterminateSystems/update-flake-lock). Keeping all
this in mind, I think there is a way to ignore most of the stuff I don't care
about for now while getting rid of my primitive shell script in favour of
robust and better-supported dependency management code that's built into Nix.
That way I can gradually integrate flakes more deeply, and if I'm wrong about
it being the future I still have the option to go back to what I was using
before (or adopt whatever the new hotness is).

## A minimal flake

The first hurdle to overcome is replacing `default.nix` with `flake.nix`. I've
found that this is a good starting point for me:

```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat.url = "github:edolstra/flake-compat";
  outputs = {nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs { inherit system; };
      # ...
    in {
      defaultPackage = null;
      devShell = null;
    });
}
```

combined with this snippet in `default.nix` taken from the [`flake-compat`
README](https://github.com/edolstra/flake-compat/blob/0f9255e01c2351cc7d116c072cb317785dd33b33/README.md#usage):

```nix
(import
  (
    let lock = builtins.fromJSON (builtins.readFile ./flake.lock); in
    fetchTarball {
      url = lock.nodes.flake-compat.locked.url or "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
      sha256 = lock.nodes.flake-compat.locked.narHash;
    }
  )
  { src = ./.; }
).defaultNix
```

A couple of things are worth pointing out:

- I include `flake-compat` in my inputs but I don't actually use it in
  `flake.nix`, it is declared solely so that it can be tracked in `flake.lock`.
- I could include more dependencies here, as long as `nix flake update` knows
  how to fetch them, which is already a huge improvement over my
  GitHub-specific `updater` script.
- If your input is a flake but you're not using it in `flake.nix`, you probably
  want to set `inpugs.<input>.flake = false` so that it doesn't pull in that
  flake's dependencies too.
- The `default.nix` snippet doesn't have the old Nix behaviour of doing the
  right thing when used with `nix-shell`, but I could probably recover this by
  including (or reimplementing)
  [`lib.inNixShell`](https://github.com/NixOS/nixpkgs/blob/9b5d456802b2322b36b69dca65b04095877495ad/lib/trivial.nix#L235-L240)
  and using it.

## Migrating all the things

I recently went on a tear, moving a bunch of my repositories over to this workflow:

- [`resume`](https://github.com/vaibhavsagar/resume/commit/a698b2df6e37c67bc05b57547d345883e76eb491)
- [`notebooks`](https://github.com/vaibhavsagar/notebooks/commit/daa4f593f2bcacdb318f57525887c853db838304)
- [`website`](https://github.com/vaibhavsagar/website/commit/760a4b4c8f44347fbe3bb39203383c4a1b42178d)

It was reasonably straightforward, except in the case of `notebooks`, where
I have a bunch of expressions that each have their own overlays etc. that
I wasn't ready to unify just yet. This meant a lot of `{ system
? builtins.currentSystem }` which I could have done without. It's an
anti-pattern to import `nixpkgs` in multiple places anyway, so this is probably
a sign that there is a better way to organise my expressions.

## Further reading

I was partly inspired to try this after reading Jade Lovelace's [excellent blog
post](https://jade.fyi/blog/flakes-arent-real/) about Nix flakes. Thank you
Jade!
