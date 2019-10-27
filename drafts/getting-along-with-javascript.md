--------------------------------------------------------------------------------
title: Getting Along with JavaScript
published: 2019-10-15
tags: programming, haskell, nix
--------------------------------------------------------------------------------

For the last couple of weeks, I've been obsessed with the idea of running
Haskell in the browser. I know this is possible, because this is what I do at
work every day, but the applications I work on professionally are complex
beasts with Haskell backends and dedicated servers making them available to
users. I'm looking for something lighter that I can serve statically using
GitHub Pages or [Glitch](https://glitch.com), so I can plop some code on a
webpage and never worry about hosting ever again.

My first instinct was to reach for a tool like
[Obelisk](https://github.com/obsidiansystems/obelisk), which bills itself as
"an easy way to develop and deploy your Reflex project". Although it does work
as advertised(!), it is geared towards the needs of the large apps I mentioned
above.  It prerenders webpages where possible to make projects as snappy as
possible, works best within the confines of the Obelisk libraries, and assumes
at least one NixOS target that will host your website, all of which mean it
doesn't yet scale down to my comparatively modest needs. It is possible to use
Obelisk anyway, but I found myself using too few of its features to justify the
effort, and I decided to move down a level and use [Reflex
Platform](https://github.com/reflex-frp/reflex-platform) directly.

The Reflex Platform is a set of changes and overrides to a revision of
[Nixpkgs](https://github.com/NixOS/nixpkgs) to best support building full-stack
and mobile Haskell applications.

I like to use the `updater` script described in [a previous blog
post](/blog/quick-easy-nixpkgs-pinning), so I'll start by copying that over and
creating a `versions.json` with the following contents:

<details>
<summary style="cursor: pointer">`versions.json`</summary>
```json
{
  "reflex-platform": {
    "owner": "reflex-frp",
    "repo": "reflex-platform",
    "branch": "develop",
    "rev": "",
    "sha256": ""
  }
}
```
</details>

I can then update this by running:

```bash
$ ./updater versions.json reflex-platform
```

to get the latest `reflex-platform`. At the time of writing, this is the
revision I used:

<details>
<summary style="cursor: pointer">pinned `versions.json`</summary>
```json
{
  "reflex-platform": {
    "owner": "reflex-frp",
    "repo": "reflex-platform",
    "branch": "develop",
    "rev": "8f4b8973a06f78c7aaf1a222f8f8443cd934569f",
    "sha256": "167smg7dyvg5yf1wn9bx6yxvazlk0qk64rzgm2kfzn9mx873s0vp"
  }
}
```
</details>

The next step is to get a Haskell project skeleton in place. I used `cabal
init` for this as follows:

```bash
$ nix-shell -p ghc cabal-install --run 'cabal init -lBSD3'
```

which generated an executable-only project, just like I wanted. I named this
project `small-viz`, because it's a small project using the
[Viz.js](http://viz-js.com/) library, but more on that later.

The next step is to actually use `reflex-platform` to develop this project, for
which we need to write a little Nix. Here's the `default.nix` I used:

<details>
<summary style="cursor: pointer">`default.nix`</summary>
```nix
let
  # ./updater versions.json reflex-platform
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  reflex-platform = fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).reflex-platform;
in (import reflex-platform { system = builtins.currentSystem; }).project ({ pkgs, ... }: {
  useWarp = true;
  withHoogle = false;
  packages = {
    small-viz = ./.;
  };
  shells = {
    ghc = ["small-viz"];
    ghcjs = ["small-viz"];
  };
})
```
</details>

This sets up our project to build with both GHC and GHCJS, because we want to
develop with GHC but eventually use GHCJS to create our final artifact. I also
set a few more options:

1. `useWarp = true` changes the JSaddle backend to `jsaddle-warp` so we can
   develop using the browser, as described
   [here](https://github.com/reflex-frp/reflex-platform/blob/8f4b8973a06f78c7aaf1a222f8f8443cd934569f/docs/project-development.md#building-frontends-with-ghc).

2. `withHoogle = false` means we don't build a local Hoogle database every time
   our packages are updated, because this step is slow and I never used the
   local documentation anyway.

For the next step I'll assume binary cache substitution has been set up as
described
[here](https://github.com/reflex-frp/reflex-platform/blob/develop/notes/NixOS.md#enabling-the-binary-cache-on-nixos):

```bash
$ nix-shell -A shells.ghc
```

This should download a lot (and build almost nothing from source since we are
pulling from the cache), and then enter a shell environment with our
dependencies in scope.

Now we can start developing our Reflex app! We can start from the small example
described
[here](https://github.com/reflex-frp/reflex-platform/tree/8f4b8973a06f78c7aaf1a222f8f8443cd934569f#dynamics-and-events):

<details>
<summary style="cursor: pointer">`Main.hs`</summary>
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main = mainWidget $ el "div" $ do
  t <- inputElement def
  dynText $ _inputElement_value t
```
</details>

We also have to add `reflex-dom` and `reflex` to our dependencies in our
`.cabal` file, and then we can get a automatically-reloading development build
with one command:

```bash
$ nix-shell -A shells.ghc --run 'ghcid -T ":main" --command "cabal new-repl"'
```

This allows a native Haskell process to control a web page, so we can navigate
to it using our browser at `http://localhost:3003` and have a fast feedback
loop. In practice there is a lot of brower refreshing involved, but this is
still much nicer than having to do a GHCJS build each time we want to look at
our changes. Now we have an input box that repeats what we type into it, which
is a good start.

So where are we going with this? My plan is to build a crude version of the
[Viz.js](http://viz-js.com) homepage, where you can write
[DOT](https://en.wikipedia.org/wiki/DOT_(graph_description_language)) and see
it rendered instantly. Viz.js is the result of compiling the venerable
[Graphviz](http://graphviz.org/) to JavaScript using
[Emscripten](https://emscripten.org). It's no longer maintained but still works
fine as far as I can tell. In order to do this I want to use some kind of
JavaScript FFI to call out to `viz.js`, but first I want to swap out our text
input for a text area, and move the repeated output to just below the text area
instead of beside it.
