--------------------------------------------------------------------------------
title: Getting Along with JavaScript
published: 2019-10-28
tags: programming, haskell, nix
--------------------------------------------------------------------------------

For the last couple of weeks, I've been obsessed with the idea of running
Haskell in the browser. I know this is possible, because this is what I do at
work every day, but the applications I work on professionally are complex
beasts with Haskell backends and dedicated servers making them available to
users. I'm looking for something lighter that I can serve statically using
GitHub Pages or [Glitch](https://glitch.com), so I can plop some code on a
webpage and never worry about hosting ever again.

If you'd like to follow along, I have the code available [at this
gist](https://gist.github.com/vaibhavsagar/24b1754b8a269fd8c54a89cb73e64fa8)
with each revision representing a step in the progression.

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
Platform](https://github.com/reflex-frp/reflex-platform) directly, which is a
set of changes and overrides to a revision of
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

*([revision](https://gist.github.com/vaibhavsagar/24b1754b8a269fd8c54a89cb73e64fa8/c21e62ecdcc053273ee5e4815ef538e1e8a29e55#file-versions-json))*

The next step is to get a Haskell project skeleton in place. I used `cabal
init` for this as follows:

```bash
$ nix-shell -p ghc cabal-install --run 'cabal init -lBSD3'
```

*([revision](https://gist.github.com/vaibhavsagar/24b1754b8a269fd8c54a89cb73e64fa8/d4c4f9ff39b595f5c8858892328adfa6ab4a4cc8#file-small-viz-cabal))*

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

*([revision](https://gist.github.com/vaibhavsagar/24b1754b8a269fd8c54a89cb73e64fa8/348b759019bc19aec9833a6b5f042c1d2f5e9b13#file-default-nix))*

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

*([revision](https://gist.github.com/vaibhavsagar/24b1754b8a269fd8c54a89cb73e64fa8/93c510d77f7a8d6b1d8d63bb1cb0be37c6d575b5#file-main-hs))*

We also have to add `reflex-dom` and `reflex` to our dependencies in our
`.cabal` file, and then we can get a automatically-reloading development build
with one command:

```bash
$ nix-shell -A shells.ghc --run 'ghcid -T "Main.main" --command "cabal new-repl"'
```

This allows a native Haskell process to control a web page, so we can navigate
to it using our browser at `http://localhost:3003` and have a fast feedback
loop. In practice there is a lot of brower refreshing involved, but this is
still much nicer than having to do a GHCJS build each time we want to look at
our changes. Now we have an input box that repeats what we type into it, which
is a good start. I should point out that this works a lot better on Google
Chrome (or Chromium) than it does on Firefox, and that's what I'll be using for
development. The final GHCJS output does not have this limitation.

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

<details>
<summary style="cursor: pointer">`Main.hs`</summary>
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main = mainWidget $ el "div" $ do
  t <- textArea def
  el "div" $
    dynText $ _textArea_value t
```
</details>

*([revision](https://gist.github.com/vaibhavsagar/24b1754b8a269fd8c54a89cb73e64fa8/d5ff4725b26db3dd596abb2e751711f5c568b6bc#file-main-hs))*

The latest version of Viz.js is available
[here](https://www.jsdelivr.com/package/npm/viz.js), and we can include it
using `mainWidgetWithHead`:

<details>
<summary style="cursor: pointer">`Main.hs`</summary>
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main = mainWidgetWithHead widgetHead $ el "div" $ do
  t <- textArea def
  el "div" $
    dynText $ _textArea_value t
  where
    widgetHead :: DomBuilder t m => m ()
    widgetHead = do
      script "https://cdn.jsdelivr.net/npm/viz.js@2.1.2/viz.min.js"
      script "https://cdn.jsdelivr.net/npm/viz.js@2.1.2/full.render.min.js"
    script src = elAttr "script" ("type" =: "text/javascript" <> "src" =: src) blank
```
</details>

*([revision](https://gist.github.com/vaibhavsagar/24b1754b8a269fd8c54a89cb73e64fa8/6a856c34755f730793f3b588a82f0fc9f836bf9c#file-main-hs))*

Now we can poke around with our browser developer tools until we have a useful
JavaScript function. Here's what I came up with, based on the examples in the
[wiki](https://github.com/mdaines/viz.js/wiki/Usage#using-a-script-tag):

```javascript
function(e, string) {
  var viz = new Viz();
  viz.renderSVGElement(string)
  .then(function(element) {
    e.innerHTML = element.outerHTML;
  })
  .catch(function(error) {
    e.innerHTML = error;
  })
}
```

Then we can start thinking about how we want to do JavaScript interop! Although
there is a GHCJS FFI as described [in the
wiki](https://github.com/ghcjs/ghcjs/wiki/A-few-examples-of-Foreign-Function-Interface),
this doesn't seem to work at all with GHC, and that means we can't use it
during development. I don't think that's good enough, and fortunately we don't
have to settle for this and instead can use
[`jsaddle`](http://hackage.haskell.org/package/jsaddle-0.9.6.0), which
describes itself as "an EDSL for calling JavaScript that can be used both from
GHCJS and GHC". We can add `jsaddle` to our dependencies, add `Viz` to the
`exposed-modules` stanza in our `.cabal` file, and create a new module `Viz`,
and then we can use the `eval` and `call` functions to call our JavaScript
directly:

<details>
<summary style="cursor: pointer">`Viz.hs`</summary>
```haskell
module Viz where

import Language.Javascript.JSaddle

viz :: JSVal -> JSVal -> JSM ()
viz element string = do
  call vizJs vizJs [element, string]
  pure ()

vizJs :: JSM JSVal
vizJs = eval
  "(function(e, string) { \
  \  var viz = new Viz(); \
  \  viz.renderSVGElement(string) \
  \  .then(function(element) { \
  \    e.innerHTML = element.outerHTML; \
  \  }) \
  \  .catch(function(error) { \
  \    e.innerHTML = error; \
  \  }) \
  \})"
```
</details>

*([revision](https://gist.github.com/vaibhavsagar/24b1754b8a269fd8c54a89cb73e64fa8/b234b2649022b1b560df1281f053bac30289ce12#file-viz-hs))*

JSaddle runs operations in `JSM`, which is similar to `IO`, and all functions
take values of type `JSVal` that can be represented as JavaScript values. We
pass `vizJs` to `call` twice because the second parameter represents the `this`
keyword.

Wiring everything up together is just a few more lines of code:

<details>
<summary style="cursor: pointer">`Main.hs`</summary>
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import Language.Javascript.JSaddle (liftJSM, toJSVal)
import Viz (viz)

main = mainWidgetWithHead widgetHead $ el "div" $ do
  t <- textArea def
  e <- _element_raw . fst <$> el' "div" blank
  performEvent_ $ ffor (updated (_textArea_value t)) $ \text -> liftJSM $ do
    jsE <- toJSVal e
    jsT <- toJSVal text
    viz jsE jsT
  where
    widgetHead :: DomBuilder t m => m ()
    widgetHead = do
      script "https://cdn.jsdelivr.net/npm/viz.js@2.1.2/viz.min.js"
      script "https://cdn.jsdelivr.net/npm/viz.js@2.1.2/full.render.min.js"
    script src = elAttr "script" ("type" =: "text/javascript" <> "src" =: src) blank
```
</details>

*([revision](https://gist.github.com/vaibhavsagar/24b1754b8a269fd8c54a89cb73e64fa8/e0e886959e338803f9c4a1a3596f8eb88474424d#file-main-hs))*

There's a lot going on here, so I'll explain in a little more detail.

Instead of an element which displays the textarea contents as they are updated,
we just want a reference to a blank `<div>`, so we use the
[`el'`](https://hackage.haskell.org/package/reflex-dom-core-0.5/docs/Reflex-Dom-Widget-Basic.html#v:el-39-)
function and pull out the raw element.
[`performEvent_`](http://hackage.haskell.org/package/reflex-0.6.2.4/docs/Reflex-PerformEvent-Class.html#v:performEvent_)
mediates the interaction between Reflex and side-effecting actions, like our
function that updates the DOM with a rendered graph, so we want to use it to
render a new graph every time the textarea is updated.

An introduction to Reflex is out of scope for this blog post, but it's worth
mentioning that the textarea value is represented as a
[`Dynamic`](http://hackage.haskell.org/package/reflex-0.6.2.4/docs/Reflex-Class.html#t:Dynamic),
which can change over time and notify consumers when it has changed. This can
be thought of as the combination of a related
[`Behavior`](http://hackage.haskell.org/package/reflex-0.6.2.4/docs/Reflex-Class.html#t:Behavior)
and
[`Event`](http://hackage.haskell.org/package/reflex-0.6.2.4/docs/Reflex-Class.html#t:Event).
`performEvent_` only takes an `Event`, and we can get the underlying `Event`
out of a `Dynamic` with
[`updated`](http://hackage.haskell.org/package/reflex-0.6.2.4/docs/Reflex-Class.html#v:updated).

`ffor` is just `flip fmap`, and we use it to operate on the underlying `Text`
value, convert both it and the reference to the element we want to update to
`JSVal`s, and then pass them as arguments to the `viz` function we defined
earlier. Now we should have a working GraphViz renderer in our browser!

We could stop here, but I think we can do better than evaluating JavaScript
strings. JSaddle is an EDSL, so we can rewrite our JavaScript in Haskell:

<details>
<summary style="cursor: pointer">`Viz.hs`</summary>
```haskell
module Viz where

import Language.Javascript.JSaddle

viz :: JSVal -> JSVal -> JSM ()
viz element string = do
  viz <- new (jsg "Viz") ()
  render <- viz # "renderSVGElement" $ [string]
  result <- render # "then" $ [(fun $ \_ _ [e] -> do
    outer <- e ! "outerHTML"
    element <# "innerHTML" $ outer
  )]
  result # "catch" $ [(fun $ \_ _ [err] ->
    element <# "innerHTML" $ err
  )]
  pure ()
```
</details>

*([revision](https://gist.github.com/vaibhavsagar/24b1754b8a269fd8c54a89cb73e64fa8/96e0dbda1ba9dc5712342bf1b123fe5d463201d0#file-viz-hs))*

This is recognisably the same logic as before, but using some new JSaddle operators:

- [`#`](http://hackage.haskell.org/package/jsaddle-0.9.6.0/docs/Language-Javascript-JSaddle.html#v:-35-)
  is for calling a JavaScript function
- [`!`](http://hackage.haskell.org/package/jsaddle-0.9.6.0/docs/Language-Javascript-JSaddle.html#v:-33-)
  is for property access
- [`<#`](http://hackage.haskell.org/package/jsaddle-0.9.6.0/docs/Language-Javascript-JSaddle.html#v:-60--35-)
  is a setter

Note also that all callables take a list of `JSVal`s as arguments, since
JSaddle doesn't know how many arguments we intend to pass in advance.

This is an improvement, but we can do even better using the lensy API (after
adding `lens` to our dependencies):

<details>
<summary style="cursor: pointer">`Viz.hs`</summary>
```haskell
module Viz where

import Language.Javascript.JSaddle
import Control.Lens ((^.))

viz :: JSVal -> JSVal -> JSM ()
viz element string = do
  viz <- new (jsg "Viz") ()
  render <- viz ^. js1 "renderSVGElement" string
  result <- render ^. js1 "then" (fun $ \_ _ [e] -> do
    outer <- e ! "outerHTML"
    element ^. jss "innerHTML" outer)
  result ^. js1 "catch" (fun $ \_ _ [err] ->
    element ^. jss "innerHTML" err)
  pure ()
```
</details>

*([revision](https://gist.github.com/vaibhavsagar/24b1754b8a269fd8c54a89cb73e64fa8/2ede687d9969666897fb1ca944ed83d239b4386b#file-viz-hs))*

Not much has changed except that we can use convenience functions like
[`js1`](http://hackage.haskell.org/package/jsaddle-0.9.6.0/docs/Language-Javascript-JSaddle.html#v:js1)
and
[`jss`](http://hackage.haskell.org/package/jsaddle-0.9.6.0/docs/Language-Javascript-JSaddle.html#v:jss).

I'm told that there is some overhead to using JSaddle which it's possible to
get rid of by using a library like
[`ghcjs-dom`](https://hackage.haskell.org/package/ghcjs-dom), but I haven't
explored this approach and I will leave this as an exercise for the reader. If
you learn how to do this, please teach me!

Now we are able to run Haskell on the frontend without having to write any
JavaScript ourselves. The final step is to put this on the internet somewhere!

Building with GHCJS is straightforward:

```bash
$ nix-build -A ghcjs.small-viz
```

I'm enamoured of the idea of deploying this to [Glitch](https://glitch.com/),
so let's look into doing that. The `index.html` created by the default GHCJS
build is unnecessary, and we can simplify it:

<details>
<summary style="cursor: pointer">`index.html`</summary>
```html
<!DOCTYPE html>
<html>
  <head>
    <script language="javascript" src="all.js"></script>
  </head>
  <body>
  </body>
</html>
```
</details>

The only JavaScript file that needs to be copied over is then `all.js`. We can
write a `glitch.nix` file to simplify this process:

<details>
<summary style="cursor: pointer">`glitch.nix`</summary>
```nix
let
  # ./updater versions.json reflex-platform
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  reflex-platform = fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).reflex-platform;
  pkgs = (import reflex-platform {}).nixpkgs;
  project = import ./default.nix;
  html = pkgs.writeText "index.html" ''
    <!DOCTYPE html>
    <html>
      <head>
        <script language="javascript" src="all.js"></script>
      </head>
      <body>
      </body>
    </html>
  '';
in pkgs.runCommand "glitch" {} ''
  mkdir -p $out
  cp ${html} $out/index.html
  cp ${project.ghcjs.small-viz}/bin/small-viz.jsexe/all.js $out/all.js
''
```
</details>

*([revision](https://gist.github.com/vaibhavsagar/24b1754b8a269fd8c54a89cb73e64fa8/a0127badaee44f316156121153c0e4bc41af9460#file-glitch-nix))*

And then produce the files we need to copy over with:

```bash
$ nix-build glitch.nix
```

I've gone ahead and done this, and it's up on
[small-viz.glitch.me/](https://small-viz.glitch.me/).

Now that everything's working, it would be nice to reduce the size of `all.js`,
which is currently over 5MB. Obelisk uses the [Closure
Compiler](https://developers.google.com/closure/compiler) to minify JavaScript,
and we can adapt [what it
does](https://github.com/obsidiansystems/obelisk/blob/071e2edb92e623b4415fb6deedc4219ad1f829f0/default.nix#L147)
and [another example by Tom
Smalley](https://github.com/tomsmalley/marking/blob/a522b8c75a96146883a7e32acf5b17bb5f4abf1b/makefile#L5-L10)
that I found when I was looking into this to update `glitch.nix`:

<details>
<summary style="cursor: pointer">`glitch.nix`</summary>
```nix
let
  # ./updater versions.json reflex-platform
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  reflex-platform = fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).reflex-platform;
  pkgs = (import reflex-platform {}).nixpkgs;
  project = import ./default.nix;
  html = pkgs.writeText "index.html" ''
    <!DOCTYPE html>
    <html>
      <head>
        <script language="javascript" src="all.js"></script>
      </head>
      <body>
      </body>
    </html>
  '';
in pkgs.runCommand "glitch" {} ''
  mkdir -p $out
  cp ${html} $out/index.html
  ${pkgs.closurecompiler}/bin/closure-compiler \
    --externs=${project.ghcjs.small-viz}/bin/small-viz.jsexe/all.js.externs \
    --jscomp_off=checkVars \
    --js_output_file="$out/all.js" \
    -O ADVANCED \
    -W QUIET \
    ${project.ghcjs.small-viz}/bin/small-viz.jsexe/all.js
''
```
</details>

*([revision](https://gist.github.com/vaibhavsagar/24b1754b8a269fd8c54a89cb73e64fa8/aac5fe1258ccfc8c9b8ca685b9db1a4f538ae183#file-glitch-nix))*

And this brings the size down to under 2MB.
