--------------------------------------------------------------------------------
title: GHCi in the Browser
published: 2024-07-03
tags: haskell, programming
--------------------------------------------------------------------------------

I'm happy to announce that you can now run GHCi entirely in your browser (if
your browser supports WebAssembly and you're willing to download approximately
220MB of compressed WASM).

## Where?

[Here](https://vaibhavsagar.com/amd64-ghc-wasi-demo) or
[here](https://vaibhavsagar.com/webvm/).

## How?

I used [`container2wasm`](https://github.com/ktock/container2wasm) to convert
an OCI image containing GHC to a WASM blob that I could serve using a lightly
modified [`container2wasm-demo`](https://github.com/ktock/container2wasm-demo).
If you're curious, the website repo is
[here](https://github.com/vaibhavsagar/amd64-ghc-wasi-demo) and the chunks of
WASM are [here](https://github.com/vaibhavsagar/amd64-ghc-wasi-container).

As of this writing, only images with an uncompressed size below 2GB can be used
with `container2wasm` (tracked
[here](https://github.com/ktock/container2wasm/issues/230)) and my initial
attempts using an OCI image generated by Nix were unsuccessful because of
duplicate filenames (tracked
[here](https://github.com/ktock/container2wasm/issues/263)).

I also separately used [WebVM](https://webvm.io/) which has the same file size
limitation as `container2wasm`, only works on `x86` binaries (which is why
I used the `i386` build of GHC), and is closed-source 😢, but potentially offers
better performance depending on how well the JIT compiler performs on this
workload.

## Why?

I've wanted to do something like this for a long time. In my capacity as
a maintainer of [IHaskell](https://github.com/IHaskell/IHaskell), installation
issues are the most common category of support request I receive. Wouldn't it
be great if a user could simply navigate to a webpage and have a correctly
configured Jupyter notebook waiting for them? The Jupyter folks also seem to be
thinking the same thing, based on the existence of
[JupyterLite](https://jupyterlite.readthedocs.io/en/stable/). Unfortunately
we're a long way off from Haskell support[^1], but I hope my proof-of-concept
shows that this is possible.

Even outside Jupyter-land, a fully-functional GHCi REPL in the browser would be
generally useful. For example, currently
[Haskell.org](https://www.haskell.org/) has a "Try it!" section where you can
enter expressions, which are currently passed to a backend server to execute.
A client-side GHCi could provide a better experience and allow us to get rid of
the backend entirely. Another wild idea: the Hackage documentation for
a package could provide a REPL with that package pre-installed for users to try
out immediately. Wouldn't that be amazing?

## Why not compile GHCi directly to JavaScript/WASM using the new backends?

I don't think that would work/result in a usable Haskell interpreter with
access to `base` or other GHC boot packages. As of this writing it is on [the
roadmap for GHC
9.12+](https://gitlab.haskell.org/ghc/ghc/-/wikis/javascript-backend?version_id=bff087ec5b0231e12b3a8d902522f3d41aed530b)
so hopefully that will eventually be possible (tracked
[here](https://gitlab.haskell.org/ghc/ghc/-/issues/25067)). If you get this
working I'd love to know about it!


[^1]: It's not something I'm working on and I don't know how to go from this
Goldbergian blob of WASM to a kernel that would work with JupyterLite. If you
have ideas, please get in touch!