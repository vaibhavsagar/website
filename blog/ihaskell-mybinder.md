--------------------------------------------------------------------------------
title: 'IHaskell on mybinder.org'
published: 2018-03-31
tags: haskell, programming
--------------------------------------------------------------------------------

[mybinder.org](https://mybinder.org/) is a website that will freely host a
GitHub repository as a collection of interactive Jupyter notebooks. It has
excellent built-in support for some of the more popular Jupyter kernels such as
Python and Julia, but it also has support for building a `Dockerfile` of your
choosing.

This means that getting an IHaskell installation up and running can be as
simple as clicking a link, and I'm happy to report that I've gotten this
working! You can [try it
here](https://mybinder.org/v2/gh/gibiansky/IHaskell/master).

I started with the simplest thing I could think of, which was to install system
dependencies and Jupyter to an Ubuntu container and then run `stack install
ihaskell`.

Once I had that working, my next step was to roll these changes into
[IHaskell's `Dockerfile`](https://github.com/gibiansky/IHaskell/pull/848) and
now everyone can use IHaskell on mybinder.org!

I've also [pushed an image to Docker
Hub](https://hub.docker.com/r/vaibhavsagar/ihaskell/) that you can use as a
base for your own repositories. A minimal example of a valid `Dockerfile` looks
something like this:

```Dockerfile
FROM vaibhavsagar/ihaskell:4afa0aee339e
```

Ideally I would also like to get this running with Nix, because that approach
results in a much smaller image and is easily extensible just by editing
`default.nix`. I have a small repo that builds but doesn't run on mybinder.org,
and I've [opened an issue](https://github.com/jupyterhub/binder/issues/87).

I'm pretty excited about what this means for the Jupyter ecosystem and IHaskell
in particular, and I'm looking forward to seeing what other people do with
this!
