--------------------------------------------------------------------------------
title: Fun with GitHub Actions
published: 2020-03-21
tags: nix, programming
--------------------------------------------------------------------------------

GitHub Actions has been generally available for a while now, but if you already
have painstakingly crafted CI infrastructure using another provider such as
Travis, it's not clear whether it's worth switching over and potentially having
to do a lot of work all over again. After experimenting with some of my own
repositories, I'm going to talk about how I've successfully used it and when
I think it makes (or doesn't make) sense to use it.

## GHC-in-GHCi CI

When I learned about
[GHC-in-GHCi](https://gitlab.haskell.org/ghc/ghc/wikis/building/in-ghci), I was
thrilled and quickly wrote [a short blog
post](https://vaibhavsagar.com/blog/2019/06/22/easy-ghc-hacking/) about how
great it is. Immediately afterwards, though, I began to worry: the instructions
worked at the time of writing, but how could I ensure that they would remain
correct in the future? It would be a very bad experience for someone to come
across the blog post a few months later, follow the steps, and yet end up with
things not working. I decided it would be a good idea to continuously test
these instructions, and I tried to set this up with Travis. I immediately ran
into disk space and build time limits. I'd heard CircleCI was better, so
I tried that instead, and I ran into similar issues.

Finally I decided it would be a good time to try GitHub Actions. The CI runs on
Microsoft Azure, and their free tier is very generous: I haven't been able to
find a build time limit for public repositories, and the machines appear to be
significantly faster than the ones Travis CI uses. Setting up a workflow is
also easier than Travis, because there are predefined actions that can be
easily included. Since my instructions required Nix, I needed to have it
installed, and the fine people behind [Cachix](https://cachix.org/) had gone
ahead and [done the work for
me](https://github.com/cachix/install-nix-action/). Another significant
improvement over Travis is the granularity of the `cron` support: GitHub
actions can run every hour at most, while Travis builds cannot be configured to
run more frequently than once a day. I was happy to test my instructions daily,
but it's good to know that I can do something more often if I feel like it. In
the end I was able to repurpose my testing script from my earlier attempts and
the rest of the workflow was straightforward:

```yaml
name: "Test"
on:
  pull_request:
  push:
  schedule:
    - cron: '0 1 * * *'
jobs:
  tests:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v1
    - uses: cachix/install-nix-action@v6
    - run: ./steps.sh
```

Here's what each section is doing:

```yaml
name: "Test"
```

This is what the workflow is called in the UI.

```yaml
on:
  pull_request:
  push:
  schedule:
    - cron: '0 1 * * *'
```

This action runs on every push, pull request, and at 1:00AM every day.

```yaml
jobs:
  tests:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v1
    - uses: cachix/install-nix-action@v6
    - run: ./steps.sh
```

There's one job (called `tests`) which runs on the latest Ubuntu virtual
machine image and

1. Checks out the repository using the `actions/checkout` action tagged `v1`
1. Installs Nix using the `cachix/install-nix-action` action tagged `v6`
1. Runs `steps.sh`

GitHub Actions distinguish between `uses` for actions and `run` for scripts.

I think this example highlights the two biggest advantages of GitHub actions
over e.g. Travis CI:

1. Extremely generous resource limits
1. Preconfigured, reusable building blocks

You can see the final repository
[here](https://github.com/vaibhavsagar/ghc-dev-ci/).

## IHaskell Docker

I help maintain [IHaskell](https://github.com/gibiansky/IHaskell), and people
recently requested [an official IHaskell Docker
image](https://github.com/gibiansky/IHaskell/issues/1030). Although this seems
like a relatively easy request to fulfill, it's not feasible to include
a Docker build as part of our Travis CI setup, as this can easily take over 50
minutes on its own. I toyed with the idea of using Docker Hub's own CI
infrastructure to build the Docker image, but that only seems to work correctly
if it's wired up by the owner of the repository (not just someone with commit
permissions).

I also looked into replacing Travis CI with GitHub Actions completely, but it
turned out that the repository owner was on a legacy plan, and GitHub Actions
wasn't available. It seemed like I was stuck.

Eventually I realised that there was nothing stopping me from creating
a repository using my own account and effectively polling the IHaskell
repository using the `cron` support. This turned out to be even easier than my
previous example, because there is [a preconfigured action for building and
pushing a Docker
image](https://github.com/elgohr/Publish-Docker-Github-Action)! This is what
I came up with:

```yaml
name: "Docker"
on:
  push:
  schedule:
    - cron: '0 1 * * *'
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@722adc63f1aa60a57ec37892e133b1d319cae598
      with:
        repository: 'gibiansky/IHaskell'
    - uses: elgohr/Publish-Docker-Github-Action@a72734e15780689886c6518c4dc2e17876d05d4e
      with:
        name: gibiansky/ihaskell
        username: ${{ secrets.DOCKER_USERNAME }}
        password: ${{ secrets.DOCKER_PASSWORD }}
        cache: true
```

Here's what each section does:

```yaml
name: "Docker"
```

I called this workflow `Docker`.

```yaml
on:
  push:
  schedule:
    - cron: '0 1 * * *'
```

It runs on every push to this repository, but not on pull requests because
I didn't want to make it possible for a malicious actor to upload anything that
way. It also runs at 1:00AM every day.

```yaml
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@722adc63f1aa60a57ec37892e133b1d319cae598
      with:
        repository: 'gibiansky/IHaskell'
    - uses: elgohr/Publish-Docker-Github-Action@a72734e15780689886c6518c4dc2e17876d05d4e
      with:
        name: gibiansky/ihaskell
        username: ${{ secrets.DOCKER_USERNAME }}
        password: ${{ secrets.DOCKER_PASSWORD }}
        cache: true
```

There's one job (`build`) which pins each of its steps to a particular commit
and

1. Uses the `checkout` action to clone `gibiansky/IHaskell` by passing it as an
   argument
2. Uses the `elgohr/Publish-Docker-Github-Action` to build and push a Docker
   image to `gibiansky/ihaskell`, with caching enabled and credentials provided

Again, I'm happy with running this daily for now, but it's nice to know that
I can run it more frequently if I want to. The combination of generous resource
limits and preconfigured building blocks makes working like this very easy.
When the repository hasn't changed, the caching support ,which pulls down the
latest Docker image before doing anything else, means that nothing is pushed
and a CI run takes only a few minutes.

## Caching

Depending on your caching requirements, GitHub Actions may not be a better
choice than e.g. Travis CI. GitHub Actions provides 5GB of caching, which is
a good amount but not enough for
[IHaskell](https://github.com/gibiansky/IHaskell), which tests against multiple
GHC versions and needs a lot of cached dependencies to complete in a reasonable
amount of time.

If you're using Nix, I would recommend using a dedicated caching provider such
as [Cachix](https://cachix.org/), no matter which CI service you use.

In closing, if your code is hosted on GitHub and you want generous resources
and preconfigured building blocks, I would recommend taking a look at GitHub
Actions. I've since switched over a couple of other repositories and it's been
relatively smooth.
