--------------------------------------------------------------------------------
title: Three Pillars of Nix
published: 2018-06-15
tags: programming, nix
--------------------------------------------------------------------------------

When I first encountered Nix, it seemed magical, and not in a good way.
Instructions such as "copy Firefox with all its dependencies to a remote
machine with this one command" and "change these two lines in
`configuration.nix`, rebuild, and now you're running a hypervisor" were
impressive but inscrutable. Here was something that claimed to solve packaging
and dependency management once and for all, but what made it work? I found it
very difficult to reconcile the experience of using it with the knowledge that
it was just Linux at the end of the day and everything was essentially shell
scripts that were being run more or less like they would be with any other
packaging toolchain.

After more than a year of using it professionally and personally, I think I've
narrowed it down to three methods that Nix uses to achieve its goals: hashing,
environment variables, and symbolic links. Let's go through them in order.

## Hashing

To build something, Nix first generates a recipe (known as a *derivation*) and
then executes it, using a hash of the recipe to uniquely identify the output.
The reason it does this instead of hashing the output is that most build
processes don't produce deterministic outputs and changing this would require
superhuman levels of engineering effort (see
[reproducible-builds.org](https://reproducible-builds.org/) for more about
this). The recipe includes the hashes of all dependencies, which in turn
include the hashes of their dependencies, and so on. This means that Nix can
cheaply check if a build has been performed before, and reuse dependencies if
they are present instead of compiling the universe from source each time a
build needs to happen. It also means that build outputs can be cached remotely
or even built remotely, with Nix transparently copying over the inputs, running
the build process, and copying the output back to your local machine.

## Environment Variables

As much as possible, Nix uses environment variables instead of file paths to
make dependencies available at build time. This is because environment
variables are much easier to control and manipulate. `nix-build` sets up a
sandbox with only the relevant environment variables set, and `nix-shell` works
similarly, by changing the `$PATH` and associated environment variables to
create a shell environment that vanishes when you don't need it anymore. To
ensure that no stray values leak into your shell, it even has a `--pure` flag
that unsets them all before specifying its own.

## Symbolic Links

Symbolic links are important because switching the target of a symbolic link
can be done atomically on all POSIX-compliant systems, and symbolic links are
how Nix addresses the mismatch between its content-addressable store and the
deeply ingrained assumptions everything else has about how a Linux system
behaves. Nix does all its builds in a sandbox, puts the outputs in its store,
and only then creates/switches a symlink pointing to the result of the build in
the current directory. This means that if any phase of the build fails, the
user's working directory is unaffected, and if a symlink exists, it always
points to a successful build output. NixOS takes this one step further and
builds an entirely new system configuration before switching it over, which
means that the system is always in a working state, even if something bad like
a power outage happens.

What would a build tool look like without one of these pillars?

Without hashing, we might have build outputs that are equivalent but no way to
check this, and we'd have to recompile the universe from scratch each time we
wanted to do a build.

Without full control of environment variables, we can't ensure that only the
dependencies we specify are used to produce the build output, and our builds
might complete successfully on our machine but not on another, which is no
better than the current non-Nix state of affairs.

Without symbolic links, we might have broken build outputs and/or system
configurations if intermediate files were put in place during a failed build.
We wouldn't be able to trust that a build either fails or produces a complete
output and Nix would be completely unsuitable for system deployment.

I hope this provides some more insight into how Nix achieves its goals. I'm
more confident that there's no magic here, just difficult and tedious
engineering!
