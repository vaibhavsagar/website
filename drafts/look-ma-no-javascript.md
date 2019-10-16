--------------------------------------------------------------------------------
title: 'Look Ma, No JavaScript!'
published: 2019-10-15
tags: programming, haskell, nix
--------------------------------------------------------------------------------

For the last couple of weeks, I've been obsessed with the idea of running
Haskell in the browser. I know this is possible, because this is what I've been
doing at work for the last year and a bit, but the applications I work on
professionally are complex beasts with Haskell backends and dedicated servers
making them available to users. I'm looking for something lighter that I can
serve statically using GitHub Pages or [Glitch](https://glitch.com),
essentially plopping some code on a webpage and never worrying about hosting
ever again.

My first instinct was to reach for a tool like
[Obelisk](https://github.com/obsidiansystems/obelisk), which bills itself as
"an easy way to develop and deploy your Reflex project". Although it does work
as advertised(!), it is geared towards the needs of the large apps I mentioned
above.  It prerenders webpages where possible to make projects as snappy as
possible, works best within the confines of the Obelisk libraries, assumes at
least one NixOS target that will host your website, all of which means it
doesn't seem to scale down to my comparatively modest needs. It is possible to
use Obelisk anyway, but I found myself using too few of its features to justify
the effort, and I decided to move down a level and use [Reflex
Platform](https://github.com/reflex-frp/reflex-platform) directly.

The Reflex Platform is a set of changes and overrides to a revision of
[Nixpkgs](https://github.com/NixOS/nixpkgs) to best support building full-stack
and mobile Haskell applications.
