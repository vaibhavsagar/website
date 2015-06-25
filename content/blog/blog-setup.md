Title: How This Static Blog Uses Travis-CI
Date: 2015-02-01
Category: programming

When I started this blog, I was reluctant to put a continuous integration
system like Travis-CI in charge of deployment because it seemed like a
frivolous use of resources and I settled for the approach outlined
[here](http://mathamy.com/migrating-to-github-pages-using-pelican.html) which
worked reasonably well. However, I recently realised that automating my
deployment would allow me to theoretically create a post using the GitHub web
interface which would automatically get turned into a new post on my blog.
This would allow me to use this blog from any web browser! In other words, I
wanted a convoluted reimplementation of half of a CMS's functionality.

I found a very helpful [blog
post](http://zonca.github.io/2013/09/automatically-build-pelican-and-publish-to-github-pages.html)
that covered most of the details I needed. Let me go through the changes I
made.

First off, I use Python 3 wherever possible so I simplified my
`requirements.txt`:

<script
src="http://gist-it.appspot.com/github.com/vaibhavsagar/website/blob/master/requirements.txt"></script>

My `.travis.yml` is different too (I removed unnecessary nesting to make it
shorter):

<script
src="http://gist-it.appspot.com/github.com/vaibhavsagar/website/blob/master/.travis.yml"></script>

I installed the `travis` gem, got a GitHub token, and tried to set the token
with

    travis encrypt GH_TOKEN=LONGTOKENFROMGITHUB --add env.global

but it didn't recognise my repository until I went to my [Travis-CI
profile](https://travis-ci.org/profile/), enabled GitHub access, and enabled it
to check my repository.

The `deploy.sh` script was almost the same (change your username):

<script
src="http://gist-it.appspot.com/github.com/vaibhavsagar/website/blob/master/deploy.sh"></script>

and that was all the setup I had to do. I then pushed a commit and watched as
Pelican and its dependencies were downloaded, my blog posts were regenerated,
and my GitHub Pages repo was updated. Seems a bit wasteful, but it was
surprisingly easy to get working.

