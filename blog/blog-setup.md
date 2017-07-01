--------------------------------------------------------------------------------
title: How This Static Blog Uses Travis-CI
published: 2015-02-01
tags: programming, automation
--------------------------------------------------------------------------------

_Update: I've switched to Hakyll, but the instructions below should continue to
work._

When I started this blog, I was reluctant to put a continuous integration
system like Travis-CI in charge of deployment because it seemed like a
frivolous use of resources and so I settled for the approach outlined
[here](http://mathamy.com/migrating-to-github-pages-using-pelican.html) which
worked reasonably well. As my understanding of continuous integration and the
possibilities of Travis improved, I changed my mind and decided it would be a
good idea to automate this process. I also realised that automating my
deployment would allow me to theoretically create or update a post using the
GitHub web interface which would automatically get turned into a new post on my
blog.  This would allow me to use this blog from any web browser! In other
words, I wanted a convoluted reimplementation of half of a CMS's functionality.

I found a very helpful [blog
post](http://zonca.github.io/2013/09/automatically-build-pelican-and-publish-to-github-pages.html)
that covered most of the details I needed. Let me go through the changes I
made.

First off, I use Python 3 wherever possible so I simplified my
`requirements.txt`:

```markdown
pelican
Markdown
```

My `.travis.yml` is different too (I removed unnecessary nesting to make it
shorter):

```yaml
branches:
  only: master
language: python
python: 3.6
cache: pip
script: make html
notifications:
  email:
    on_success: always
    on_failure: always
before_install: git submodule update --init --recursive
after_success: bash deploy.sh
env:
  global:
secure: Ew3edSrbYr/rofui49TpBTbmHivreifr0FLjqP+1CBPXERKEsCjs9thWBnjKIZYirfetMb5ShF9EZ1g8D459BS7Sn+ziXUP4X3I4jvdx1Yj55o2CuWOD6Gx6ShPkWEhZRZhFFIKpGfLO4XAKP3suSPJvB1Lp67GC0BFyPucSSU8=
```

I installed the `travis` gem, got a GitHub token, and tried to set the token
with

    travis encrypt GH_TOKEN=LONGTOKENFROMGITHUB --add env.global

but it didn't recognise my repository until I went to my [Travis-CI
profile](https://travis-ci.org/profile/), enabled GitHub access, and enabled it
to check my repository.

The `deploy.sh` script was almost the same (change your username):

```bash
#!/usr/bin/env bash
BRANCH=master
TARGET_REPO=vaibhavsagar/vaibhavsagar.github.io.git
PELICAN_OUTPUT_FOLDER=output

if [ "$TRAVIS_PULL_REQUEST" == "false" ]; then
    echo -e "Starting to deploy to Github Pages\n"
    if [ "$TRAVIS" == "true" ]; then
        git config --global user.email "travis@travis-ci.org"
        git config --global user.name "Travis"
    fi
    # using token, clone gh-pages branch
    git clone --depth 1 --quiet --branch=$BRANCH https://${GH_TOKEN}@github.com/$TARGET_REPO built_website > /dev/null
    # go into directory and copy data we're interested in to that directory
    cd built_website
    rsync -av --delete --exclude=.git  ../$PELICAN_OUTPUT_FOLDER/ ./
    # add, commit and push files
    git add -A
    git commit -m "Travis build $TRAVIS_BUILD_NUMBER pushed to Github Pages"
    git push -fq origin $BRANCH > /dev/null
    echo -e "Deploy completed\n"
fi
```

and that was all the setup I had to do. I then pushed a commit and watched as
Pelican and its dependencies were downloaded, my blog posts were regenerated,
and my GitHub Pages repo was updated. Seems a bit wasteful, but it was
surprisingly easy to get working and it performs admirably.
