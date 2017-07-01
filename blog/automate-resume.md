--------------------------------------------------------------------------------
title: Generate A PDF From Your Resume With Travis-CI
published: 2015-02-26
tags: programming, automation
--------------------------------------------------------------------------------

After [automating my blog](/blog/2015/02/01/blog-setup), I decided to aim a little
higher and use Travis-CI to automate document generation from my resume. I use
[Pandoc](http://johnmacfarlane.net/pandoc/) to convert my source resume written
in Markdown to Github-flavoured Markdown, an HTML file for my website, and a
PDF (via LaTeX) that I've spent a ludicrous amount of time styling. I do have
all these packages configured on my home computer, but it was becoming tedious
to make changes and commit three separate files instead of just one. I was also
finding it difficult to keep the PDF version of my resume up to date with the
rest of my changes, and I thought it would be an interesting challenge to get
everything set up in a repeatable manner. One good reference is [this blog
post](http://www.steveklabnik.com/automatically_update_github_pages_with_travis_example/)
by Steve Klabnik.

The versions of `pandoc` and `LaTeX` on Ubuntu 12.04 (which Travis-CI uses for
its workers) are hopelessly out of date, so my first problem was getting the
newest version of `pandoc` installed. I used `stack` to download and install
this.

I already had a `Makefile`, so converting my source to Github-flavoured
Markdown and HTML was as easy as `make readme` and `make html`:

```makefile
all:: pdf html readme

travis:: html readme

pdf:: resume.md templates/header.tex
	pandoc resume.md -H templates/header.tex -o Vaibhav_Sagar_resume.pdf

html:: resume.md templates/header.css
	pandoc resume.md -s -H templates/header.css -o index.html

readme:: resume.md
	pandoc resume.md -t markdown_github -o readme.md

clean::
	rm Vaibhav_Sagar_resume.pdf
	rm readme.md
	rm index.html
```

I decided it would be best if the build process updated my `gh-pages` branch
instead of uploading to a different repository:

```bash
#!/usr/bin/env bash
BRANCH=gh-pages
TARGET_REPO=vaibhavsagar/resume.git

if [ "$TRAVIS_PULL_REQUEST" == "false" ]; then
    echo -e "Starting to deploy to Github Pages\n"
    if [ "$TRAVIS" == "true" ]; then
        git config --global user.email "travis@travis-ci.org"
        git config --global user.name "Travis"
    fi
    # using token clone gh-pages branch
    git clone --quiet --branch=$BRANCH https://${GH_TOKEN}@github.com/$TARGET_REPO build > /dev/null
    # go into directory and copy data we're interested in to that directory
    cd build
    cp ../readme.md ../index.html .
    # add, commit and push files
    git add -f .
    git commit -m "Travis build $TRAVIS_BUILD_NUMBER pushed to Github Pages"
    git push -fq origin $BRANCH > /dev/null
    echo -e "Deploy completed\n"
fi
```

The next challenge was actually generating my PDF and uploading it to Github
Releases. Travis CI has excellent support for deploying binaries to Releases
but using the `travis` gem to configure support makes a completely new Github
token which I thought was a bit redundant. The documentation for `travis
encrypt` is pretty comprehensive and I just encrypted my old token and used
that instead. My completed `.travis.yml` is as follows:

```yaml
language: c
sudo: true

cache:
  directories:
  - $HOME/.stack
  - /usr/local/texlive/2016/bin/x86_64-linux
addons:
  apt:
    packages:
    - libgmp-dev
    - xzdec
before_install:
- mkdir -p $HOME/.local/bin
- travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

install:
- stack setup
- stack install pandoc
script: make travis
notifications:
  email:
    on_success: always
    on_failure: always
after_success: bash .ci/update_pages.sh
before_deploy: bash .ci/build_pdf.sh
deploy:
  provider: releases
  file: Vaibhav_Sagar_resume.pdf
  on:
    repo: vaibhavsagar/resume
    all_branches: true
    tags: true
  api_key:
    secure: gtBPUeDXL9S6h4aWyXCEhWxbtkATx2lIBmkVTcWDHMvgwQHmfo42OPq7rQWjO6g/iOlv71Q1VQMQc84ERcZjtBRSE0pb1s1Baqs2Hk7ec/JeWsEXDZmBIs/Z3V6pHb14zCs5GNYyerXDpQ97P4RG9Vjdy+rc3I1+kkuCMF7zB3k=
env:
  global:
    secure: Bzof/7yN7HgV2eJk7FNliNx/cagIU4I113SwNJhChFyYOSy816oPrwQaCMyZuwvbcEIfLMY0K0qxtQK1MoPq7zTYiCTW3UPB2+mzTfTHPMTm5nWjZv0BmdqVoG8IJwxfo5cIV8hfKiu2ezNKcDuqgwb80mYwpTwlQPsY9gOm1Tc=
```

If you're trying something similar and you're getting stuck, feel free to reach
out and I'll be more than happy to help.
