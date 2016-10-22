Title: Generate A PDF From Your Resume With Travis-CI
Date: 2015-02-26
Category: programming

After [automating my blog]({filename}blog-setup.md), I decided to aim a little
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

<script
src="http://gist-it.appspot.com/github.com/vaibhavsagar/resume/blob/master/Makefile">
</script>

I decided it would be best if the build process updated my `gh-pages` branch
instead of uploading to a different repository:

<script
src="http://gist-it.appspot.com/github.com/vaibhavsagar/resume/blob/master/.ci/update_pages.sh">
</script>

The next challenge was actually generating my PDF and uploading it to Github
Releases. Travis CI has excellent support for deploying binaries to Releases
but using the `travis` gem to configure support makes a completely new Github
token which I thought was a bit redundant. The documentation for `travis
encrypt` is pretty comprehensive and I just encrypted my old token and used
that instead. My completed `.travis.yml` is as follows:

<script
src="http://gist-it.appspot.com/github.com/vaibhavsagar/resume/blob/master/.travis.yml">
</script>

And if you'd like to see the process I went through to arrive at this solution,
have a look at [the test repo](https://github.com/vaibhavsagar/resumate) I
used. The commit log is a fairly accurate representation of my thought process.
If you're trying something similar and you're getting stuck, feel free to reach
out and I'll be more than happy to help.
