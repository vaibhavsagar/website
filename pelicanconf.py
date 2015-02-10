#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

THEME = 'notmyidea'

TIMEZONE = 'Australia/Sydney'

AUTHOR = 'Vaibhav Sagar'
SITENAME = "Vaibhav Sagar's Blog"
SITEURL = ''
OUTPUT_PATH = 'output/blog'

ARTICLE_PATHS = ['blog']
ARTICLE_URL = 'blog/{date:%Y}/{date:%m}/{date:%d}/{slug}/'
ARTICLE_SAVE_AS = 'blog/{date:%Y}/{date:%m}/{date:%d}/{slug}/index.html'

PAGE_PATHS = ['pages']
PAGE_URL = '{slug}/'
PAGE_SAVE_AS = '{slug}/index.html'

CATEGORY_SAVE_AS = 'blog/{slug}/index.html'
CATEGORY_URL = 'blog/{slug}/'

ARCHIVES_SAVE_AS = 'blog/archives/index.html'

INDEX_SAVE_AS = 'blog/index.html'

DEFAULT_LANG = 'en'

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None

# Blogroll
LINKS = (('Pelican', 'http://getpelican.com/'),
         ('Python.org', 'http://python.org/'),
         ('Jinja2', 'http://jinja.pocoo.org/'))

# Social widget
SOCIAL = ()

DEFAULT_PAGINATION = 10

STATIC_PATHS = ['images', 'extra/CNAME', 'extra/LICENSE', 'extra/README.md']
EXTRA_PATH_METADATA = {
    'extra/CNAME':     {'path': 'CNAME'},
    'extra/LICENSE':   {'path': 'LICENSE'},
    'extra/README.md': {'path': 'README.md'}
}
# Uncomment following line if you want document-relative URLs when developing
# RELATIVE_URLS = True
