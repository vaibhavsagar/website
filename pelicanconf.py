#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

THEME = 'notmyidea'

TIMEZONE = 'Australia/Sydney'

AUTHOR = 'Vaibhav Sagar'
SITENAME = "Vaibhav Sagar's Site"
SITEURL = 'http://www.vaibhavsagar.com'
OUTPUT_PATH = 'output/blog'
GITHUB_URL = 'https://github.com/vaibhavsagar'
DISQUS_SITENAME = 'vaibhavsagar'

ARTICLE_PATHS = ['blog']
ARTICLE_URL = 'blog/{date:%Y}/{date:%m}/{date:%d}/{slug}/'
ARTICLE_SAVE_AS = 'blog/{date:%Y}/{date:%m}/{date:%d}/{slug}/index.html'
SLUGIFY_SOURCE = 'basename'

PAGE_PATHS = ['pages']
PAGE_URL = '{slug}/'
PAGE_SAVE_AS = '{slug}/index.html'

CATEGORY_SAVE_AS = 'blog/{slug}/index.html'
CATEGORY_URL = 'blog/{slug}/'

ARCHIVES_SAVE_AS = 'blog/archives/index.html'

INDEX_SAVE_AS = 'blog/index.html'

MENUITEMS = [
    ('all', '/blog/archives/'),
]

DEFAULT_LANG = 'en'

# Feed generation is usually not desired when developing
FEED_DOMAIN = SITEURL
FEED_ATOM = 'atom.xml'
FEED_ALL_ATOM = 'feeds/all.atom.xml'
CATEGORY_FEED_ATOM = 'feeds/%s.atom.xml'
TRANSLATION_FEED_ATOM = None

# Social widget
SOCIAL = ()

DEFAULT_PAGINATION = 10

STATIC_PATHS = ['images', 'extra']
EXTRA_PATH_METADATA = {
    'extra/CNAME': {'path': 'CNAME'},
    'extra/LICENSE': {'path': 'LICENSE'},
    'extra/README.md': {'path': 'README.md'},
    'extra/google44295b6adcb10ecd.html': {
        'path': 'google44295b6adcb10ecd.html'
    }
}

DELETE_OUTPUT_DIRECTORY = True

# Uncomment following line if you want document-relative URLs when developing
# RELATIVE_URLS = True

MD_EXTENSIONS = ['fenced_code', 'codehilite(css_class=highlight)', 'extra']
GOOGLE_ANALYTICS = "UA-79891461-1"
