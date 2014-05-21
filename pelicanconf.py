#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = 'Vaibhav Sagar'
SITENAME = "Vaibhav Sagar's Blog"
SITEURL = ''

TIMEZONE = 'Australia/Sydney'

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
#RELATIVE_URLS = True
