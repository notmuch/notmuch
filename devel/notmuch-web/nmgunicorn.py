#!/usr/bin/env python3

# to launch nmweb from gunicorn.

from nmweb import urls, index, search, show
import web

app = web.application(urls, globals())

# get the wsgi app from web.py application object
wsgiapp = app.wsgifunc()
