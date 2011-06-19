#!/usr/bin/env python

import os
import re
from distutils.core import setup

def get_version():
    file = open('notmuch/__init__.py')
    try:
        for line in file:
            if re.match('__VERSION__\s*=\s*',line) != None:
                version = line.split('=', 1)[1]
                return eval(version, {}, {})
    finally:
        file.close()
    raise IOError('Unexpected end-of-file')

__VERSION__=get_version()

setup(name='notmuch',
      version=__VERSION__,
      description='Python binding of the notmuch mail search and indexing library.',
      author='Sebastian Spaeth',
      author_email='Sebastian@SSpaeth.de',
      url='http://notmuchmail.org/',
      download_url='http://notmuchmail.org/releases/notmuch-'+__VERSION__+'.tar.gz',
      packages=['notmuch'],
      keywords = ["library", "email"],
      long_description="""Overview
==============

The notmuch  module provides an interface to the `notmuch <http://notmuchmail.org>`_ functionality, directly interfacing with a shared notmuch library. Notmuch provides a maildatabase that allows for extremely quick searching and filtering of your email according to various criteria.

The documentation for the latest cnotmuch release can be `viewed online <http://packages.python.org/notmuch>`_.

The classes notmuch.Database, notmuch.Query  provide most of the core functionality, returning notmuch.Messages and notmuch.Tags.

Installation and Deinstallation
-------------------------------

notmuch is included in the upstream notmuch source repository and it is
packaged on http://pypi.python.org. This means you can do "easy_install
notmuch" (or using pip) on your linux box and it will get installed
into:

/usr/local/lib/python2.x/dist-packages/

For uninstalling, you will need to remove the "notmuch-0.x-py2.x.egg"
directory and delete one entry refering to cnotmuch in the
"easy-install.pth" file in that directory. There should be no trace
left of cnotmuch then.

Requirements
------------

You need to have notmuch installed (or rather libnotmuch.so.1). The
release version 0.3 should work fine. Also, notmuch makes use of the
ctypes library, and has only been tested with python 2.5. It will not
work on earlier python versions.
""",
      classifiers=['Development Status :: 2 - Pre-Alpha',
                   'Intended Audience :: Developers',
                   'License :: OSI Approved :: GNU General Public License (GPL)',
                   'Programming Language :: Python :: 2',
                   'Programming Language :: Python :: 3',
                   'Topic :: Communications :: Email',
                   'Topic :: Software Development :: Libraries'
                   ],
      platforms='',
      license='http://www.gnu.org/licenses/gpl-3.0.txt',
     )
