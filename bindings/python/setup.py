#!/usr/bin/env python

"""
This file is part of notmuch.

Notmuch is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

Notmuch is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with notmuch.  If not, see <https://www.gnu.org/licenses/>.

Copyright 2010 Sebastian Spaeth <Sebastian@SSpaeth.de>
"""

import os
from distutils.core import setup

# get the notmuch version number without importing the notmuch module
version_file = os.path.join(os.path.dirname(__file__),
                            'notmuch', 'version.py')
exec(compile(open(version_file).read(), version_file, 'exec'))
assert '__VERSION__' in globals(), \
    'Failed to read the notmuch binding version number'

setup(name='notmuch',
      version=__VERSION__,
      description='Python binding of the notmuch mail search and indexing library.',
      author='Sebastian Spaeth',
      author_email='Sebastian@SSpaeth.de',
      url='https://notmuchmail.org/',
      download_url='https://notmuchmail.org/releases/notmuch-%s.tar.gz' % __VERSION__,
      packages=['notmuch'],
      keywords=['library', 'email'],
      long_description='''Overview
========

The notmuch module provides an interface to the `notmuch
<https://notmuchmail.org>`_ functionality, directly interfacing with a
shared notmuch library. Notmuch provides a maildatabase that allows
for extremely quick searching and filtering of your email according to
various criteria.

The documentation for the latest notmuch release can be `viewed
online <https://notmuch.readthedocs.io/>`_.

Requirements
------------

You need to have notmuch installed (or rather libnotmuch.so.1). Also,
notmuch makes use of the ctypes library, and has only been tested with
python >= 2.5. It will not work on earlier python versions.
''',
      classifiers=['Development Status :: 3 - Alpha',
                   'Intended Audience :: Developers',
                   'License :: OSI Approved :: GNU General Public License (GPL)',
                   'Programming Language :: Python :: 2',
                   'Programming Language :: Python :: 3',
                   'Topic :: Communications :: Email',
                   'Topic :: Software Development :: Libraries'
                   ],
      platforms='',
      license='https://www.gnu.org/licenses/gpl-3.0.txt',
     )
