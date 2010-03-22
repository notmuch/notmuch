#!/usr/bin/env python

from distutils.core import setup
from cnotmuch.notmuch import __VERSION__
setup(name='cnotmuch',
      version=__VERSION__,
      description='Python binding of the notmuch mail search and indexing library.',
      author='Sebastian Spaeth',
      author_email='Sebastian@SSpaeth.de',
      url='http://bitbucket.org/spaetz/cnotmuch/',
      download_url='http://bitbucket.org/spaetz/cnotmuch/get/v'+__VERSION__+'.tar.gz',
      packages=['cnotmuch'],
      keywords = ["library", "email"],
      long_description="""The cnotmuch  module provides an interface to the `notmuch <http://notmuchmail.org>`_ functionality, directly interfacing with a shared notmuch library. Notmuch provides a maildatabase that allows for extremely quick searching and filtering of your email according to various criteria.

The documentation for the latest cnotmuch release can be `viewed online <http://spaetz.bitbucket.org/>`_.

The classes notmuch.Database, notmuch.Query  provide most of the core functionality, returning notmuch.Messages and notmuch.Tags.
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
