'''
This file is part of notmuch.

This module handles differences between python2.x and python3.x and
allows the notmuch bindings to support both version families with one
source tree.

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
Copyright 2012 Justus Winter <4winter@informatik.uni-hamburg.de>
'''

import sys

if sys.version_info[0] == 2:
    from ConfigParser import SafeConfigParser

    class Python3StringMixIn(object):
        def __str__(self):
            return unicode(self).encode('utf-8')

    def encode_utf8(value):
        '''
        Ensure a nicely utf-8 encoded string to pass to wrapped
        libnotmuch functions.

        C++ code expects strings to be well formatted and unicode
        strings to have no null bytes.
        '''
        if not isinstance(value, basestring):
            raise TypeError('Expected str or unicode, got %s' % type(value))

        if isinstance(value, unicode):
            return value.encode('utf-8', 'replace')

        return value
else:
    from configparser import SafeConfigParser

    class Python3StringMixIn(object):
        def __str__(self):
            return self.__unicode__()

    def encode_utf8(value):
        '''
        Ensure a nicely utf-8 encoded string to pass to wrapped
        libnotmuch functions.

        C++ code expects strings to be well formatted and unicode
        strings to have no null bytes.
        '''
        if not isinstance(value, str):
            raise TypeError('Expected str, got %s' % type(value))

        return value.encode('utf-8', 'replace')

# We import the SafeConfigParser class on behalf of other code to cope
# with the differences between Python 2 and 3.
SafeConfigParser # avoid warning about unused import
