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
along with notmuch.  If not, see <http://www.gnu.org/licenses/>.

Copyright 2010 Sebastian Spaeth <Sebastian@SSpaeth.de>'
"""
import sys
from ctypes import CDLL, Structure, POINTER

#-----------------------------------------------------------------------------
#package-global instance of the notmuch library
try:
    nmlib = CDLL("libnotmuch.so.2")
except:
    raise ImportError("Could not find shared 'notmuch' library.")


if sys.version_info[0] == 2:
    class Python3StringMixIn(object):
        def __str__(self):
            return unicode(self).encode('utf-8')


    def _str(value):
        """Ensure a nicely utf-8 encoded string to pass to libnotmuch

        C++ code expects strings to be well formatted and
        unicode strings to have no null bytes."""
        if not isinstance(value, basestring):
            raise TypeError("Expected str or unicode, got %s" % type(value))
        if isinstance(value, unicode):
            return value.encode('UTF-8')
        return value
else:
    class Python3StringMixIn(object):
        def __str__(self):
            return self.__unicode__()


    def _str(value):
        """Ensure a nicely utf-8 encoded string to pass to libnotmuch

        C++ code expects strings to be well formatted and
        unicode strings to have no null bytes."""
        if not isinstance(value, str):
            raise TypeError("Expected str, got %s" % type(value))
        return value.encode('UTF-8')


class Enum(object):
    """Provides ENUMS as "code=Enum(['a','b','c'])" where code.a=0 etc..."""
    def __init__(self, names):
        for number, name in enumerate(names):
            setattr(self, name, number)


class NotmuchDatabaseS(Structure):
    pass
NotmuchDatabaseP = POINTER(NotmuchDatabaseS)


class NotmuchQueryS(Structure):
    pass
NotmuchQueryP = POINTER(NotmuchQueryS)


class NotmuchThreadsS(Structure):
    pass
NotmuchThreadsP = POINTER(NotmuchThreadsS)


class NotmuchThreadS(Structure):
    pass
NotmuchThreadP = POINTER(NotmuchThreadS)


class NotmuchMessagesS(Structure):
    pass
NotmuchMessagesP = POINTER(NotmuchMessagesS)


class NotmuchMessageS(Structure):
    pass
NotmuchMessageP = POINTER(NotmuchMessageS)


class NotmuchTagsS(Structure):
    pass
NotmuchTagsP = POINTER(NotmuchTagsS)


class NotmuchDirectoryS(Structure):
    pass
NotmuchDirectoryP = POINTER(NotmuchDirectoryS)


class NotmuchFilenamesS(Structure):
    pass
NotmuchFilenamesP = POINTER(NotmuchFilenamesS)
