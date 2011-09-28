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

from ctypes import CDLL, c_char_p, c_int
from ctypes.util import find_library

#-----------------------------------------------------------------------------
#package-global instance of the notmuch library
try:
    nmlib = CDLL("libnotmuch.so.1")
except:
    raise ImportError("Could not find shared 'notmuch' library.")


class Enum(object):
    """Provides ENUMS as "code=Enum(['a','b','c'])" where code.a=0 etc..."""
    def __init__(self, names):
        for number, name in enumerate(names):
            setattr(self, name, number)


class Status(Enum):
    """Enum with a string representation of a notmuch_status_t value."""
    _status2str = nmlib.notmuch_status_to_string
    _status2str.restype = c_char_p
    _status2str.argtypes = [c_int]

    def __init__(self, statuslist):
        """It is initialized with a list of strings that are available as
        Status().string1 - Status().stringn attributes.
        """
        super(Status, self).__init__(statuslist)

    @classmethod
    def status2str(self, status):
        """Get a string representation of a notmuch_status_t value."""
        # define strings for custom error messages
        if status == STATUS.NOT_INITIALIZED:
            return "Operation on uninitialized object impossible."
        return str(Status._status2str(status))

STATUS = Status(['SUCCESS',
  'OUT_OF_MEMORY',
  'READ_ONLY_DATABASE',
  'XAPIAN_EXCEPTION',
  'FILE_ERROR',
  'FILE_NOT_EMAIL',
  'DUPLICATE_MESSAGE_ID',
  'NULL_POINTER',
  'TAG_TOO_LONG',
  'UNBALANCED_FREEZE_THAW',
  'UNBALANCED_ATOMIC',
  'NOT_INITIALIZED'])
"""STATUS is a class, whose attributes provide constants that serve as return
indicators for notmuch functions. Currently the following ones are defined. For
possible return values and specific meaning for each method, see the method
description.

  * SUCCESS
  * OUT_OF_MEMORY
  * READ_ONLY_DATABASE
  * XAPIAN_EXCEPTION
  * FILE_ERROR
  * FILE_NOT_EMAIL
  * DUPLICATE_MESSAGE_ID
  * NULL_POINTER
  * TAG_TOO_LONG
  * UNBALANCED_FREEZE_THAW
  * UNBALANCED_ATOMIC
  * NOT_INITIALIZED

Invoke the class method `notmuch.STATUS.status2str` with a status value as
argument to receive a human readable string"""
STATUS.__name__ = 'STATUS'


class NotmuchError(Exception):
    def __init__(self, status=None, message=None):
        """Is initiated with a (notmuch.STATUS[,message=None])"""
        super(NotmuchError, self).__init__(message, status)

    def __str__(self):
        if self.args[0] is not None:
            return self.args[0]
        else:
            return STATUS.status2str(self.args[1])

def _str(value):
    """Ensure a nicely utf-8 encoded string to pass to libnotmuch

    C++ code expects strings to be well formatted and
    unicode strings to have no null bytes."""
    if not isinstance(value, basestring):
        raise TypeError("Expected str or unicode, got %s" % str(type(value)))
    if isinstance(value, unicode):
        return value.encode('UTF-8')
    return value

