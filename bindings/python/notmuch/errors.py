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

from ctypes import c_char_p, c_int

from .globals import (
    nmlib,
    Enum,
    Python3StringMixIn,
)

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
        """Get a (unicode) string representation of a notmuch_status_t value."""
        # define strings for custom error messages
        if status == STATUS.NOT_INITIALIZED:
            return "Operation on uninitialized object impossible."
        return unicode(Status._status2str(status))

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
  'UNSUPPORTED_OPERATION',
  'UPGRADE_REQUIRED',
  'PATH_ERROR',
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
  * UNSUPPORTED_OPERATION
  * UPGRADE_REQUIRED
  * PATH_ERROR
  * NOT_INITIALIZED

Invoke the class method `notmuch.STATUS.status2str` with a status value as
argument to receive a human readable string"""
STATUS.__name__ = 'STATUS'


class NotmuchError(Exception, Python3StringMixIn):
    """Is initiated with a (notmuch.STATUS[, message=None]). It will not
    return an instance of the class NotmuchError, but a derived instance
    of a more specific Error Message, e.g. OutOfMemoryError. Each status
    but SUCCESS has a corresponding subclassed Exception."""

    @classmethod
    def get_exc_subclass(cls, status):
        """Returns a fine grained Exception() type,
        detailing the error status"""
        subclasses = {
            STATUS.OUT_OF_MEMORY: OutOfMemoryError,
            STATUS.READ_ONLY_DATABASE: ReadOnlyDatabaseError,
            STATUS.XAPIAN_EXCEPTION: XapianError,
            STATUS.FILE_ERROR: FileError,
            STATUS.FILE_NOT_EMAIL: FileNotEmailError,
            STATUS.DUPLICATE_MESSAGE_ID: DuplicateMessageIdError,
            STATUS.NULL_POINTER: NullPointerError,
            STATUS.TAG_TOO_LONG: TagTooLongError,
            STATUS.UNBALANCED_FREEZE_THAW: UnbalancedFreezeThawError,
            STATUS.UNBALANCED_ATOMIC: UnbalancedAtomicError,
            STATUS.UNSUPPORTED_OPERATION: UnsupportedOperationError,
            STATUS.UPGRADE_REQUIRED: UpgradeRequiredError,
            STATUS.PATH_ERROR: PathError,
            STATUS.NOT_INITIALIZED: NotInitializedError,
        }
        assert 0 < status <= len(subclasses)
        return subclasses[status]

    def __new__(cls, *args, **kwargs):
        """Return a correct subclass of NotmuchError if needed

        We return a NotmuchError instance if status is None (or 0) and a
        subclass that inherits from NotmuchError depending on the
        'status' parameter otherwise."""
        # get 'status'. Passed in as arg or kwarg?
        status = args[0] if len(args) else kwargs.get('status', None)
        # no 'status' or cls is subclass already, return 'cls' instance
        if not status or cls != NotmuchError:
            return super(NotmuchError, cls).__new__(cls)
        subclass = cls.get_exc_subclass(status)  # which class to use?
        return subclass.__new__(subclass, *args, **kwargs)

    def __init__(self, status=None, message=None):
        self.status = status
        self.message = message

    def __unicode__(self):
        if self.message is not None:
            return self.message
        elif self.status is not None:
            return STATUS.status2str(self.status)
        else:
            return 'Unknown error'


# List of Subclassed exceptions that correspond to STATUS values and are
# subclasses of NotmuchError.
class OutOfMemoryError(NotmuchError):
    status = STATUS.OUT_OF_MEMORY


class ReadOnlyDatabaseError(NotmuchError):
    status = STATUS.READ_ONLY_DATABASE


class XapianError(NotmuchError):
    status = STATUS.XAPIAN_EXCEPTION


class FileError(NotmuchError):
    status = STATUS.FILE_ERROR


class FileNotEmailError(NotmuchError):
    status = STATUS.FILE_NOT_EMAIL


class DuplicateMessageIdError(NotmuchError):
    status = STATUS.DUPLICATE_MESSAGE_ID


class NullPointerError(NotmuchError):
    status = STATUS.NULL_POINTER


class TagTooLongError(NotmuchError):
    status = STATUS.TAG_TOO_LONG


class UnbalancedFreezeThawError(NotmuchError):
    status = STATUS.UNBALANCED_FREEZE_THAW


class UnbalancedAtomicError(NotmuchError):
    status = STATUS.UNBALANCED_ATOMIC


class UnsupportedOperationError(NotmuchError):
    status = STATUS.UNSUPPORTED_OPERATION


class UpgradeRequiredError(NotmuchError):
    status = STATUS.UPGRADE_REQUIRED


class PathError(NotmuchError):
    status = STATUS.PATH_ERROR


class NotInitializedError(NotmuchError):
    """Derived from NotmuchError, this occurs if the underlying data
    structure (e.g. database is not initialized (yet) or an iterator has
    been exhausted. You can test for NotmuchError with .status =
    STATUS.NOT_INITIALIZED"""
    status = STATUS.NOT_INITIALIZED
