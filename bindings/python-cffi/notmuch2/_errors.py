from notmuch2 import _capi as capi


class NotmuchError(Exception):
    """Base exception for errors originating from the notmuch library.

    Usually this will have two attributes:

    :status: This is a numeric status code corresponding to the error
       code in the notmuch library.  This is normally fairly
       meaningless, it can also often be ``None``.  This exists mostly
       to easily create new errors from notmuch status codes and
       should not normally be used by users.

    :message: A user-facing message for the error.  This can
       occasionally also be ``None``.  Usually you'll want to call
       ``str()`` on the error object instead to get a sensible
       message.
    """

    @classmethod
    def exc_type(cls, status):
        """Return correct exception type for notmuch status."""
        types = {
            capi.lib.NOTMUCH_STATUS_OUT_OF_MEMORY:
                OutOfMemoryError,
            capi.lib.NOTMUCH_STATUS_READ_ONLY_DATABASE:
                ReadOnlyDatabaseError,
            capi.lib.NOTMUCH_STATUS_XAPIAN_EXCEPTION:
                XapianError,
            capi.lib.NOTMUCH_STATUS_FILE_ERROR:
                FileError,
            capi.lib.NOTMUCH_STATUS_FILE_NOT_EMAIL:
                FileNotEmailError,
            capi.lib.NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID:
                DuplicateMessageIdError,
            capi.lib.NOTMUCH_STATUS_NULL_POINTER:
                NullPointerError,
            capi.lib.NOTMUCH_STATUS_TAG_TOO_LONG:
                TagTooLongError,
            capi.lib.NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW:
                UnbalancedFreezeThawError,
            capi.lib.NOTMUCH_STATUS_UNBALANCED_ATOMIC:
                UnbalancedAtomicError,
            capi.lib.NOTMUCH_STATUS_UNSUPPORTED_OPERATION:
                UnsupportedOperationError,
            capi.lib.NOTMUCH_STATUS_UPGRADE_REQUIRED:
                UpgradeRequiredError,
            capi.lib.NOTMUCH_STATUS_PATH_ERROR:
                PathError,
            capi.lib.NOTMUCH_STATUS_ILLEGAL_ARGUMENT:
                IllegalArgumentError,
            capi.lib.NOTMUCH_STATUS_NO_CONFIG:
                NoConfigError,
            capi.lib.NOTMUCH_STATUS_NO_DATABASE:
                NoDatabaseError,
            capi.lib.NOTMUCH_STATUS_DATABASE_EXISTS:
                DatabaseExistsError,
        }
        return types[status]

    def __new__(cls, *args, **kwargs):
        """Return the correct subclass based on status."""
        # This is simplistic, but the actual __init__ will fail if the
        # signature is wrong anyway.
        if args:
            status = args[0]
        else:
            status = kwargs.get('status', None)
        if status and cls == NotmuchError:
            exc = cls.exc_type(status)
            return exc.__new__(exc, *args, **kwargs)
        else:
            return super().__new__(cls)

    def __init__(self, status=None, message=None):
        self.status = status
        self.message = message

    def __str__(self):
        if self.message:
            return self.message
        elif self.status:
            return capi.lib.notmuch_status_to_string(self.status)
        else:
            return 'Unknown error'


class OutOfMemoryError(NotmuchError): pass
class ReadOnlyDatabaseError(NotmuchError): pass
class XapianError(NotmuchError): pass
class FileError(NotmuchError): pass
class FileNotEmailError(NotmuchError): pass
class DuplicateMessageIdError(NotmuchError): pass
class NullPointerError(NotmuchError): pass
class TagTooLongError(NotmuchError): pass
class UnbalancedFreezeThawError(NotmuchError): pass
class UnbalancedAtomicError(NotmuchError): pass
class UnsupportedOperationError(NotmuchError): pass
class UpgradeRequiredError(NotmuchError): pass
class PathError(NotmuchError): pass
class IllegalArgumentError(NotmuchError): pass
class NoConfigError(NotmuchError): pass
class NoDatabaseError(NotmuchError): pass
class DatabaseExistsError(NotmuchError): pass

class ObjectDestroyedError(NotmuchError):
    """The object has already been destroyed and it's memory freed.

    This occurs when :meth:`destroy` has been called on the object but
    you still happen to have access to the object.  This should not
    normally occur since you should never call :meth:`destroy` by
    hand.
    """

    def __str__(self):
        if self.message:
            return self.message
        else:
            return 'Memory already freed'
