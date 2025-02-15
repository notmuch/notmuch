.. currentmodule:: notmuch

Status and Errors
=================

Some methods return a status, indicating if an operation was successful and what the error was. Most of these status codes are expressed as a specific value, the :class:`notmuch.STATUS`.

.. note::

    Prior to version 0.12 the exception classes and the enumeration
    :class:`notmuch.STATUS` were defined in `notmuch.globals`. They
    have since then been moved into `notmuch.errors`.

:class:`STATUS` -- Notmuch operation return value
--------------------------------------------------

.. autoclass:: notmuch.STATUS
   :inherited-members:

.. automethod:: notmuch.STATUS.status2str

:exc:`NotmuchError` -- A Notmuch execution error
------------------------------------------------
Whenever an error occurs, we throw a special Exception :exc:`NotmuchError`, or a more fine grained Exception which is derived from it. This means it is always safe to check for NotmuchErrors if you want to catch all errors. If you are interested in more fine grained exceptions, you can use those below.

.. autoexception:: NotmuchError

The following exceptions are all directly derived from NotmuchError. Each of them corresponds to a specific :class:`notmuch.STATUS` value. You can either check the :attr:`status` attribute of a NotmuchError to see if a specific error has occurred, or you can directly check for the following Exception types:

.. autoexception:: OutOfMemoryError(message=None)
   :members:
.. autoexception:: ReadOnlyDatabaseError(message=None)
   :members:
.. autoexception:: XapianError(message=None)
   :members:
.. autoexception:: FileError(message=None)
   :members:
.. autoexception:: FileNotEmailError(message=None)
   :members:
.. autoexception:: DuplicateMessageIdError(message=None)
   :members:
.. autoexception:: NullPointerError(message=None)
   :members:
.. autoexception:: TagTooLongError(message=None)
   :members:
.. autoexception:: UnbalancedFreezeThawError(message=None)
   :members:
.. autoexception:: UnbalancedAtomicError(message=None)
   :members:
.. autoexception:: UnsupportedOperationError(message=None)
   :members:
.. autoexception:: UpgradeRequiredError(message=None)
   :members:
.. autoexception:: PathError(message=None)
   :members:
.. autoexception:: NotInitializedError(message=None)
   :members:
