.. currentmodule:: notmuch

Status and Errors
=================

Some methods return a status, indicating if an operation was successful and what the error was. Most of these status codes are expressed as a specific value, the :class:`notmuch.STATUS`.

:class:`STATUS` -- Notmuch operation return value
--------------------------------------------------

.. autoclass:: notmuch.STATUS
   :inherited-members:

.. automethod:: notmuch.STATUS.status2str

:exc:`NotmuchError` -- A Notmuch execution error
------------------------------------------------
Whenever an error occurs, we throw a special Exception:

.. autoexception:: NotmuchError
   :members:

   This execption inherits directly from :exc:`Exception` and is raised on errors during the notmuch execution.
