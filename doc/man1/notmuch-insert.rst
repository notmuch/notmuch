.. _notmuch-insert(1):

==============
notmuch-insert
==============

SYNOPSIS
========

**notmuch** **insert** [option ...] [+<*tag*>|-<*tag*> ...]

DESCRIPTION
===========

**notmuch insert** reads a message from standard input and delivers it
into the maildir directory given by configuration option
**database.mail_root**, then incorporates the message into the notmuch
database. It is an alternative to using a separate tool to deliver the
message then running :any:`notmuch-new(1)` afterwards.

The new message will be tagged with the tags specified by the
**new.tags** configuration option, then by operations specified on the
command-line: tags prefixed by '+' are added while those prefixed by '-'
are removed.

If the new message is a duplicate of an existing message in the database
(it has same Message-ID), it will be added to the maildir folder and
notmuch database, but the tags will not be changed.

The **insert** command supports hooks. See :any:`notmuch-hooks(5)` for
more details on hooks.

Option arguments must appear before any tag operation arguments.
Supported options for **insert** include

.. program:: insert

.. option:: --folder=<folder>

   Deliver the message to the specified folder, relative to the
   top-level directory given by the value of **database.mail_root**. The
   default is the empty string, which means delivering to the
   top-level directory.

.. option:: --create-folder

   Try to create the folder named by the ``--folder`` option, if it
   does not exist. Otherwise the folder must already exist for mail
   delivery to succeed.

.. option:: --keep

   Keep the message file if indexing fails, and keep the message
   indexed if applying tags or maildir flag synchronization
   fails. Ignore these errors and return exit status 0 to indicate
   successful mail delivery.

.. option:: --no-hooks

   Prevent hooks from being run.

.. option:: --world-readable

   When writing mail to the mailbox, allow it to be read by users
   other than the current user.  Note that this does not override
   umask.  By default, delivered mail is only readable by the current
   user.

.. option:: --decrypt=(true|nostash|auto|false)

   If ``true`` and the message is encrypted, try to decrypt the
   message while indexing, stashing any session keys discovered.  If
   ``auto``, and notmuch already knows about a session key for the
   message, it will try decrypting using that session key but will
   not try to access the user's secret keys.  If decryption is
   successful, index the cleartext itself.  Either way, the message
   is always stored to disk in its original form (ciphertext).

   ``nostash`` is the same as ``true`` except that it will not stash
   newly-discovered session keys in the database.

   Be aware that the index is likely sufficient (and a stashed
   session key is certainly sufficient) to reconstruct the cleartext
   of the message itself, so please ensure that the notmuch message
   index is adequately protected. DO NOT USE ``--decrypt=true`` or
   ``--decrypt=nostash`` without considering the security of your
   index.

   See also ``index.decrypt`` in :any:`notmuch-config(1)`.

EXIT STATUS
===========

This command returns exit status 0 on successful mail delivery,
non-zero otherwise. The default is to indicate failed mail delivery on
any errors, including message file delivery to the filesystem, message
indexing to Notmuch database, changing tags, and synchronizing tags to
maildir flags. The ``--keep`` option may be used to settle for
successful message file delivery.

This command supports the following special exit status code for
errors most likely to be temporary in nature, e.g. failure to get a
database write lock.

``75 (EX_TEMPFAIL)``
    A temporary failure occurred; the user is invited to retry.

The exit status of the **post-insert** hook does not affect the exit
status of the **insert** command.

SEE ALSO
========

:any:`notmuch(1)`,
:any:`notmuch-config(1)`,
:any:`notmuch-count(1)`,
:any:`notmuch-dump(1)`,
:any:`notmuch-hooks(5)`,
:any:`notmuch-reply(1)`,
:any:`notmuch-restore(1)`,
:any:`notmuch-search(1)`,
:any:`notmuch-search-terms(7)`,
:any:`notmuch-show(1)`,
:any:`notmuch-tag(1)`
