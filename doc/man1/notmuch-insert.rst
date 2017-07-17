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
**database.path**, then incorporates the message into the notmuch
database. It is an alternative to using a separate tool to deliver the
message then running **notmuch new** afterwards.

The new message will be tagged with the tags specified by the
**new.tags** configuration option, then by operations specified on the
command-line: tags prefixed by '+' are added while those prefixed by '-'
are removed.

If the new message is a duplicate of an existing message in the database
(it has same Message-ID), it will be added to the maildir folder and
notmuch database, but the tags will not be changed.

The **insert** command supports hooks. See **notmuch-hooks(5)** for
more details on hooks.

Option arguments must appear before any tag operation arguments.
Supported options for **insert** include

    ``--folder=<``\ folder\ **>**
        Deliver the message to the specified folder, relative to the
        top-level directory given by the value of **database.path**. The
        default is to deliver to the top-level directory.

    ``--create-folder``
        Try to create the folder named by the ``--folder`` option, if it
        does not exist. Otherwise the folder must already exist for mail
        delivery to succeed.

    ``--keep``
        Keep the message file if indexing fails, and keep the message
        indexed if applying tags or maildir flag synchronization
        fails. Ignore these errors and return exit status 0 to
        indicate successful mail delivery.

    ``--no-hooks``
        Prevent hooks from being run.

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

**notmuch(1)**, **notmuch-config(1)**, **notmuch-count(1)**,
**notmuch-dump(1)**, **notmuch-hooks(5)**, **notmuch-reply(1)**,
**notmuch-restore(1)**, **notmuch-search(1)**,
**notmuch-search-terms(7)**, **notmuch-show(1)**, **notmuch-tag(1)**
