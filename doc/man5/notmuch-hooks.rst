.. _notmuch-hooks(5):

=============
notmuch-hooks
=============

SYNOPSIS
========

<hook_dir>/{pre-new, post-new, post-insert}

DESCRIPTION
===========

Hooks are scripts (or arbitrary executables or symlinks to such) that
notmuch invokes before and after certain actions. These scripts reside
in a directory defined as described in :any:`notmuch-config(1)`. They
must have executable permissions.

The currently available hooks are described below.

**pre-new**
    This hook is invoked by the :any:`notmuch-new(1)` command before
    scanning or importing new messages into the database. If this hook
    exits with a non-zero status, notmuch will abort further
    processing of the :any:`notmuch-new(1)` command.

    Typically this hook is used for fetching or delivering new mail to
    be imported into the database.

**post-new**
    This hook is invoked by the :any:`notmuch-new(1)` command after
    new messages have been imported into the database and initial tags
    have been applied. The hook will not be run if there have been any
    errors during the scan or import.

    Typically this hook is used to perform additional query-based
    tagging on the imported messages.

**post-insert**
    This hook is invoked by the :any:`notmuch-insert(1)` command after
    the message has been delivered, added to the database, and initial
    tags have been applied. The hook will not be run if there have
    been any errors during the message delivery; what is regarded as
    successful delivery depends on the ``--keep`` option.

    Typically this hook is used to perform additional query-based
    tagging on the delivered messages.

SEE ALSO
========

:any:`notmuch(1)`,
:any:`notmuch-config(1)`,
:any:`notmuch-count(1)`,
:any:`notmuch-dump(1)`,
:any:`notmuch-insert(1)`,
:any:`notmuch-new(1)`,
:any:`notmuch-reply(1)`,
:any:`notmuch-restore(1)`,
:any:`notmuch-search(1)`,
:any:`notmuch-search-terms(7)`,
:any:`notmuch-show(1)`,
:any:`notmuch-tag(1)`
