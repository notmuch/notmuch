=============
notmuch-hooks
=============

SYNOPSIS
========

$DATABASEDIR/.notmuch/hooks/*

DESCRIPTION
===========

Hooks are scripts (or arbitrary executables or symlinks to such) that
notmuch invokes before and after certain actions. These scripts reside
in the .notmuch/hooks directory within the database directory and must
have executable permissions.

The currently available hooks are described below.

    **pre-new**
        This hook is invoked by the **new** command before scanning or
        importing new messages into the database. If this hook exits
        with a non-zero status, notmuch will abort further processing of
        the **new** command.

        Typically this hook is used for fetching or delivering new mail
        to be imported into the database.

    **post-new**
        This hook is invoked by the **new** command after new messages
        have been imported into the database and initial tags have been
        applied. The hook will not be run if there have been any errors
        during the scan or import.

        Typically this hook is used to perform additional query-based
        tagging on the imported messages.

    **post-insert**

        This hook is invoked by the **insert** command after the
        message has been delivered, added to the database, and initial
        tags have been applied. The hook will not be run if there have
        been any errors during the message delivery; what is regarded
        as successful delivery depends on the ``--keep`` option.

        Typically this hook is used to perform additional query-based
        tagging on the delivered messages.

SEE ALSO
========

**notmuch(1)**, **notmuch-config(1)**, **notmuch-count(1)**,
**notmuch-dump(1)**, **notmuch-insert(1)**, **notmuch-new(1)**,
**notmuch-reply(1)**, **notmuch-restore(1)**, **notmuch-search(1)**,
**notmuch-search-terms(7)**, **notmuch-show(1)**, **notmuch-tag(1)**
