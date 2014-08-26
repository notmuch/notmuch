===============
notmuch-compact
===============

SYNOPSIS
========

**notmuch** **compact** [--quiet] [--backup=<*directory*>]

DESCRIPTION
===========

The **compact** command can be used to compact the notmuch database.
This can both reduce the space required by the database and improve
lookup performance.

The compacted database is built in a temporary directory and is later
moved into the place of the origin database. The original uncompacted
database is discarded, unless the ``--backup=``\ <directory> option is
used.

Note that the database write lock will be held during the compaction
process (which may be quite long) to protect data integrity.

Supported options for **compact** include

    ``--backup=``\ <directory>
        Save the current database to the given directory before
        replacing it with the compacted database. The backup directory
        must not exist and it must reside on the same mounted filesystem
        as the current database.

    ``--quiet``
        Do not report database compaction progress to stdout.

ENVIRONMENT
===========

The following environment variables can be used to control the behavior
of notmuch.

**NOTMUCH\_CONFIG**
    Specifies the location of the notmuch configuration file. Notmuch
    will use ${HOME}/.notmuch-config if this variable is not set.

SEE ALSO
========

**notmuch(1)**, **notmuch-count(1)**, **notmuch-dump(1)**,
**notmuch-hooks(5)**, **notmuch-insert(1)**, **notmuch-new(1)**,
**notmuch-reply(1)**, **notmuch-restore(1)**, **notmuch-search(1)**,
**notmuch-search-terms(7)**, **notmuch-show(1)**, **notmuch-tag(1)**
