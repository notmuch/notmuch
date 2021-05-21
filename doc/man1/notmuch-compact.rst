.. _notmuch-compact(1):

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

.. program:: compact

.. option:: --backup=<directory>

   Save the current database to the given directory before replacing
   it with the compacted database. The backup directory must not
   exist and it must reside on the same mounted filesystem as the
   current database.

.. option:: --quiet

   Do not report database compaction progress to stdout.

SEE ALSO
========

:any:`notmuch(1)`,
:any:`notmuch-count(1)`,
:any:`notmuch-dump(1)`,
:any:`notmuch-hooks(5)`,
:any:`notmuch-insert(1)`,
:any:`notmuch-new(1)`,
:any:`notmuch-reply(1)`,
:any:`notmuch-restore(1)`,
:any:`notmuch-search(1)`,
:any:`notmuch-search-terms(7)`,
:any:`notmuch-show(1)`,
:any:`notmuch-tag(1)`
