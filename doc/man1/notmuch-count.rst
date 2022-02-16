.. _notmuch-count(1):

=============
notmuch-count
=============

SYNOPSIS
========

**notmuch** **count** [*option* ...] <*search-term*> ...

DESCRIPTION
===========

Count messages matching the search terms.

The number of matching messages (or threads) is output to stdout.

With no search terms, a count of all messages (or threads) in the
database will be displayed.

See :any:`notmuch-search-terms(7)` for details of the supported syntax for
<search-terms>.

Supported options for **count** include

.. program:: count

.. option:: --output=(messages|threads|files)

   messages
     Output the number of matching messages. This is the default.

   threads
     Output the number of matching threads.

   files
     Output the number of files associated with matching
     messages. This may be bigger than the number of matching
     messages due to duplicates (i.e. multiple files having the
     same message-id).

.. option:: --exclude=(true|false)

   Specify whether to omit messages matching search.exclude\_tags from
   the count (the default) or not.

.. option:: --batch

   Read queries from a file (stdin by default), one per line, and
   output the number of matching messages (or threads) to stdout, one
   per line. On an empty input line the count of all messages (or
   threads) in the database will be output. This option is not
   compatible with specifying search terms on the command line.

.. option:: --lastmod

   Append lastmod (counter for number of database updates) and UUID
   to the output. lastmod values are only comparable between
   databases with the same UUID.

.. option:: --input=<filename>

   Read input from given file, instead of from stdin. Implies
   ``--batch``.

SEE ALSO
========

:any:`notmuch(1)`,
:any:`notmuch-config(1)`,
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
