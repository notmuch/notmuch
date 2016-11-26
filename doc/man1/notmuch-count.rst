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

See **notmuch-search-terms(7)** for details of the supported syntax for
<search-terms>.

Supported options for **count** include

    ``--output=(messages|threads|files)``

        **messages**
            Output the number of matching messages. This is the default.

        **threads**
            Output the number of matching threads.

        **files**
            Output the number of files associated with matching
            messages. This may be bigger than the number of matching
            messages due to duplicates (i.e. multiple files having the
            same message-id).

    ``--exclude=(true|false)``
        Specify whether to omit messages matching search.tag\_exclude
        from the count (the default) or not.

    ``--batch``
        Read queries from a file (stdin by default), one per line, and
        output the number of matching messages (or threads) to stdout,
        one per line. On an empty input line the count of all messages
        (or threads) in the database will be output. This option is not
        compatible with specifying search terms on the command line.

    ``--lastmod``
        Append lastmod (counter for number of database updates) and UUID
        to the output. lastmod values are only comparable between databases
        with the same UUID.

    ``--input=``\ <filename>
        Read input from given file, instead of from stdin. Implies
        ``--batch``.

SEE ALSO
========

**notmuch(1)**, **notmuch-config(1)**, **notmuch-dump(1)**,
**notmuch-hooks(5)**, **notmuch-insert(1)**, **notmuch-new(1)**,
**notmuch-reply(1)**, **notmuch-restore(1)**, **notmuch-search(1)**,
**notmuch-search-terms(7)**, **notmuch-show(1)**, **notmuch-tag(1)**
