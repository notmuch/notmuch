===============
notmuch-restore
===============

SYNOPSIS
========

**notmuch** **restore** [--accumulate] [--format=(auto|batch-tag|sup)] [--input=<*filename*>]

DESCRIPTION
===========

Restores the tags from the given file (see **notmuch dump**).

The input is read from the given filename, if any, or from stdin.

Supported options for **restore** include

    ``--accumulate``
        The union of the existing and new tags is applied, instead of
        replacing each message's tags as they are read in from the dump
        file.

    ``--format=(sup|batch-tag|auto)``
        Notmuch restore supports two plain text dump formats, with each
        line specifying a message-id and a set of tags. For details of
        the actual formats, see **notmuch-dump(1)**.

        **sup**
            The **sup** dump file format is specifically chosen to be
            compatible with the format of files produced by sup-dump. So
            if you've previously been using sup for mail, then the
            **notmuch restore** command provides you a way to import all
            of your tags (or labels as sup calls them).

        **batch-tag**
            The **batch-tag** dump format is intended to more robust
            against malformed message-ids and tags containing whitespace
            or non-\ **ascii(7)** characters. See **notmuch-dump(1)**
            for details on this format.

            **notmuch restore** updates the maildir flags according to
            tag changes if the **maildir.synchronize\_flags**
            configuration option is enabled. See **notmuch-config(1)**
            for details.

        **auto**
            This option (the default) tries to guess the format from the
            input. For correctly formed input in either supported
            format, this heuristic, based the fact that batch-tag format
            contains no parentheses, should be accurate.

    ``--include=(config|properties|tags)``

      Control what kind of metadata is restored.

        **config**

          Restore configuration data to the database. Each configuration line starts
          with "#@ ", followed by a space separated key-value pair.
          Both key and value are hex encoded if needed.

        **properties**

          Output per-message (key,value) metadata.  Each line starts
          with "#= ", followed by a message id, and a space separated
          list of key=value pairs.  pair.  Ids, keys and values are
          hex encoded if needed.

        **tags**

          Output per-message metadata, namely tags. See *format* above
          for more details.

      The default is to restore all available types of data.  The
      option can be specified multiple times to select some subset.

    ``--input=``\ <filename>
        Read input from given file instead of stdin.

GZIPPED INPUT
=============

\ **notmuch restore** will detect if the input is compressed in
**gzip(1)** format and automatically decompress it while reading. This
detection does not depend on file naming and in particular works for
standard input.

SEE ALSO
========

**notmuch(1)**, **notmuch-config(1)**, **notmuch-count(1)**,
**notmuch-dump(1)**, **notmuch-hooks(5)**, **notmuch-insert(1)**,
**notmuch-new(1)**, **notmuch-reply(1)**, **notmuch-search(1)**,
**notmuch-search-terms(7)**, **notmuch-show(1)**, **notmuch-tag(1)**
