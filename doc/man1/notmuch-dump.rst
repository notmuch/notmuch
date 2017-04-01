============
notmuch-dump
============

SYNOPSIS
========

**notmuch** **dump** [--gzip] [--format=(batch-tag|sup)] [--output=<*file*>] [--] [<*search-term*> ...]

DESCRIPTION
===========

Dump tags for messages matching the given search terms.

Output is to the given filename, if any, or to stdout.

These tags are the only data in the notmuch database that can't be
recreated from the messages themselves. The output of notmuch dump is
therefore the only critical thing to backup (and much more friendly to
incremental backup than the native database files.)

See **notmuch-search-terms(7)** for details of the supported syntax
for <search-terms>. With no search terms, a dump of all messages in
the database will be generated. A "--" argument instructs notmuch that
the remaining arguments are search terms.

Supported options for **dump** include

    ``--gzip``
        Compress the output in a format compatible with **gzip(1)**.

    ``--format=(sup|batch-tag)``
        Notmuch restore supports two plain text dump formats, both with one
        message-id per line, followed by a list of tags.

        **batch-tag**

            The default **batch-tag** dump format is intended to more
            robust against malformed message-ids and tags containing
            whitespace or non-\ **ascii(7)** characters. Each line has
            the form

                +<*encoded-tag*\ > +<*encoded-tag*\ > ... --
                id:<*quoted-message-id*\ >

            Tags are hex-encoded by replacing every byte not matching
            the regex **[A-Za-z0-9@=.,\_+-]** with **%nn** where nn is
            the two digit hex encoding. The message ID is a valid
            Xapian query, quoted using Xapian boolean term quoting
            rules: if the ID contains whitespace or a close paren or
            starts with a double quote, it must be enclosed in double
            quotes and double quotes inside the ID must be
            doubled. The astute reader will notice this is a special
            case of the batch input format for **notmuch-tag(1)**;
            note that the single message-id query is mandatory for
            **notmuch-restore(1)**.

        **sup**

            The **sup** dump file format is specifically chosen to be
            compatible with the format of files produced by
            sup-dump. So if you've previously been using sup for mail,
            then the **notmuch restore** command provides you a way to
            import all of your tags (or labels as sup calls
            them). Each line has the following form

                <*message-id*\ > **(** <*tag*\ > ... **)**

            with zero or more tags are separated by spaces. Note that
            (malformed) message-ids may contain arbitrary non-null
            characters. Note also that tags with spaces will not be
            correctly restored with this format.

    ``--include=(config|properties|tags)``

    Control what kind of metadata is included in the output.

      **config**

        Output configuration data stored in the database. Each line
        starts with "#@ ", followed by a space separated key-value
        pair.  Both key and value are hex encoded if needed.

      **properties**

        Output per-message (key,value) metadata.  Each line starts
        with "#= ", followed by a message id, and a space separated
        list of key=value pairs.  pair.  Ids, keys and values are hex
        encoded if needed.

      **tags**

        Output per-message boolean metadata, namely tags. See *format* above
        for description of the output.

      The default is to include all available types of data.  The
      option can be specified multiple times to select some subset. As
      of version 3 of the dump format, there is a header line of the
      following form

      |
      |  #notmuch-dump <*format*>:<*version*> <*included*>

      where <*included*> is a comma separated list of the above
      options.

    ``--output=``\ <filename>
        Write output to given file instead of stdout.

SEE ALSO
========

**notmuch(1)**, **notmuch-config(1)**, **notmuch-count(1)**,
**notmuch-hooks(5)**, **notmuch-insert(1)**, **notmuch-new(1)**,
**notmuch-reply(1)**, **notmuch-restore(1)**, **notmuch-search(1)**,
**notmuch-search-terms(7)**, **notmuch-show(1)**, **notmuch-tag(1)**
