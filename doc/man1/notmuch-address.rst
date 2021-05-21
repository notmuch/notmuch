.. _notmuch-address(1):

===============
notmuch-address
===============

SYNOPSIS
========

**notmuch** **address** [*option* ...] <*search-term*> ...

DESCRIPTION
===========

Search for messages matching the given search terms, and display the
addresses from them. Duplicate addresses are filtered out.

See :any:`notmuch-search-terms(7)` for details of the supported syntax for
<search-terms>.

Supported options for **address** include

.. program:: address

.. option:: --format=(json|sexp|text|text0)

   Presents the results in either JSON, S-Expressions, newline
   character separated plain-text (default), or null character
   separated plain-text (compatible with :manpage:`xargs(1)` -0
   option where available).

.. option:: --format-version=N

   Use the specified structured output format version. This is
   intended for programs that invoke :any:`notmuch(1)` internally. If
   omitted, the latest supported version will be used.

.. option:: --output=(sender|recipients|count|address)

   Controls which information appears in the output. This option can
   be given multiple times to combine different outputs.  When
   neither ``--output=sender`` nor ``--output=recipients`` is
   given, ``--output=sender`` is implied.

   **sender**
     Output all addresses from the *From* header.

     Note: Searching for **sender** should be much faster than
     searching for **recipients**, because sender addresses are
     cached directly in the database whereas other addresses need
     to be fetched from message files.

   **recipients**
     Output all addresses from the *To*, *Cc* and *Bcc* headers.

   **count**
     Print the count of how many times was the address encountered
     during search.

     Note: With this option, addresses are printed only after the
     whole search is finished. This may take long time.

   **address**
     Output only the email addresses instead of the full mailboxes
     with names and email addresses. This option has no effect on
     the JSON or S-Expression output formats.

.. option:: --deduplicate=(no|mailbox|address)

   Control the deduplication of results.

   **no**
     Output all occurrences of addresses in the matching
     messages. This is not applicable with ``--output=count``.

   **mailbox**
     Deduplicate addresses based on the full, case sensitive name
     and email address, or mailbox. This is effectively the same as
     piping the ``--deduplicate=no`` output to **sort | uniq**, except
     for the order of results. This is the default.

   **address**
     Deduplicate addresses based on the case insensitive address
     part of the mailbox. Of all the variants (with different name
     or case), print the one occurring most frequently among the
     matching messages. If ``--output=count`` is specified, include all
     variants in the count.

.. option:: --sort=(newest-first|oldest-first)

   This option can be used to present results in either chronological
   order (**oldest-first**) or reverse chronological order
   (**newest-first**).

   By default, results will be displayed in reverse chronological
   order, (that is, the newest results will be displayed first).

   However, if either ``--output=count`` or ``--deduplicate=address`` is
   specified, this option is ignored and the order of the results is
   unspecified.

.. option:: --exclude=(true|false)

   A message is called "excluded" if it matches at least one tag in
   search.exclude\_tags that does not appear explicitly in the search
   terms. This option specifies whether to omit excluded messages in
   the search process.

   The default value, **true**, prevents excluded messages from
   matching the search terms.

   **false** allows excluded messages to match search terms and
   appear in displayed results.

EXIT STATUS
===========

This command supports the following special exit status codes

``20``
    The requested format version is too old.

``21``
    The requested format version is too new.

SEE ALSO
========

:any:`notmuch(1)`,
:any:`notmuch-config(1)`,
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
