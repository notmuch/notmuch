.. _notmuch-search(1):

==============
notmuch-search
==============

SYNOPSIS
========

**notmuch** **search** [*option* ...] <*search-term*> ...

DESCRIPTION
===========

Search for messages matching the given search terms, and display as
results the threads containing the matched messages.

The output consists of one line per thread, giving a thread ID, the date
of the newest (or oldest, depending on the sort option) matched message
in the thread, the number of matched messages and total messages in the
thread, the names of all participants in the thread, and the subject of
the newest (or oldest) message.

See :any:`notmuch-search-terms(7)` for details of the supported syntax for
<search-terms>.

Supported options for **search** include

.. program:: search

.. option:: --format=(json|sexp|text|text0)

   Presents the results in either JSON, S-Expressions, newline
   character separated plain-text (default), or null character
   separated plain-text (compatible with :manpage:`xargs(1)` -0
   option where available).

.. option:: --format-version=N

   Use the specified structured output format version. This is
   intended for programs that invoke :any:`notmuch(1)` internally. If
   omitted, the latest supported version will be used.

.. option:: --output=(summary|threads|messages|files|tags)

   **summary**
     Output a summary of each thread with any message matching the
     search terms. The summary includes the thread ID, date, the
     number of messages in the thread (both the number matched and
     the total number), the authors of the thread and the
     subject. In the case where a thread contains multiple files
     for some messages, the total number of files is printed in
     parentheses (see below for an example).

   **threads**
     Output the thread IDs of all threads with any message matching
     the search terms, either one per line (``--format=text``),
     separated by null characters (``--format=text0``), as a JSON array
     (``--format=json``), or an S-Expression list (``--format=sexp``).

   **messages**
     Output the message IDs of all messages matching the search
     terms, either one per line (``--format=text``), separated by null
     characters (``--format=text0``), as a JSON array (``--format=json``),
     or as an S-Expression list (``--format=sexp``).

   **files**
     Output the filenames of all messages matching the search
     terms, either one per line (``--format=text``), separated by null
     characters (``--format=text0``), as a JSON array (``--format=json``),
     or as an S-Expression list (``--format=sexp``).

     Note that each message may have multiple filenames associated
     with it. All of them are included in the output (unless
     limited with the ``--duplicate=N`` option). This may be
     particularly confusing for **folder:** or **path:** searches
     in a specified directory, as the messages may have duplicates
     in other directories that are included in the output, although
     these files alone would not match the search.

   **tags**
     Output all tags that appear on any message matching the search
     terms, either one per line (``--format=text``), separated by null
     characters (``--format=text0``), as a JSON array (``--format=json``),
     or as an S-Expression list (``--format=sexp``).

.. option:: --sort=(newest-first|oldest-first)

   This option can be used to present results in either chronological
   order (**oldest-first**) or reverse chronological order
   (**newest-first**).

   Note: The thread order will be distinct between these two options
   (beyond being simply reversed). When sorting by **oldest-first**
   the threads will be sorted by the oldest message in each thread,
   but when sorting by **newest-first** the threads will be sorted by
   the newest message in each thread.

   By default, results will be displayed in reverse chronological
   order, (that is, the newest results will be displayed first).

.. option:: --offset=[-]N

   Skip displaying the first N results. With the leading '-', start
   at the Nth result from the end.

.. option:: --limit=N

   Limit the number of displayed results to N.

.. option:: --exclude=(true|false|all|flag)

   A message is called "excluded" if it matches at least one tag in
   search.exclude\_tags that does not appear explicitly in the search
   terms. This option specifies whether to omit excluded messages in
   the search process.

   **true** (default)
     Prevent excluded messages from matching the search terms.

   **all**
     Additionally prevent excluded messages from appearing in
     displayed results, in effect behaving as though the excluded
     messages do not exist.

   **false**
     Allow excluded messages to match search terms and appear in
     displayed results. Excluded messages are still marked in the
     relevant outputs.

   **flag**
     Only has an effect when ``--output=summary``. The output is
     almost identical to **false**, but the "match count" is the
     number of matching non-excluded messages in the thread, rather
     than the number of matching messages.

.. option:: --duplicate=N

   For ``--output=files``, output the Nth filename associated with
   each message matching the query (N is 1-based). If N is greater
   than the number of files associated with the message, don't print
   anything.

   For ``--output=messages``, only output message IDs of messages
   matching the search terms that have at least N filenames
   associated with them.

   Note that this option is orthogonal with the **folder:** search
   prefix. The prefix matches messages based on filenames. This
   option filters filenames of the matching messages.

EXAMPLE
=======

The following shows an example of the summary output format, with one
message having multiple filenames.

::

  % notmuch search date:today.. and tag:bad-news
  thread:0000000000063c10 Today [1/1] Some Persun; To the bone (bad-news inbox unread)
  thread:0000000000063c25 Today [1/1(2)] Ann Other; Bears (bad-news inbox unread)
  thread:0000000000063c00 Today [1/1] A Thurd; Bites, stings, sad feelings (bad-news unread)

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
:any:`notmuch-address(1)`
:any:`notmuch-config(1)`,
:any:`notmuch-count(1)`,
:any:`notmuch-dump(1)`,
:any:`notmuch-hooks(5)`,
:any:`notmuch-insert(1)`,
:any:`notmuch-new(1)`,
:any:`notmuch-reply(1)`,
:any:`notmuch-restore(1)`,
:any:`notmuch-search-terms(7)`,
:any:`notmuch-show(1)`,
:any:`notmuch-tag(1)`
