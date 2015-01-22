====================
notmuch-search-terms
====================

SYNOPSIS
========

**notmuch** **count** [option ...] <*search-term*> ...

**notmuch** **dump** [--format=(batch-tag|sup)] [--] [--output=<*file*>] [--] [<*search-term*> ...]

**notmuch** **search** [option ...] <*search-term*> ...

**notmuch** **show** [option ...] <*search-term*> ...

**notmuch** **tag** +<*tag*> ... -<*tag*> [--] <*search-term*> ...

DESCRIPTION
===========

Several notmuch commands accept a common syntax for search terms.

The search terms can consist of free-form text (and quoted phrases)
which will match all messages that contain all of the given
terms/phrases in the body, the subject, or any of the sender or
recipient headers.

As a special case, a search string consisting of exactly a single
asterisk ("\*") will match all messages.

In addition to free text, the following prefixes can be used to force
terms to match against specific portions of an email, (where <brackets>
indicate user-supplied values):

-  from:<name-or-address>

-  to:<name-or-address>

-  subject:<word-or-quoted-phrase>

-  attachment:<word>

-  mimetype:<word>

-  tag:<tag> (or is:<tag>)

-  id:<message-id>

-  thread:<thread-id>

-  folder:<maildir-folder>

-  path:<directory-path> or path:<directory-path>/**

-  date:<since>..<until>

The **from:** prefix is used to match the name or address of the sender
of an email message.

The **to:** prefix is used to match the names or addresses of any
recipient of an email message, (whether To, Cc, or Bcc).

Any term prefixed with **subject:** will match only text from the
subject of an email. Searching for a phrase in the subject is supported
by including quotation marks around the phrase, immediately following
**subject:**.

The **attachment:** prefix can be used to search for specific filenames
(or extensions) of attachments to email messages.

The **mimetype:** prefix will be used to match text from the
content-types of MIME parts within email messages (as specified by the
sender).

For **tag:** and **is:** valid tag values include **inbox** and
**unread** by default for new messages added by **notmuch new** as well
as any other tag values added manually with **notmuch tag**.

For **id:**, message ID values are the literal contents of the
Message-ID: header of email messages, but without the '<', '>'
delimiters.

The **thread:** prefix can be used with the thread ID values that are
generated internally by notmuch (and do not appear in email messages).
These thread ID values can be seen in the first column of output from
**notmuch search**

The **path:** prefix searches for email messages that are in
particular directories within the mail store. The directory must be
specified relative to the top-level maildir (and without the leading
slash). By default, **path:** matches messages in the specified
directory only. The "/\*\*" suffix can be used to match messages in
the specified directory and all its subdirectories recursively.
**path:""** matches messages in the root of the mail store and,
likewise, **path:\*\*** matches all messages.

The **folder:** prefix searches for email messages by maildir or MH
folder. For MH-style folders, this is equivalent to **path:**. For
maildir, this includes messages in the "new" and "cur"
subdirectories. The exact syntax for maildir folders depends on your
mail configuration. For maildir++, **folder:""** matches the inbox
folder (which is the root in maildir++), other folder names always
start with ".", and nested folders are separated by "."s, such as
**folder:.classes.topology**. For "file system" maildir, the inbox is
typically **folder:INBOX** and nested folders are separated by
slashes, such as **folder:classes/topology**.

Both **path:** and **folder:** will find a message if *any* copy of
that message is in the specific directory/folder.

The **date:** prefix can be used to restrict the results to only
messages within a particular time range (based on the Date: header) with
a range syntax of:

date:<since>..<until>

See **DATE AND TIME SEARCH** below for details on the range expression,
and supported syntax for <since> and <until> date and time expressions.

The time range can also be specified using timestamps with a syntax of:

<initial-timestamp>..<final-timestamp>

Each timestamp is a number representing the number of seconds since
1970-01-01 00:00:00 UTC.

In addition to individual terms, multiple terms can be combined with
Boolean operators ( **and**, **or**, **not** , etc.). Each term in the
query will be implicitly connected by a logical AND if no explicit
operator is provided, (except that terms with a common prefix will be
implicitly combined with OR until we get Xapian defect #402 fixed).

Parentheses can also be used to control the combination of the Boolean
operators, but will have to be protected from interpretation by the
shell, (such as by putting quotation marks around any parenthesized
expression).

DATE AND TIME SEARCH
====================

notmuch understands a variety of standard and natural ways of expressing
dates and times, both in absolute terms ("2012-10-24") and in relative
terms ("yesterday"). Any number of relative terms can be combined ("1
hour 25 minutes") and an absolute date/time can be combined with
relative terms to further adjust it. A non-exhaustive description of the
syntax supported for absolute and relative terms is given below.

The range expression
--------------------

date:<since>..<until>

The above expression restricts the results to only messages from <since>
to <until>, based on the Date: header.

<since> and <until> can describe imprecise times, such as "yesterday".
In this case, <since> is taken as the earliest time it could describe
(the beginning of yesterday) and <until> is taken as the latest time it
could describe (the end of yesterday). Similarly, date:january..february
matches from the beginning of January to the end of February.

Currently, we do not support spaces in range expressions. You can
replace the spaces with '\_', or (in most cases) '-', or (in some cases)
leave the spaces out altogether. Examples in this man page use spaces
for clarity.

Open-ended ranges are supported (since Xapian 1.2.1), i.e. it's possible
to specify date:..<until> or date:<since>.. to not limit the start or
end time, respectively. Pre-1.2.1 Xapian does not report an error on
open ended ranges, but it does not work as expected either.

Entering date:expr without ".." (for example date:yesterday) won't work,
as it's not interpreted as a range expression at all. You can achieve
the expected result by duplicating the expr both sides of ".." (for
example date:yesterday..yesterday).

Relative date and time
----------------------

[N\|number]
(years\|months\|weeks\|days\|hours\|hrs\|minutes\|mins\|seconds\|secs)
[...]

All refer to past, can be repeated and will be accumulated.

Units can be abbreviated to any length, with the otherwise ambiguous
single m being m for minutes and M for months.

Number can also be written out one, two, ..., ten, dozen, hundred.
Additionally, the unit may be preceded by "last" or "this" (e.g., "last
week" or "this month").

When combined with absolute date and time, the relative date and time
specification will be relative from the specified absolute date and
time.

Examples: 5M2d, two weeks

Supported absolute time formats
-------------------------------

-  H[H]:MM[:SS] [(am\|a.m.\|pm\|p.m.)]

-  H[H] (am\|a.m.\|pm\|p.m.)

-  HHMMSS

-  now

-  noon

-  midnight

-  Examples: 17:05, 5pm

Supported absolute date formats
-------------------------------

-  YYYY-MM[-DD]

-  DD-MM[-[YY]YY]

-  MM-YYYY

-  M[M]/D[D][/[YY]YY]

-  M[M]/YYYY

-  D[D].M[M][.[YY]YY]

-  D[D][(st\|nd\|rd\|th)] Mon[thname] [YYYY]

-  Mon[thname] D[D][(st\|nd\|rd\|th)] [YYYY]

-  Wee[kday]

Month names can be abbreviated at three or more characters.

Weekday names can be abbreviated at three or more characters.

Examples: 2012-07-31, 31-07-2012, 7/31/2012, August 3

Time zones
----------

-  (+\|-)HH:MM

-  (+\|-)HH[MM]

Some time zone codes, e.g. UTC, EET.

SEE ALSO
========

**notmuch(1)**, **notmuch-config(1)**, **notmuch-count(1)**,
**notmuch-dump(1)**, **notmuch-hooks(5)**, **notmuch-insert(1)**,
**notmuch-new(1)**, **notmuch-reply(1)**, **notmuch-restore(1)**,
**notmuch-search(1)**, **notmuch-show(1)**, **notmuch-tag(1)**
