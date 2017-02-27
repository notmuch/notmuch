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

-  from:/<regex>/

-  to:<name-or-address>

-  subject:<word-or-quoted-phrase>

-  subject:/<regex>/

-  attachment:<word>

-  mimetype:<word>

-  tag:<tag> (or is:<tag>)

-  id:<message-id>

-  thread:<thread-id>

-  folder:<maildir-folder>

-  path:<directory-path> or path:<directory-path>/**

-  date:<since>..<until>

-  lastmod:<initial-revision>..<final-revision>

-  query:<name>

-  property:<key>=<value>

The **from:** prefix is used to match the name or address of the sender
of an email message.

The **to:** prefix is used to match the names or addresses of any
recipient of an email message, (whether To, Cc, or Bcc).

Any term prefixed with **subject:** will match only text from the
subject of an email. Searching for a phrase in the subject is supported
by including quotation marks around the phrase, immediately following
**subject:**.

If notmuch is built with **Xapian Field Processors** (see below) the
**from:** and **subject** prefix can be also used to restrict the
results to those whose from/subject value matches a regular expression
(see **regex(7)**) delimited with //.

::

   notmuch search 'from:/bob@.*[.]example[.]com/'

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

The **lastmod:** prefix can be used to restrict the result by the
database revision number of when messages were last modified (tags
were added/removed or filenames changed).  This is usually used in
conjunction with the **--uuid** argument to **notmuch search**
to find messages that have changed since an earlier query.

The **query:** prefix allows queries to refer to previously saved
queries added with **notmuch-config(1)**. Named queries are only
available if notmuch is built with **Xapian Field Processors** (see
below).

The **property:** prefix searches for messages with a particular
<key>=<value> property pair. Properties are used internally by notmuch
(and extensions) to add metadata to messages. A given key can be
present on a given message with several different values.

Operators
---------

In addition to individual terms, multiple terms can be combined with
Boolean operators (**and**, **or**, **not**, and **xor**). Each term
in the query will be implicitly connected by a logical AND if no
explicit operator is provided (except that terms with a common prefix
will be implicitly combined with OR).  The shorthand '-<term>' can be
used for 'not <term>' but unfortunately this does not work at the
start of an expression.  Parentheses can also be used to control the
combination of the Boolean operators, but will have to be protected
from interpretation by the shell, (such as by putting quotation marks
around any parenthesized expression).

In addition to the standard boolean operators, Xapian provides several
operators specific to text searching.

::

        notmuch search term1 NEAR term2

will return results where term1 is within 10 words of term2. The
threshold can be set like this:

::

        notmuch search term1 NEAR/2 term2

The search

::

        notmuch search term1 ADJ term2

will return results where term1 is within 10 words of term2, but in the
same order as in the query. The threshold can be set the same as with
NEAR:

::

        notmuch search term1 ADJ/7 term2


Stemming
--------

**Stemming** in notmuch means that these searches

::

        notmuch search detailed
        notmuch search details
        notmuch search detail

will all return identical results, because Xapian first "reduces" the
term to the common stem (here 'detail') and then performs the search.

There are two ways to turn this off: a search for a capitalized word
will be performed unstemmed, so that one can search for "John" and not
get results for "Johnson"; phrase searches are also unstemmed (see
below for details).  Stemming is currently only supported for
English. Searches for words in other languages will be performed unstemmed.

Wildcards
---------

It is possible to use a trailing '\*' as a wildcard. A search for
'wildc\*' will match 'wildcard', 'wildcat', etc.


Boolean and Probabilistic Prefixes
----------------------------------

Xapian (and hence notmuch) prefixes are either **boolean**, supporting
exact matches like "tag:inbox" or **probabilistic**, supporting a more
flexible **term** based searching. Certain **special** prefixes are
processed by notmuch in a way not stricly fitting either of Xapian's
built in styles. The prefixes currently supported by notmuch are as
follows.

Boolean
   **tag:**, **id:**, **thread:**, **folder:**, **path:**, **property:**
Probabilistic
  **to:**, **attachment:**, **mimetype:**
Special
   **from:**, **query:**, **subject:**

Terms and phrases
-----------------

In general Xapian distinguishes between lists of terms and
**phrases**. Phrases are indicated by double quotes (but beware you
probably need to protect those from your shell) and insist that those
unstemmed words occur in that order. One useful, but initially
surprising feature is that the following are equivalant ways to write
the same phrase.

- "a list of words"
- a-list-of-words
- a/list/of/words
- a.list.of.words

Both parenthesised lists of terms and quoted phrases are ok with
probabilisitic prefixes such as **to:**, **from:**, and **subject:**. In particular

::

   subject:(pizza free)

is equivalent to

::

   subject:pizza and subject:free

Both of these will match a subject "Free Delicious Pizza" while

::

   subject:"pizza free"

will not.

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

date:<expr>..! can be used as a shorthand for date:<expr>..<expr>. The
expansion takes place before interpretation, and thus, for example,
date:monday..! matches from the beginning of Monday until the end of
Monday.
With **Xapian Field Processor** support (see below), non-range
date queries such as date:yesterday will work, but otherwise
will give unexpected results; if in doubt use date:yesterday..!

Currently, we do not support spaces in range expressions. You can
replace the spaces with '\_', or (in most cases) '-', or (in some cases)
leave the spaces out altogether. Examples in this man page use spaces
for clarity.

Open-ended ranges are supported (since Xapian 1.2.1), i.e. it's possible
to specify date:..<until> or date:<since>.. to not limit the start or
end time, respectively. Pre-1.2.1 Xapian does not report an error on
open ended ranges, but it does not work as expected either.

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

XAPIAN FIELD PROCESSORS
=======================

Certain optional features of the notmuch query processor rely on the
presence of the Xapian field processor API. You can determine if your
notmuch was built against a sufficiently recent version of Xapian by running

::

  % notmuch config get built_with.field_processor

Currently the following features require field processor support:

- non-range date queries, e.g. "date:today"
- named queries e.g. "query:my_special_query"
- regular expression searches, e.g. "subject:/^\\[SPAM\\]/"

SEE ALSO
========

**notmuch(1)**, **notmuch-config(1)**, **notmuch-count(1)**,
**notmuch-dump(1)**, **notmuch-hooks(5)**, **notmuch-insert(1)**,
**notmuch-new(1)**, **notmuch-reply(1)**, **notmuch-restore(1)**,
**notmuch-search(1)**, **notmuch-show(1)**, **notmuch-tag(1)**
