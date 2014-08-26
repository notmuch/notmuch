===========
notmuch-tag
===========

SYNOPSIS
========

**notmuch** **tag** [options ...] +<*tag*>|-<*tag*> [--] <*search-term*> ...

**notmuch** **tag** **--batch** [--input=<*filename*>]

DESCRIPTION
===========

Add/remove tags for all messages matching the search terms.

See **notmuch-search-terms(7)** for details of the supported syntax for
<*search-term*\ >.

Tags prefixed by '+' are added while those prefixed by '-' are removed.
For each message, tag changes are applied in the order they appear on
the command line.

The beginning of the search terms is recognized by the first argument
that begins with neither '+' nor '-'. Support for an initial search term
beginning with '+' or '-' is provided by allowing the user to specify a
"--" argument to separate the tags from the search terms.

**notmuch tag** updates the maildir flags according to tag changes if
the **maildir.synchronize\_flags** configuration option is enabled. See
**notmuch-config(1)** for details.

Supported options for **tag** include

    ``--remove-all``
        Remove all tags from each message matching the search terms
        before applying the tag changes appearing on the command line.
        This means setting the tags of each message to the tags to be
        added. If there are no tags to be added, the messages will have
        no tags.

    ``--batch``
        Read batch tagging operations from a file (stdin by default).
        This is more efficient than repeated **notmuch tag**
        invocations. See `TAG FILE FORMAT <#tag_file_format>`__ below
        for the input format. This option is not compatible with
        specifying tagging on the command line.

    ``--input=``\ <filename>
        Read input from given file, instead of from stdin. Implies
        ``--batch``.

TAG FILE FORMAT
===============

The input must consist of lines of the format:

+<*tag*\ >\|-<*tag*\ > [...] [--] <*query*\ >

Each line is interpreted similarly to **notmuch tag** command line
arguments. The delimiter is one or more spaces ' '. Any characters in
<*tag*\ > **may** be hex-encoded with %NN where NN is the hexadecimal
value of the character. To hex-encode a character with a multi-byte
UTF-8 encoding, hex-encode each byte. Any spaces in <tag> **must** be
hex-encoded as %20. Any characters that are not part of <*tag*\ > **must
not** be hex-encoded.

In the future tag:"tag with spaces" style quoting may be supported for
<*tag*\ > as well; for this reason all double quote characters in
<*tag*\ > **should** be hex-encoded.

The <*query*\ > should be quoted using Xapian boolean term quoting
rules: if a term contains whitespace or a close paren or starts with a
double quote, it must be enclosed in double quotes (not including any
prefix) and double quotes inside the term must be doubled (see below for
examples).

Leading and trailing space ' ' is ignored. Empty lines and lines
beginning with '#' are ignored.

EXAMPLE
-------

The following shows a valid input to batch tagging. Note that only the
isolated '\*' acts as a wildcard. Also note the two different quotings
of the tag **space in tags**

::

    +winner *
    +foo::bar%25 -- (One and Two) or (One and tag:winner)
    +found::it -- tag:foo::bar%
    # ignore this line and the next

    +space%20in%20tags -- Two
    # add tag '(tags)', among other stunts.
    +crazy{ +(tags) +&are +#possible\ -- tag:"space in tags"
    +match*crazy -- tag:crazy{
    +some_tag -- id:"this is ""nauty)"""

SEE ALSO
========

**notmuch(1)**, **notmuch-config(1)**, **notmuch-count(1)**,
**notmuch-dump(1)**, **notmuch-hooks(5)**, **notmuch-insert(1)**,
**notmuch-new(1)**, **notmuch-reply(1)**, **notmuch-restore(1)**,
**notmuch-search(1)**, **notmuch-search-terms(7)**, **notmuch-show(1)**,
