.. _notmuch-sexp-queries(7):

====================
notmuch-sexp-queries
====================

SYNOPSIS
========

**notmuch** *subcommand* ``--query=sexp`` [option ...]  ``--`` '(and (to santa) (date december))'

DESCRIPTION
===========

Notmuch supports an alternative query syntax based on `S-expressions
<https://en.wikipedia.org/wiki/S-expression>`_ . It can be selected
with the command line ``--query=sexp`` or with the appropriate option
to the library function :c:func:`notmuch_query_create_with_syntax`.
Support for this syntax is currently optional, you can test if your
build of notmuch supports it with

::

   $ notmuch config get built_with.sexp_queries


S-EXPRESSIONS
-------------

An *s-expression* is either an atom, or list of whitespace delimited
s-expressions inside parentheses. Atoms are either

*basic value*

    A basic value is an unquoted string containing no whitespace, double quotes, or
    parentheses.

*quoted string*

    Double quotes (") delimit strings possibly containing whitespace
    or parentheses. These can contain double quote characters by
    escaping with backslash. E.g. ``"this is a quote \""``.

S-EXPRESSION QUERIES
--------------------

An s-expression query is either an atom, the empty list, or a
*compound query* consisting of a prefix atom (first element) defining
a *field*, *logical operation*, or *modifier*, and 0 or more
subqueries.

``*``

   "*" matches any non-empty string in the current field.

``()``

    The empty list matches all messages

*term*

    Match all messages containing *term*, possibly after stemming or
    phrase splitting. For discussion of stemming in notmuch see
    :any:`notmuch-search-terms(7)`. Stemming only applies to unquoted
    terms (basic values) in s-expression queries.  For information on
    phrase splitting see :any:`fields`.

``(`` *field* |q1| |q2| ... |qn| ``)``

    Restrict the queries |q1| to |qn| to *field*, and combine with *and*
    (for most fields) or *or*. See :any:`fields` for more information.

``(`` *operator* |q1| |q2| ... |qn| ``)``

    Combine queries |q1| to |qn|. Currently supported operators are
    ``and``, ``or``, and ``not``. ``(not`` |q1| ... |qn| ``)`` is equivalent
    to ``(and (not`` |q1| ``) ... (not`` |qn| ``))``.

``(`` *modifier* |q1| |q2| ... |qn| ``)``

    Combine queries |q1| to |qn|, and reinterpret the result (e.g. as a regular expression).
    See :any:`modifiers` for more information.

``(macro (`` |p1| ... |pn| ``) body)``

    Define saved query with parameter substitution. The syntax is
    recognized only in saved s-expression queries (see ``squery.*`` in
    :any:`notmuch-config(1)`). Parameter names in ``body`` must be
    prefixed with ``,`` to be expanded (see :any:`macro_examples`).
    Macros may refer to other macros, but only to their own
    parameters [#macro-details]_.

.. _fields:

FIELDS
``````

*Fields* [#aka-pref]_
correspond to attributes of mail messages. Some are inherent (and
immutable) like ``subject``, while others ``tag`` and ``property`` are
settable by the user.  Each concrete field in
:any:`the table below <field-table>`
is discussed further under "Search prefixes" in
:any:`notmuch-search-terms(7)`. The row *user* refers to user defined
fields, described in :any:`notmuch-config(1)`.

Most fields are either *phrase fields* [#aka-prob]_ (which match
sequences of words), or *term fields* [#aka-bool]_ (which match exact
strings). *Phrase splitting* breaks the term (basic value or quoted
string) into words, ignore punctuation. Phrase splitting is applied to
terms in phrase (probabilistic) fields. Both phrase splitting and
stemming apply only in phrase fields.

Each term or phrase field has an associated combining operator
(``and`` or ``or``) used to combine the queries from each element of
the tail of the list. This is generally ``or`` for those fields where
a message has one such attribute, and ``and`` otherwise.

Term or phrase fields can contain arbitrarily complex queries made up
from terms, operators, and modifiers, but not other fields.

.. _field-table:

.. table:: Fields with supported modifiers

  +------------+-----------+-----------+-----------+-----------+----------+
  |   field    |  combine  |   type    |  expand   | wildcard  |  regex   |
  +============+===========+===========+===========+===========+==========+
  |   *none*   |    and    |           |    no     |    yes    |    no    |
  +------------+-----------+-----------+-----------+-----------+----------+
  |   *user*   |    and    |  phrase   |    no     |    yes    |    no    |
  +------------+-----------+-----------+-----------+-----------+----------+
  | attachment |    and    |  phrase   |    yes    |    yes    |    no    |
  +------------+-----------+-----------+-----------+-----------+----------+
  |    body    |    and    |  phrase   |    no     |    no     |    no    |
  +------------+-----------+-----------+-----------+-----------+----------+
  |    date    |           |   range   |    no     |    no     |    no    |
  +------------+-----------+-----------+-----------+-----------+----------+
  |   folder   |    or     |  phrase   |    yes    |    yes    |   yes    |
  +------------+-----------+-----------+-----------+-----------+----------+
  |    from    |    and    |  phrase   |    yes    |    yes    |   yes    |
  +------------+-----------+-----------+-----------+-----------+----------+
  |     id     |    or     |   term    |    no     |    yes    |   yes    |
  +------------+-----------+-----------+-----------+-----------+----------+
  |     is     |    and    |   term    |    yes    |    yes    |   yes    |
  +------------+-----------+-----------+-----------+-----------+----------+
  |  lastmod   |           |   range   |    no     |    no     |    no    |
  +------------+-----------+-----------+-----------+-----------+----------+
  |    mid     |    or     |   term    |    no     |    yes    |   yes    |
  +------------+-----------+-----------+-----------+-----------+----------+
  |  mimetype  |    or     |  phrase   |    yes    |    yes    |    no    |
  +------------+-----------+-----------+-----------+-----------+----------+
  |    path    |    or     |   term    |    no     |    yes    |   yes    |
  +------------+-----------+-----------+-----------+-----------+----------+
  |  property  |    and    |   term    |    yes    |    yes    |   yes    |
  +------------+-----------+-----------+-----------+-----------+----------+
  |  subject   |    and    |  phrase   |    yes    |    yes    |   yes    |
  +------------+-----------+-----------+-----------+-----------+----------+
  |    tag     |    and    |   term    |    yes    |    yes    |   yes    |
  +------------+-----------+-----------+-----------+-----------+----------+
  |   thread   |    or     |   term    |    yes    |    yes    |   yes    |
  +------------+-----------+-----------+-----------+-----------+----------+
  |     to     |    and    |  phrase   |    yes    |    yes    |    no    |
  +------------+-----------+-----------+-----------+-----------+----------+

.. _modifiers:

MODIFIERS
`````````

*Modifiers* refer to any prefixes (first elements of compound queries)
that are neither operators nor fields.

``(infix`` *atom* ``)``

    Interpret *atom* as an infix notmuch query (see
    :any:`notmuch-search-terms(7)`). Not supported inside fields.

``(matching`` |q1| |q2| ... |qn| ``)`` ``(of`` |q1| |q2| ... |qn|  ``)``

    Match all messages have the same values of the current field as
    those matching all of |q1| ... |qn|. Supported in most term [#not-path]_ or
    phrase fields. Most commonly used in the ``thread`` field.

``(query`` *atom* ``)``

    Expand to the saved query named by *atom*. See
    :any:`notmuch-config(1)` for more. Note that the saved query must
    be in infix syntax (:any:`notmuch-search-terms(7)`). Not supported
    inside fields.

``(regex`` *atom* ``)`` ``(rx`` *atom* ``)``

    Interpret *atom* as a POSIX.2 regular expression (see
    :manpage:`regex(7)`). This applies in term fields and a subset [#not-phrase]_ of
    phrase fields (see :any:`field-table`).

``(starts-with`` *subword* ``)``

    Matches any term starting with *subword*.  This applies in either
    phrase or term :any:`fields <fields>`, or outside of fields [#not-body]_. Note that
    a ``starts-with`` query cannot be part of a phrase. The
    atom ``*`` is a synonym for ``(starts-with "")``.

EXAMPLES
========

``Wizard``

    Match all messages containing the word "wizard", ignoring case.

``added``

    Match all messages containing "added", but also those containing "add", "additional",
    "Additional", "adds", etc... via stemming.

``(and Bob Marley)``

    Match messages containing words "Bob" and "Marley", or their stems
    The words need not be adjacent.

``(not Bob Marley)``

    Match messages containing neither "Bob" nor "Marley", nor their stems,

``"quick fox"`` ``quick-fox`` ``quick@fox``

    Match the *phrase* "quick" followed by "fox" in phrase fields (or
    outside a field). Match the literal string in a term field.

``(folder (of (id 1234@invalid)))``

    Match any message in the same folder as the one with Message-Id "1234@invalid"

``(id 1234@invalid blah@test)``

    Matches Message-Id "1234@invalid" *or* Message-Id "blah@test"

``(and (infix "date:2009-11-18..2009-11-18") (tag unread))``

    Match messages in the given date range with tag unread.

``(starts-with prelim)``

    Match any words starting with "prelim".

``(subject quick "brown fox")``

    Match messages whose subject contains "quick" (anywhere, stemmed) and
    the phrase "brown fox".

``(subject (starts-with prelim))``

    Matches any word starting with "prelim", inside a message subject.

``(subject (starts-wih quick) "brown fox")``

    Match messages whose subject contains "quick brown fox", but also
    "brown fox quicksand".

``(thread (of (id 1234@invalid)))``

    Match any message in the same thread as the one with Message-Id "1234@invalid"

``(thread (matching (from bob@example.com) (to bob@example.com)))``

    Match any (messages in) a thread containing a message from
    "bob@example.com" and a (possibly distinct) message to "bob at
    example.com")

``(to (or bob@example.com mallory@example.org))`` ``(or (to bob@example.com) (to mallory@example.org))``

    Match in the "To" or "Cc" headers, "bob@example.com",
    "mallory@example.org", and also "bob@example.com.au" since it
    contains the adjacent triple "bob", "example", "com".

``(not (to *))``

    Match messages with an empty or invalid 'To' and 'Cc' field.

``(List *)``

    Match messages with a non-empty List-Id header, assuming
    configuration ``index.header.List=List-Id``

.. _macro_examples:

MACRO EXAMPLES
--------------

A macro that takes two parameters and applies different fields to them.

::

   $ notmuch config set squery.TagSubject '(macro (tagname subj) (and (tag ,tagname) (subject ,subj)))'
   $ notmuch search --query=sexp '(TagSubject inbox maildir)'

Nested macros are allowed.

::

    $ notmuch config set squery.Inner '(macro (x) (subject ,x))'
    $ notmuch config set squery.Outer  '(macro (x y) (and (tag ,x) (Inner ,y)))'
    $ notmuch search --query=sexp '(Outer inbox maildir)'

Parameters can be re-used to reduce boilerplate. Any field, including
user defined fields is permitted within a macro.

::

    $ notmuch config set squery.About '(macro (name) (or (subject ,name) (List ,name)))'
    $ notmuch search --query=sexp '(About notmuch)'


NOTES
=====

.. [#macro-details] Technically macros impliment lazy evaluation and
                    lexical scope. There is one top level scope
                    containing all macro definitions, but all
                    parameter definitions are local to a given macro.

.. [#aka-pref] a.k.a. prefixes

.. [#aka-prob] a.k.a. probabilistic prefixes

.. [#aka-bool] a.k.a. boolean prefixes

.. [#not-phrase] Due to the implemention of phrase fields in Xapian,
                 regex queries could only match individual words.

.. [#not-body] Due the the way ``body`` is implemented in notmuch,
               this modifier is not supported in the ``body`` field.

.. [#not-path] Due to the way recursive ``path`` queries are implemented
               in notmuch, this modifier is not supported in the
               ``path`` field.

.. |q1| replace:: `q`\ :sub:`1`
.. |q2| replace:: `q`\ :sub:`2`
.. |qn| replace:: `q`\ :sub:`n`

.. |p1| replace:: `p`\ :sub:`1`
.. |p2| replace:: `p`\ :sub:`2`
.. |pn| replace:: `p`\ :sub:`n`
