.. _notmuch-sexp-queries(7):

====================
notmuch-sexp-queries
====================

SYNOPSIS
========

**notmuch** **search** ``--query=sexp`` '(and (to santa) (date december))'

DESCRIPTION
===========


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

``(id 1234@invalid blah@test)``
    Matches Message-Id "1234@invalid" *or* Message-Id "blah@test"

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

``(to (or bob@example.com mallory@example.org))`` ``(or (to bob@example.com) (to mallory@example.org))``
    Match in the "To" or "Cc" headers, "bob@example.com",
    "mallory@example.org", and also "bob@example.com.au" since it
    contains the adjacent triple "bob", "example", "com".

``(not (to *))``
    Match messages with an empty or invalid 'To' and 'Cc' field.

NOTES
=====

.. [#aka-pref] a.k.a. prefixes

.. [#aka-prob] a.k.a. probabilistic prefixes

.. [#aka-bool] a.k.a. boolean prefixes

.. [#not-body] Due the the way ``body`` is implemented in notmuch,
               this modifier is not supported in the ``body`` field.

.. |q1| replace:: :math:`q_1`
.. |q2| replace:: :math:`q_2`
.. |qn| replace:: :math:`q_n`
