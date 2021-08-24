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

``*`` ``()``
    Match all messages.

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
  |    path    |    or     |   term    |    yes    |    yes    |   yes    |
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

``(subject quick "brown fox")``
    Match messages whose subject contains "quick" (anywhere, stemmed) and
    the phrase "brown fox".

NOTES
=====

.. [#aka-pref] a.k.a. prefixes

.. [#aka-prob] a.k.a. probabilistic prefixes

.. [#aka-bool] a.k.a. boolean prefixes

.. |q1| replace:: :math:`q_1`
.. |q2| replace:: :math:`q_2`
.. |qn| replace:: :math:`q_n`
