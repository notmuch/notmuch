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
``()``
    The empty list matches all messages

*term*
    Match all messages containing *term*, possibly after stemming
    or phase splitting.

``(`` *field* |q1| |q2| ... |qn| ``)``
    Restrict the queries |q1| to |qn| to *field*, and combine with *and*
    (for most fields) or *or*. See :any:`fields` for more information.

``(`` *operator* |q1| |q2| ... |qn| ``)``
    Combine queries |q1| to |qn|. See :any:`operators` for more information.

``(`` *modifier* |q1| |q2| ... |qn| ``)``
    Combine queries |q1| to |qn|, and reinterpret the result (e.g. as a regular expression).
    See :any:`modifiers` for more information.

.. _fields:

FIELDS
``````

.. _operators:

OPERATORS
`````````

.. _modifiers:

MODIFIERS
`````````

EXAMPLES
========

``Wizard``
    Match all messages containing the word "wizard", ignoring case.

.. |q1| replace:: :math:`q_1`
.. |q2| replace:: :math:`q_2`
.. |qn| replace:: :math:`q_n`
