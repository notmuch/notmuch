==============
notmuch-report
==============

SYNOPSIS
========

**notmuch-report** [options ...]

DESCRIPTION
===========

Generate HTML or plain-text reports showing query results.

OPTIONS
=======

  ``-h``, ``--help``

    Show a help message, including a list of available options, and
    exit.

  ``--text``
    Output plain text instead of HTML.

  ``--config`` <PATH>
    Load config from given file.  The format is described in
    **notmuch-report.json(5)**.  If this option is not set,
    **notmuch-report** loads the config from the Git repository at
    ``NMBGIT``.  See :ref:`NMBGIT <NMBGIT>` for details.

  ``--list-views``
    List available views (by title) and exit.

  ``--get-query`` <VIEW>
    Print the configured query for view matching the given title.

ENVIRONMENT
===========

.. _NMBGIT:

  ``NMBGIT``
    If ``--config PATH`` is not set, **notmuch-report** will attempt
    to load a config file named ``notmuch-report.json`` from the
    ``config`` branch of the ``NMBGIT`` repository (defaulting to
    ``~/.nmbug``).

SEE ALSO
========

**notmuch(1)**, **notmuch-report.json(5)**, **notmuch-search(1)**,
 **notmuch-tag(1)**

