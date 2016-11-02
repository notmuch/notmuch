==============
notmuch-report
==============

NAME
====

notmuch-report.json - configure output for **notmuch-report(1)**

DESCRIPTION
===========

The config file is JSON_ with the following fields:

meta
  An object with page-wide information

  title
    Page title used in the default header.

  blurb
    Introduction paragraph used in the default header.

  header
    `Python format string`_ for the HTML header.  Optional.  It is
    formatted with the following context:

    date
      The current UTC date.

    datetime
      The current UTC date-time.

    title
      The **meta.title** value.

    blurb
      The **meta.blurb** value.

    encoding
      The encoding used for the output file.

    inter_message_padding
      0.25em, for consistent CSS generation.

    border_radius
      0.5em, for consistent CSS generation.

  footer
    `Python format string`_ for the HTML footer.  It is formatted with
    the same context used for **meta.header**.  Optional.

  message-url
    `Python format string`_ for message-linking URLs.  Optional.
    Defaults to linking Gmane_.  It is formatted with the following
    context:

    message-id
      The quoted_ message ID.

    subject
      The message subject.

views
  An array of view objects, where each object has the following
  fields:

  title
    Header text for the view.

  comment
    Paragraph describing the view in more detail.  Optional.

  id
    Anchor string for the view.  Optional, defaulting to a slugged
    form of the view title

  query
    An array of strings, which will be joined with 'and' to form the
    view query.

.. _Gmane: http://gmane.org/
.. _JSON: http://json.org/
.. _Python format string: https://docs.python.org/3/library/string.html#formatstrings
.. _quoted: https://docs.python.org/3/library/urllib.parse.html#urllib.parse.quote

EXAMPLE
=======

::

  {
    "meta": {
      "title": "Notmuch Patches",
      "blurb": "For more information see <a href=\"https://notmuchmail.org/nmbug\">nmbug</a>",
      "header": "<html><head></head><body><h1>{title}</h1><p>{blurb}</p><h2>Views</h2>",
      "footer": "<hr><p>Generated: {datetime}</p></html>",
      "message-url": "http://mid.gmane.org/{message-id}"
    },
    "views": [
      {
        "title": "Bugs",
        "comment": "Unresolved bugs.",
        "query": [
          "tag:notmuch::bug",
          "not tag:notmuch::fixed",
          "not tag:notmuch::wontfix"
        ]
      },
      {
        "title": "Review",
        "comment": "These patches are under review, or waiting for feedback.",
        "id": "under-review",
        "query": [
          "tag:notmuch::patch",
          "not tag:notmuch::pushed",
          "not tag:notmuch::obsolete",
          "not tag:notmuch::stale",
          "not tag:notmuch::wontfix",
          "(tag:notmuch::moreinfo or tag:notmuch::needs-review)"
        ]
      }
    ]
  }

SEE ALSO
========

**notmuch(1)**, **notmuch-report(1)**, **notmuch-search(1)**, **notmuch-tag(1)**
