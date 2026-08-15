``filter.py`` is a Python script implementing an external attachment filter for
notmuch indexing (see ``index.filter`` in :any:`notmuch-config(1)` for details).

The filter supports the following data types:

* PDF, converted with ``pdftotext``;

* HTML, converted with ``w3m`` or ``elinks``;

* office formats (MS Office, ODF, RTF), converted with ``soffice`` (part of
  ``LibreOffice``);

* a variety of text-like formats (scripts, source code, diffs, etc.) are passed
  through unchanged;

* archives supported by libarchive are read recursively; this requires the
  libarchive Python bindings (``python3-libarchive-c`` package on Debian);

* email messages, either standalone or in mbox format, are parsed and converted
  recursively;

To avoid recursion bombs, recursion depth for archives and email messages is by
default limited to 8, use the ``--max-recurse`` switch to change it.
