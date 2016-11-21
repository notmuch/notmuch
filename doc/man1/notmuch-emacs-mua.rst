=================
notmuch-emacs-mua
=================

SYNOPSIS
========

**notmuch** **emacs-mua** [options ...] [<to-address> ... | <mailto-url>]

DESCRIPTION
===========

Start composing an email in the Notmuch Emacs UI with the specified
subject, recipients, and message body, or mailto: URL.

Supported options for **emacs-mua** include

    ``-h, --help``
        Display help.

    ``-s, --subject=``\ <subject>
        Specify the subject of the message.

    ``--to=``\ <to-address>
        Specify a recipient (To).

    ``-c, --cc=``\ <cc-address>
        Specify a carbon-copy (Cc) recipient.

    ``-b, --bcc=``\ <bcc-address>
        Specify a blind-carbon-copy (Bcc) recipient.

    ``-i, --body=``\ <file>
        Specify a file to include into the body of the message.

    ``--hello``
        Go to the Notmuch hello screen instead of the message composition
        window if no message composition parameters are given.

    ``--no-window-system``
        Even if a window system is available, use the current terminal.

    ``--client``
        Use **emacsclient**, rather than **emacs**. For
        **emacsclient** to work, you need an already running Emacs
        with a server, or use ``--auto-daemon``.

    ``--auto-daemon``
        Automatically start Emacs in daemon mode, if the Emacs server
        is not running. Applicable with ``--client``. Implies
        ``--create-frame``.

    ``--create-frame``
        Create a new frame instead of trying to use the current Emacs
        frame. Applicable with ``--client``. This will be required
        when Emacs is running (or automatically started with
        ``--auto-daemon``) in daemon mode.

    ``--print``
        Output the resulting elisp to stdout instead of evaluating it.

The supported positional parameters and short options are a compatible
subset of the **mutt** MUA command-line options. The options and
positional parameters modifying the message can't be combined with the
mailto: URL.

Options may be specified multiple times.

ENVIRONMENT VARIABLES
=====================

**EMACS**
    Name of emacs command to invoke. Defaults to "emacs".

**EMACSCLIENT**
    Name of emacsclient command to invoke. Defaults to "emacsclient".

SEE ALSO
========

**notmuch(1)**, **emacsclient(1)**, **mutt(1)**
