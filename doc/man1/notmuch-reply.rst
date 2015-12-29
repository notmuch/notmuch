=============
notmuch-reply
=============

SYNOPSIS
========

**notmuch** **reply** [option ...] <*search-term*> ...

DESCRIPTION
===========

Constructs a reply template for a set of messages.

To make replying to email easier, **notmuch reply** takes an existing
set of messages and constructs a suitable mail template. Its To:
address is set according to the original email in this way: if the
Reply-to: header is present and different from any To:/Cc: address it
is used, otherwise From: header is used. Unless
``--reply-to=sender`` is specified, values from the To: and Cc: headers
are copied, but not including any of the current user's email addresses
(as configured in primary\_mail or other\_email in the .notmuch-config
file) in the recipient list.

It also builds a suitable new subject, including Re: at the front (if
not already present), and adding the message IDs of the messages being
replied to to the References list and setting the In-Reply-To: field
correctly.

Finally, the original contents of the emails are quoted by prefixing
each line with '> ' and included in the body.

The resulting message template is output to stdout.

Supported options for **reply** include

    ``--format=``\ (**default**\ \|\ **json**\ \|\ **sexp**\ \|\ **headers-only**)

        **default**
            Includes subject and quoted message body as an RFC 2822
            message.

        **json**
            Produces JSON output containing headers for a reply message
            and the contents of the original message. This output can be
            used by a client to create a reply message intelligently.

        **sexp**
            Produces S-Expression output containing headers for a reply
            message and the contents of the original message. This
            output can be used by a client to create a reply message
            intelligently.

        **headers-only**
            Only produces In-Reply-To, References, To, Cc, and Bcc
            headers.

    ``--format-version=N``
        Use the specified structured output format version. This is
        intended for programs that invoke **notmuch(1)** internally. If
        omitted, the latest supported version will be used.

    ``--reply-to=``\ (**all**\ \|\ **sender**)

        **all** (default)
            Replies to all addresses.

        **sender**
            Replies only to the sender. If replying to user's own
            message (Reply-to: or From: header is one of the user's
            configured email addresses), try To:, Cc:, and Bcc: headers
            in this order, and copy values from the first that contains
            something other than only the user's addresses.

    ``--decrypt``
        Decrypt any MIME encrypted parts found in the selected content
        (ie. "multipart/encrypted" parts). Status of the decryption will
        be reported (currently only supported with --format=json and
        --format=sexp) and on successful decryption the
        multipart/encrypted part will be replaced by the decrypted
        content.

        Decryption expects a functioning **gpg-agent(1)** to provide any
        needed credentials. Without one, the decryption will fail.

See **notmuch-search-terms(7)** for details of the supported syntax for
<search-terms>.

Note: It is most common to use **notmuch reply** with a search string
matching a single message, (such as id:<message-id>), but it can be
useful to reply to several messages at once. For example, when a series
of patches are sent in a single thread, replying to the entire thread
allows for the reply to comment on issues found in multiple patches. The
default format supports replying to multiple messages at once, but the
JSON and S-Expression formats do not.

EXIT STATUS
===========

This command supports the following special exit status codes

``20``
    The requested format version is too old.

``21``
    The requested format version is too new.

SEE ALSO
========

**notmuch(1)**, **notmuch-config(1)**, **notmuch-count(1)**,
**notmuch-dump(1)**, **notmuch-hooks(5)**, **notmuch-insert(1)**,
**notmuch-new(1)**, **notmuch-restore(1)**, **notmuch-search(1)**,
**notmuch-search-terms(7)**, **notmuch-show(1)**, **notmuch-tag(1)**
