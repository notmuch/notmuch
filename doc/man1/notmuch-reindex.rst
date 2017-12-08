===============
notmuch-reindex
===============

SYNOPSIS
========

**notmuch** **reindex** [*option* ...] <*search-term*> ...

DESCRIPTION
===========

Re-index all messages matching the search terms.

See **notmuch-search-terms(7)** for details of the supported syntax for
<*search-term*\ >.

The **reindex** command searches for all messages matching the
supplied search terms, and re-creates the full-text index on these
messages using the supplied options.

Supported options for **reindex** include

    ``--decrypt=(true|false)``

        If true, when encountering an encrypted message, try to
        decrypt it while reindexing.  If decryption is successful,
        index the cleartext itself.  Be aware that the index is likely
        sufficient to reconstruct the cleartext of the message itself,
        so please ensure that the notmuch message index is adequately
        protected. DO NOT USE ``--decrypt=true`` without
        considering the security of your index.

        See also ``index.decrypt`` in **notmuch-config(1)**.

SEE ALSO
========

**notmuch(1)**,
**notmuch-config(1)**,
**notmuch-count(1)**,
**notmuch-dump(1)**,
**notmuch-hooks(5)**,
**notmuch-insert(1)**,
**notmuch-new(1)**,
**notmuch-reply(1)**,
**notmuch-restore(1)**,
**notmuch-search(1)**,
**notmuch-search-terms(7)**,
**notmuch-show(1)**,
**notmuch-tag(1)**
