.. _notmuch-reindex(1):

===============
notmuch-reindex
===============

SYNOPSIS
========

**notmuch** **reindex** [*option* ...] <*search-term*> ...

DESCRIPTION
===========

Re-index all messages matching the search terms.

See :any:`notmuch-search-terms(7)` for details of the supported syntax for
<*search-term*\ >.

The **reindex** command searches for all messages matching the
supplied search terms, and re-creates the full-text index on these
messages using the supplied options.

Supported options for **reindex** include

.. program:: reindex

.. option:: --decrypt=(true|nostash|auto|false)

   If ``true``, when encountering an encrypted message, try to
   decrypt it while reindexing, stashing any session keys discovered.
   If ``auto``, and notmuch already knows about a session key for the
   message, it will try decrypting using that session key but will
   not try to access the user's secret keys.  If decryption is
   successful, index the cleartext itself.

   ``nostash`` is the same as ``true`` except that it will not stash
   newly-discovered session keys in the database.

   If ``false``, notmuch reindex will also delete any stashed session
   keys for all messages matching the search terms.

   Be aware that the index is likely sufficient (and a stashed
   session key is certainly sufficient) to reconstruct the cleartext
   of the message itself, so please ensure that the notmuch message
   index is adequately protected. DO NOT USE ``--decrypt=true`` or
   ``--decrypt=nostash`` without considering the security of your
   index.

   See also ``index.decrypt`` in :any:`notmuch-config(1)`.

EXAMPLES
========

A user just received an encrypted message without indexing its
cleartext.  After reading it (via ``notmuch show --decrypt=true``),
they decide that they want to index its cleartext so that they can
easily find it later and read it without having to have access to
their secret keys:

::

 notmuch reindex --decrypt=true id:1234567@example.com

A user wants to change their policy going forward to start indexing
cleartext.  But they also want indexed access to the cleartext of all
previously-received encrypted messages.  Some messages might have
already been indexed in the clear (as in the example above). They can
ask notmuch to just reindex the not-yet-indexed messages:

::

  notmuch config set index.decrypt true
  notmuch reindex tag:encrypted and not property:index.decryption=success

Later, the user changes their mind, and wants to stop indexing
cleartext (perhaps their threat model has changed, or their trust in
their index store has been shaken).  They also want to clear all of
their old cleartext from the index.  Note that they compact the
database afterward as a workaround for
https://trac.xapian.org/ticket/742:

::

  notmuch config set index.decrypt false
  notmuch reindex property:index.decryption=success
  notmuch compact

SEE ALSO
========

:any:`notmuch(1)`,
:any:`notmuch-compact(1)`,
:any:`notmuch-config(1)`,
:any:`notmuch-count(1)`,
:any:`notmuch-dump(1)`,
:any:`notmuch-hooks(5)`,
:any:`notmuch-insert(1)`,
:any:`notmuch-new(1)`,
:any:`notmuch-reply(1)`,
:any:`notmuch-restore(1)`,
:any:`notmuch-search(1)`,
:any:`notmuch-search-terms(7)`,
:any:`notmuch-show(1)`,
:any:`notmuch-tag(1)`
