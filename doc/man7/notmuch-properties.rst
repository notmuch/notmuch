==================
notmuch-properties
==================

SYNOPSIS
========

**notmuch** **count** **property:**\ <*key*>=<*value*>

**notmuch** **search** **property:**\ <*key*>=<*value*>

**notmuch** **show** **property:**\ <*key*>=<*value*>

**notmuch** **reindex** **property:**\ <*key*>=<*value*>

**notmuch** **tag** +<*tag*> **property:**\ <*key*>=<*value*>


**notmuch** **dump** **--include=properties**

**notmuch** **restore** **--include=properties**

DESCRIPTION
===========

Several notmuch commands can search for, modify, add or remove
properties associated with specific messages.  Properties are
key/value pairs, and a message can have more than one key/value pair
for the same key.

While users can select based on a specific property in their search
terms with the prefix **property:**, the notmuch command-line
interface does not provide mechanisms for modifying properties
directly to the user.

Instead, message properties are expected to be set and used
programmatically, according to logic in notmuch itself, or in
extensions to it.

Extensions to notmuch which make use of properties are encouraged to
report the specific properties used to the upstream notmuch project,
as a way of avoiding collisions in the property namespace.

CONVENTIONS
===========

Any property with a key that starts with "index." will be removed (and
possibly re-set) upon reindexing (see **notmuch-reindex(1)**).

MESSAGE PROPERTIES
==================

The following properties are set by notmuch internally in the course
of its normal activity.

**index.decryption**

    If a message contains encrypted content, and notmuch tries to
    decrypt that content during indexing, it will add the property
    ``index.decryption=success`` when the cleartext was successfully
    indexed.  If notmuch attempts to decrypt any part of a message
    during indexing and that decryption attempt fails, it will add the
    property ``index.decryption=failure`` to the message.

    Note that it's possible for a single message to have both
    ``index.decryption=success`` and ``index.decryption=failure``.
    Consider an encrypted e-mail message that contains another
    encrypted e-mail message as an attachment -- if the outer message
    can be decrypted, but the attached part cannot, then both
    properties will be set on the message as a whole.

    If notmuch never tried to decrypt an encrypted message during
    indexing (which is the default), then this property will not be
    set on that message.

SEE ALSO
========

**notmuch(1)**,
**notmuch-dump(1)**,
**notmuch-insert(1)**,
**notmuch-new(1)**,
**notmuch-reindex(1)**,
**notmuch-restore(1)**,
***notmuch-search-terms(7)**
