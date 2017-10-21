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

SEE ALSO
========

**notmuch(1)**,
**notmuch-dump(1)**,
**notmuch-insert(1)**,
**notmuch-new(1)**,
**notmuch-reindex(1)**,
**notmuch-restore(1)**,
***notmuch-search-terms(7)**
