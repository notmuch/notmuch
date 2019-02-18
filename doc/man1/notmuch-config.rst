==============
notmuch-config
==============

SYNOPSIS
========

**notmuch** **config** **get** <*section*>.<*item*>

**notmuch** **config** **set** <*section*>.<*item*> [*value* ...]

**notmuch** **config** **list**

DESCRIPTION
===========

The **config** command can be used to get or set settings in the notmuch
configuration file and corresponding database.

Items marked **[STORED IN DATABASE]** are only in the database.  They
should not be placed in the configuration file, and should be accessed
programmatically as described in the SYNOPSIS above.

**get**
    The value of the specified configuration item is printed to
    stdout. If the item has multiple values (it is a list), each value
    is separated by a newline character.

**set**
    The specified configuration item is set to the given value. To
    specify a multiple-value item (a list), provide each value as a
    separate command-line argument.

    If no values are provided, the specified configuration item will
    be removed from the configuration file.

**list**
    Every configuration item is printed to stdout, each on a separate
    line of the form::

        *section*.\ *item*\ =\ *value*

    No additional whitespace surrounds the dot or equals sign
    characters. In a multiple-value item (a list), the values are
    separated by semicolon characters.

The available configuration items are described below.

**database.path**
    The top-level directory where your mail currently exists and to
    where mail will be delivered in the future. Files should be
    individual email messages. Notmuch will store its database within
    a sub-directory of the path configured here named ``.notmuch``.

    Default: ``$MAILDIR`` variable if set, otherwise ``$HOME/mail``.

**user.name**
    Your full name.

    Default: ``$NAME`` variable if set, otherwise read from
    ``/etc/passwd``.

**user.primary\_email**
    Your primary email address.

    Default: ``$EMAIL`` variable if set, otherwise constructed from
    the username and hostname of the current machine.

**user.other\_email**
    A list of other email addresses at which you receive email.

    Default: not set.

**new.tags**
    A list of tags that will be added to all messages incorporated by
    **notmuch new**.

    Default: ``unread;inbox``.

**new.ignore**
    A list to specify files and directories that will not be searched
    for messages by **notmuch new**. Each entry in the list is either:

    A file or a directory name, without path, that will be ignored,
    regardless of the location in the mail store directory hierarchy.

    Or:

    A regular expression delimited with // that will be matched
    against the path of the file or directory relative to the database
    path. Matching files and directories will be ignored. The
    beginning and end of string must be explicitly anchored. For
    example, /.*/foo$/ would match "bar/foo" and "bar/baz/foo", but
    not "foo" or "bar/foobar".

    Default: empty list.

**search.exclude\_tags**
    A list of tags that will be excluded from search results by
    default. Using an excluded tag in a query will override that
    exclusion.

    Default: empty list. Note that **notmuch-setup(1)** puts
    ``deleted;spam`` here when creating new configuration file.

**maildir.synchronize\_flags**
    If true, then the following maildir flags (in message filenames)
    will be synchronized with the corresponding notmuch tags:

    +--------+-----------------------------------------------+
    | Flag   | Tag                                           |
    +========+===============================================+
    | D      | draft                                         |
    +--------+-----------------------------------------------+
    | F      | flagged                                       |
    +--------+-----------------------------------------------+
    | P      | passed                                        |
    +--------+-----------------------------------------------+
    | R      | replied                                       |
    +--------+-----------------------------------------------+
    | S      | unread (added when 'S' flag is not present)   |
    +--------+-----------------------------------------------+

    The **notmuch new** command will notice flag changes in filenames
    and update tags, while the **notmuch tag** and **notmuch restore**
    commands will notice tag changes and update flags in filenames.

    If there have been any changes in the maildir (new messages added,
    old ones removed or renamed, maildir flags changed, etc.), it is
    advisable to run **notmuch new** before **notmuch tag** or
    **notmuch restore** commands to ensure the tag changes are
    properly synchronized to the maildir flags, as the commands expect
    the database and maildir to be in sync.

    Default: ``true``.

**crypto.gpg_path**
    Name (or full path) of gpg binary to use in verification and
    decryption of PGP/MIME messages.  NOTE: This configuration item is
    deprecated, and will be ignored if notmuch is built against GMime
    3.0 or later.

    Default: ``gpg``.

**index.decrypt** **[STORED IN DATABASE]**
    Policy for decrypting encrypted messages during indexing.  Must be
    one of: ``false``, ``auto``, ``nostash``, or ``true``.

    When indexing an encrypted e-mail message, if this variable is set
    to ``true``, notmuch will try to decrypt the message and index the
    cleartext, stashing a copy of any discovered session keys for the
    message.  If ``auto``, it will try to index the cleartext if a
    stashed session key is already known for the message (e.g. from a
    previous copy), but will not try to access your secret keys.  Use
    ``false`` to avoid decrypting even when a stashed session key is
    already present.

    ``nostash`` is the same as ``true`` except that it will not stash
    newly-discovered session keys in the database.

    From the command line (i.e. during **notmuch-new(1)**,
    **notmuch-insert(1)**, or **notmuch-reindex(1)**), the user can
    override the database's stored decryption policy with the
    ``--decrypt=`` option.

    Here is a table that summarizes the functionality of each of these
    policies:

    +------------------------+-------+------+---------+------+
    |                        | false | auto | nostash | true |
    +========================+=======+======+=========+======+
    | Index cleartext using  |       |  X   |    X    |  X   |
    | stashed session keys   |       |      |         |      |
    +------------------------+-------+------+---------+------+
    | Index cleartext        |       |      |    X    |  X   |
    | using secret keys      |       |      |         |      |
    +------------------------+-------+------+---------+------+
    | Stash session keys     |       |      |         |  X   |
    +------------------------+-------+------+---------+------+
    | Delete stashed session |   X   |      |         |      |
    | keys on reindex        |       |      |         |      |
    +------------------------+-------+------+---------+------+

    Stashed session keys are kept in the database as properties
    associated with the message.  See ``session-key`` in
    **notmuch-properties(7)** for more details about how they can be
    useful.

    Be aware that the notmuch index is likely sufficient (and a
    stashed session key is certainly sufficient) to reconstruct the
    cleartext of the message itself, so please ensure that the notmuch
    message index is adequately protected.  DO NOT USE
    ``index.decrypt=true`` or ``index.decrypt=nostash`` without
    considering the security of your index.

    Default: ``auto``.

**index.header.<prefix>** **[STORED IN DATABASE]**
    Define the query prefix <prefix>, based on a mail header. For
    example ``index.header.List=List-Id`` will add a probabilistic
    prefix ``List:`` that searches the ``List-Id`` field.  User
    defined prefixes must not start with 'a'...'z'; in particular
    adding a prefix with same name as a predefined prefix is not
    supported. See **notmuch-search-terms(7)** for a list of existing
    prefixes, and an explanation of probabilistic prefixes.

**built_with.<name>**
    Compile time feature <name>. Current possibilities include
    "compact" (see **notmuch-compact(1)**) and "field_processor" (see
    **notmuch-search-terms(7)**).

**query.<name>** **[STORED IN DATABASE]**
    Expansion for named query called <name>. See
    **notmuch-search-terms(7)** for more information about named
    queries.

ENVIRONMENT
===========

The following environment variables can be used to control the behavior
of notmuch.

**NOTMUCH\_CONFIG**
    Specifies the location of the notmuch configuration file. Notmuch
    will use ${HOME}/.notmuch-config if this variable is not set.

SEE ALSO
========

**notmuch(1)**,
**notmuch-count(1)**,
**notmuch-dump(1)**,
**notmuch-hooks(5)**,
**notmuch-insert(1)**,
**notmuch-new(1)**,
**notmuch-reply(1)**,
**notmuch-restore(1)**,
**notmuch-search(1)**,
**notmuch-search-terms(7)**,
**notmuch-properties(7)**,
**notmuch-show(1)**,
**notmuch-tag(1)**
