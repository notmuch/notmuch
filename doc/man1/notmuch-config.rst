.. _notmuch-config(1):

==============
notmuch-config
==============

SYNOPSIS
========

**notmuch** **config** **get** <*section*>.<*item*>

**notmuch** **config** **set** [--database] <*section*>.<*item*> [*value* ...]

**notmuch** **config** **list**

DESCRIPTION
===========

The **config** command can be used to get or set settings in the notmuch
configuration file and corresponding database.

.. program:: config

.. option:: get

   The value of the specified configuration item is printed to
   stdout. If the item has multiple values (it is a list), each value
   is separated by a newline character.

.. option:: set

   The specified configuration item is set to the given value. To
   specify a multiple-value item (a list), provide each value as a
   separate command-line argument.

   If no values are provided, the specified configuration item will
   be removed from the configuration file.

   With the `--database` option, updates configuration metadata
   stored in the database, rather than the default (text)
   configuration file.

.. option:: list

   Every configuration item is printed to stdout, each on a separate
   line of the form::

     section.item=value

   No additional whitespace surrounds the dot or equals sign
   characters. In a multiple-value item (a list), the values are
   separated by semicolon characters.

The available configuration items are described below. Non-absolute
paths are presumed relative to `$HOME` for items in section
**database**.

**database.path**
    Notmuch will store its database here, (in
    sub-directory named ``.notmuch`` if **database.mail\_root**
    is unset).

    Default: see :ref:`database`

**database.mail_root**
    The top-level directory where your mail currently exists and to
    where mail will be delivered in the future. Files should be
    individual email messages.

    History: this configuration value was introduced in notmuch 0.32.

    Default: For compatibility with older configurations, the value of
    database.path is used if **database.mail\_root** is unset.

**database.backup_dir**
    Directory to store tag dumps when upgrading database.

    History: this configuration value was introduced in notmuch 0.32.

    Default: A sibling directory of the Xapian database called
    `backups`.

**database.hook_dir**
    Directory containing hooks run by notmuch commands. See
    :any:`notmuch-hooks(5)`.

    History: this configuration value was introduced in notmuch 0.32.

    Default: See HOOKS, below.

**database.autocommit**

    How often to commit transactions to disk. `0` means wait until
    command completes, otherwise an integer `n` specifies to commit to
    disk after every `n` completed transactions.

    History: this configuration value was introduced in notmuch 0.33.

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
    for messages by :any:`notmuch-new(1)`. Each entry in the list is either:

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

    Default: empty list. Note that :any:`notmuch-setup(1)` puts
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

    The :any:`notmuch-new(1)` command will notice flag changes in
    filenames and update tags, while the :any:`notmuch-tag(1)` and
    :any:`notmuch-restore(1)` commands will notice tag changes and
    update flags in filenames.

    If there have been any changes in the maildir (new messages added,
    old ones removed or renamed, maildir flags changed, etc.), it is
    advisable to run :any:`notmuch-new(1)` before
    :any:`notmuch-tag(1)` or :any:`notmuch-restore(1)` commands to
    ensure the tag changes are properly synchronized to the maildir
    flags, as the commands expect the database and maildir to be in
    sync.

    Default: ``true``.

**index.decrypt**
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

    From the command line (i.e. during :any:`notmuch-new(1)`,
    :any:`notmuch-insert(1)`, or :any:`notmuch-reindex(1)`), the user can
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
    :any:`notmuch-properties(7)` for more details about how they can be
    useful.

    Be aware that the notmuch index is likely sufficient (and a
    stashed session key is certainly sufficient) to reconstruct the
    cleartext of the message itself, so please ensure that the notmuch
    message index is adequately protected.  DO NOT USE
    ``index.decrypt=true`` or ``index.decrypt=nostash`` without
    considering the security of your index.

    Default: ``auto``.

**index.header.<prefix>**
    Define the query prefix <prefix>, based on a mail header. For
    example ``index.header.List=List-Id`` will add a probabilistic
    prefix ``List:`` that searches the ``List-Id`` field.  User
    defined prefixes must not start with 'a'...'z'; in particular
    adding a prefix with same name as a predefined prefix is not
    supported. See :any:`notmuch-search-terms(7)` for a list of existing
    prefixes, and an explanation of probabilistic prefixes.

**built_with.<name>**
    Compile time feature <name>. Current possibilities include
    "retry_lock" (configure option, included by default).
    (since notmuch 0.30, "compact" and "field_processor" are
    always included.)

**query.<name>**
    Expansion for named query called <name>. See
    :any:`notmuch-search-terms(7)` for more information about named
    queries.

**squery.<name>**
    Expansion for named query called <name>, using s-expression syntax. See
    :any:`notmuch-sexp-queries(7)` for more information about s-expression
    queries.

FILES
=====

.. _config_search:

CONFIGURATION
-------------

Notmuch configuration file search order:

1. File specified by :option:`notmuch --config` global option; see
   :any:`notmuch(1)`.

2. File specified by :envvar:`NOTMUCH_CONFIG` environment variable.

3. ``$XDG_CONFIG_HOME/notmuch/<profile>/config`` where ``<profile>``
   is defined by :envvar:`NOTMUCH_PROFILE` environment variable if
   set, ``$XDG_CONFIG_HOME/notmuch/default/config`` otherwise.

4. ``$HOME/.notmuch-config.<profile>`` where ``<profile>`` is defined
   by :envvar:`NOTMUCH_PROFILE` environment variable if set,
   ``$HOME/.notmuch-config`` otherwise.

.. _database:

DATABASE LOCATION
-----------------

Notmuch database search order:

1. Directory specified by :envvar:`NOTMUCH_DATABASE` environment variable.

2. Directory specified by config key ``database.path``.

3. ``$XDG_DATA_HOME/notmuch/<profile>`` where ``<profile>``
   is defined by :envvar:`NOTMUCH_PROFILE` environment variable if
   set, ``$XDG_DATA_HOME/notmuch/default`` otherwise.

4. Directory specified by :envvar:`MAILDIR` environment variable.

5. ``$HOME/mail``

HOOKS
-----

Notmuch hook directory search order:

1. Directory specified by ``database.hook_dir`` configuration option.

2. ``$XDG_CONFIG_HOME/notmuch/<profile>/hooks`` where ``<profile>``
   is defined by :envvar:`NOTMUCH_PROFILE` environment variable if
   set, ``$XDG_CONFIG_HOME/notmuch/default/hooks`` otherwise.

3. ``<database.path>/.notmuch/hooks``

SEE ALSO
========

:any:`notmuch(1)`,
:any:`notmuch-count(1)`,
:any:`notmuch-dump(1)`,
:any:`notmuch-hooks(5)`,
:any:`notmuch-insert(1)`,
:any:`notmuch-new(1)`,
:any:`notmuch-properties(7)`,
:any:`notmuch-reply(1)`,
:any:`notmuch-restore(1)`,
:any:`notmuch-search(1)`,
:any:`notmuch-search-terms(7)`,
:any:`notmuch-show(1)`,
:any:`notmuch-tag(1)`
