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
configuration file.

    **get**
        The value of the specified configuration item is printed to
        stdout. If the item has multiple values (it is a list), each
        value is separated by a newline character.

    **set**
        The specified configuration item is set to the given value. To
        specify a multiple-value item (a list), provide each value as a
        separate command-line argument.

        If no values are provided, the specified configuration item will
        be removed from the configuration file.

    **list**
        Every configuration item is printed to stdout, each on a
        separate line of the form:

        *section*.\ *item*\ =\ *value*

        No additional whitespace surrounds the dot or equals sign
        characters. In a multiple-value item (a list), the values are
        separated by semicolon characters.

The available configuration items are described below.

    **database.path**
        The top-level directory where your mail currently exists and to
        where mail will be delivered in the future. Files should be
        individual email messages. Notmuch will store its database
        within a sub-directory of the path configured here named
        ``.notmuch``.

        Default: ``$MAILDIR`` variable if set, otherwise ``$HOME/mail``.

    **user.name**
        Your full name.

        Default: ``$NAME`` variable if set, otherwise read from
        ``/etc/passwd``.

    **user.primary\_email**
        Your primary email address.

        Default: ``$EMAIL`` variable if set, otherwise constructed from the
        username and hostname of the current machine.

    **user.other\_email**
        A list of other email addresses at which you receive email.

        Default: not set.

    **new.tags**
        A list of tags that will be added to all messages incorporated
        by **notmuch new**.

        Default: ``unread;inbox``.

    **new.ignore**
        A list of file and directory names, without path, that will not
        be searched for messages by **notmuch new**. All the files and
        directories matching any of the names specified here will be
        ignored, regardless of the location in the mail store directory
        hierarchy.

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

        The **notmuch new** command will notice flag changes in
        filenames and update tags, while the **notmuch tag** and
        **notmuch restore** commands will notice tag changes and update
        flags in filenames.

        If there have been any changes in the maildir (new messages
        added, old ones removed or renamed, maildir flags changed,
        etc.), it is advisable to run **notmuch new** before **notmuch
        tag** or **notmuch restore** commands to ensure the tag changes
        are properly synchronized to the maildir flags, as the commands
        expect the database and maildir to be in sync.

        Default: ``true``.

    **crypto.gpg_path**

        Name (or full path) of gpg binary to use in verification and
        decryption of PGP/MIME messages.  NOTE: This configuration
        item is deprecated, and will be ignored if notmuch is built
        against GMime 3.0 or later.

        Default: ``gpg``.

    **built_with.<name>**

        Compile time feature <name>. Current possibilities include
        "compact" (see **notmuch-compact(1)**)
        and "field_processor" (see **notmuch-search-terms(7)**).

    **query.<name>**

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

**notmuch(1)**, **notmuch-count(1)**, **notmuch-dump(1)**,
**notmuch-hooks(5)**, **notmuch-insert(1)**, **notmuch-new(1)**,
**notmuch-reply(1)**, **notmuch-restore(1)**, **notmuch-search(1)**,
**notmuch-search-terms(7)**, **notmuch-show(1)**, **notmuch-tag(1)**
