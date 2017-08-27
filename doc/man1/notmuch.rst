=======
notmuch
=======

SYNOPSIS
========

**notmuch** [option ...] **command** [arg ...]

DESCRIPTION
===========

Notmuch is a command-line based program for indexing, searching,
reading, and tagging large collections of email messages.

This page describes how to get started using notmuch from the command
line, and gives a brief overview of the commands available. For more
information on e.g. **notmuch show** consult the **notmuch-show(1)** man
page, also accessible via **notmuch help show**

The quickest way to get started with Notmuch is to simply invoke the
``notmuch`` command with no arguments, which will interactively guide
you through the process of indexing your mail.

NOTE
====

While the command-line program ``notmuch`` provides powerful
functionality, it does not provide the most convenient interface for
that functionality. More sophisticated interfaces are expected to be
built on top of either the command-line interface, or more likely, on
top of the notmuch library interface. See https://notmuchmail.org for
more about alternate interfaces to notmuch. The emacs-based interface to
notmuch (available under **emacs/** in the Notmuch source distribution)
is probably the most widely used at this time.

OPTIONS
=======

Supported global options for ``notmuch`` include

    ``--help`` [command-name]
	Print a synopsis of available commands and exit.
	With an optional command name, show the man page
	for that subcommand.

    ``--version``
	Print the installed version of notmuch, and exit.

    ``--config=FILE``
	Specify the configuration file to use. This overrides any
	configuration file specified by ${NOTMUCH\_CONFIG}.

    ``--uuid=HEX``
       Enforce that the database UUID (a unique identifier which
       persists until e.g. the database is compacted)
       is HEX; exit with an error if it is not. This is useful to
       detect rollover in modification counts on messages. You can
       find this UUID using e.g. ``notmuch count --lastmod``

All global options except ``--config`` can also be specified after the
command. For example, ``notmuch subcommand --uuid=HEX`` is
equivalent to ``notmuch --uuid=HEX subcommand``.

COMMANDS
========

SETUP
-----

The **notmuch setup** command is used to configure Notmuch for first
use, (or to reconfigure it later).

The setup command will prompt for your full name, your primary email
address, any alternate email addresses you use, and the directory
containing your email archives. Your answers will be written to a
configuration file in ${NOTMUCH\_CONFIG} (if set) or
${HOME}/.notmuch-config . This configuration file will be created with
descriptive comments, making it easy to edit by hand later to change the
configuration. Or you can run **notmuch setup** again to change the
configuration.

The mail directory you specify can contain any number of sub-directories
and should primarily contain only files with individual email messages
(eg. maildir or mh archives are perfect). If there are other, non-email
files (such as indexes maintained by other email programs) then notmuch
will do its best to detect those and ignore them.

Mail storage that uses mbox format, (where one mbox file contains many
messages), will not work with notmuch. If that's how your mail is
currently stored, it is recommended you first convert it to maildir
format with a utility such as mb2md before running **notmuch setup .**

Invoking ``notmuch`` with no command argument will run **setup** if the
setup command has not previously been completed.

OTHER COMMANDS
--------------

Several of the notmuch commands accept search terms with a common
syntax. See **notmuch-search-terms**\ (7) for more details on the
supported syntax.

The **search**, **show**, **address** and **count** commands are used
to query the email database.

The **reply** command is useful for preparing a template for an email
reply.

The **tag** command is the only command available for manipulating
database contents.

The **dump** and **restore** commands can be used to create a textual
dump of email tags for backup purposes, and to restore from that dump.

The **config** command can be used to get or set settings in the notmuch
configuration file.

CUSTOM COMMANDS
---------------

If the given command is not known to notmuch, notmuch tries to execute
the external **notmuch-<subcommand>** in ${PATH} instead. This allows
users to have their own notmuch related tools to be run via the
notmuch command. By design, this does not allow notmuch's own commands
to be overridden using external commands.

OPTION SYNTAX
-------------

All options accepting an argument can be used with '=' or ':' as a
separator. For the cases where it's not ambiguous (in particular
excluding boolean options), a space can also be used. The following
are all equivalent:

::

   notmuch --config=alt-config config get user.name
   notmuch --config:alt-config config get user.name
   notmuch --config alt-config config get user.name

ENVIRONMENT
===========

The following environment variables can be used to control the behavior
of notmuch.

**NOTMUCH\_CONFIG**
    Specifies the location of the notmuch configuration file. Notmuch
    will use ${HOME}/.notmuch-config if this variable is not set.

**NOTMUCH\_TALLOC\_REPORT**
    Location to write a talloc memory usage report. See
    **talloc\_enable\_leak\_report\_full** in **talloc(3)** for more
    information.

**NOTMUCH\_DEBUG\_QUERY**
    If set to a non-empty value, the notmuch library will print (to
    stderr) Xapian queries it constructs.

SEE ALSO
========

**notmuch-address(1)**, **notmuch-compact(1)**, **notmuch-config(1)**,
**notmuch-count(1)**, **notmuch-dump(1)**, **notmuch-hooks(5)**,
**notmuch-insert(1)**, **notmuch-new(1)**, **notmuch-reply(1)**,
**notmuch-restore(1)**, **notmuch-search(1)**,
**notmuch-search-terms(7)**, **notmuch-show(1)**, **notmuch-tag(1)**

The notmuch website: **https://notmuchmail.org**

CONTACT
=======

Feel free to send questions, comments, or kudos to the notmuch mailing
list <notmuch@notmuchmail.org> . Subscription is not required before
posting, but is available from the notmuchmail.org website.

Real-time interaction with the Notmuch community is available via IRC
(server: irc.freenode.net, channel: #notmuch).
