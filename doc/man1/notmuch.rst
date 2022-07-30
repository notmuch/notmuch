.. _notmuch(1):
.. _notmuch-setup(1):

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
information on e.g. **notmuch show** consult the
:any:`notmuch-show(1)` man page, also accessible via **notmuch help
show**

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

.. program:: notmuch

.. option:: --help [command-name]

   Print a synopsis of available commands and exit. With an optional
   command name, show the man page for that subcommand.

.. option:: --version

   Print the installed version of notmuch, and exit.

.. option:: --config=FILE

   Specify the configuration file to use. This overrides any
   configuration file specified by :envvar:`NOTMUCH_CONFIG`. The empty
   string is a permitted and sometimes useful value of *FILE*, which
   tells ``notmuch`` to use only configuration metadata from the database.

.. option:: --uuid=HEX

   Enforce that the database UUID (a unique identifier which persists
   until e.g. the database is compacted) is HEX; exit with an error
   if it is not. This is useful to detect rollover in modification
   counts on messages. You can find this UUID using e.g. ``notmuch
   count --lastmod``

All global options except ``--config`` can also be specified after the
command. For example, ``notmuch subcommand --uuid=HEX`` is equivalent
to ``notmuch --uuid=HEX subcommand``.

COMMANDS
========

SETUP
-----

The **notmuch setup** command is used to configure Notmuch for first
use, (or to reconfigure it later).

The setup command will prompt for your full name, your primary email
address, any alternate email addresses you use, and the directory
containing your email archives. Your answers will be written to a
configuration file in :envvar:`NOTMUCH_CONFIG` (if set) or
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
format with a utility such as :manpage:`mb2md(1)` before running
**notmuch setup**.

Invoking ``notmuch`` with no command argument will run **setup** if the
setup command has not previously been completed.

OTHER COMMANDS
--------------

Several of the notmuch commands accept search terms with a common
syntax. See :any:`notmuch-search-terms(7)` for more details on the
supported syntax.

The :any:`notmuch-search(1)`, :any:`notmuch-show(1)`,
:any:`notmuch-address(1)` and :any:`notmuch-count(1)` commands are
used to query the email database.

The :any:`notmuch-reply(1)` command is useful for preparing a template
for an email reply.

The :any:`notmuch-tag(1)` command is the only command available for
manipulating database contents.

The :any:`notmuch-dump(1)` and :any:`notmuch-restore(1)` commands can
be used to create a textual dump of email tags for backup purposes,
and to restore from that dump.

The :any:`notmuch-config(1)` command can be used to get or set
settings in the notmuch configuration file.

EXTERNAL COMMANDS
-----------------

If the given command is not known to notmuch, notmuch tries to execute
the external **notmuch-<subcommand>** in :envvar:`PATH` instead. This
allows users to have their own notmuch related tools to be run via the
notmuch command. By design, this does not allow notmuch's own commands
to be overridden using external commands.  The environment variable
:envvar:`NOTMUCH_CONFIG` will be set according to :option:`--config`,
if the latter is present.

OPTION SYNTAX
-------------

All options accepting an argument can be used with '=' or ':' as a
separator. Except for boolean options (which would be ambiguous), a
space can also be used as a separator. The following are all
equivalent:

::

   notmuch --config=alt-config config get user.name
   notmuch --config:alt-config config get user.name
   notmuch --config alt-config config get user.name

.. _duplicate-files:

DUPLICATE MESSAGE FILES
=======================

Notmuch considers the :mailheader:`Message-ID` to be the primary
identifier of message. Per :rfc:`5322` the :mailheader:`Message-ID` is
supposed to be globally unique, but this fails in two distinct
ways. When you receive copies of a message via a mechanism like
:mailheader:`Cc` or via a mailing list, the copies are typically
interchangeable. In the case of some broken mail sending software, the
same :mailheader:`Message-ID` is used for completely unrelated
messages. The options :option:`search --duplicate` and :option:`show
--duplicate` options provide the user with control over which message
file is displayed. Front ends will need to provide their own
interface, see e.g. the Emacs front-end :any:`emacs-show-duplicates`.

ENVIRONMENT
===========

The following environment variables can be used to control the behavior
of notmuch.

.. envvar:: NOTMUCH_CONFIG

   Specifies the location of the notmuch configuration file. See
   :any:`notmuch-config(1)` for details.

.. envvar:: NOTMUCH_DATABASE

   Specifies the location of the notmuch database. See
   :any:`notmuch-config(1)` for details.

.. envvar:: NOTMUCH_PROFILE

   Selects among notmuch configurations. See :any:`notmuch-config(1)`
   for details.

.. envvar:: NOTMUCH_TALLOC_REPORT

   Location to write a talloc memory usage report. See
   **talloc\_enable\_leak\_report\_full** in :manpage:`talloc(3)` for more
   information.

.. envvar:: NOTMUCH_DEBUG_QUERY

   If set to a non-empty value, the notmuch library will print (to
   stderr) Xapian queries it constructs.

SEE ALSO
========

:any:`notmuch-address(1)`,
:any:`notmuch-compact(1)`,
:any:`notmuch-config(1)`,
:any:`notmuch-count(1)`,
:any:`notmuch-dump(1)`,
:any:`notmuch-hooks(5)`,
:any:`notmuch-insert(1)`,
:any:`notmuch-new(1)`,
:any:`notmuch-properties(7)`,
:any:`notmuch-reindex(1)`,
:any:`notmuch-reply(1)`,
:any:`notmuch-restore(1)`,
:any:`notmuch-search(1)`,
:any:`notmuch-search-terms(7)`,
:any:`notmuch-show(1)`,
:any:`notmuch-tag(1)`

The notmuch website: **https://notmuchmail.org**

CONTACT
=======

Feel free to send questions, comments, or kudos to the notmuch mailing
list <notmuch@notmuchmail.org> . Subscription is not required before
posting, but is available from the notmuchmail.org website.

Real-time interaction with the Notmuch community is available via IRC
(server: irc.libera.chat, channel: #notmuch).
