.. _notmuch-git(1):

===========
notmuch-git
===========

SYNOPSIS
========

**notmuch** **git** [-h] [-N] [-C *repo*] [-p *prefix*] [-v] [-l *log level*] *subcommand*

**nmbug** [-h] [-C *repo*] [-p *prefix*] [-v] [-l *log level*] *subcommand*

DESCRIPTION
===========

Manage notmuch tags with Git.

OPTIONS
-------

Supported options for `notmuch git` include

.. program:: notmuch-git

.. option::  -h, --help

   show help message and exit

.. option:: -N, --nmbug

   Set defaults for :option:`--tag-prefix` and :option:`--git-dir` suitable for the
   :any:`notmuch` bug tracker

.. option:: -C <repo>, --git-dir <repo>

   Operate on git repository *repo*. See :ref:`repo_location` for
   defaults.

.. option:: -p <prefix>, --tag-prefix <prefix>

   Operate only on tags with prefix *prefix*. See :ref:`prefix_val` for
   defaults.

.. option::   -v, --version

   show notmuch-git's version number and exit

.. option::   -l <level>, --log-level <level>

   Log verbosity, one of: `critical`, `error`, `warning`, `info`,
   `debug`. Defaults to `warning`.

SUBCOMMANDS
-----------

For help on a particular subcommand, run: 'notmuch-git ... <command> --help'.

.. program:: notmuch-git

.. option:: archive [tree-ish] [arg ...]

Dump a tar archive of a committed tag set using 'git archive'. See
:any:`format` for details of the archive contents.

   .. describe:: tree-ish

   The tree or commit to produce an archive for. Defaults to 'HEAD'.

   .. describe:: arg

   If present, any optional arguments are passed through to
   :manpage:`git-archive(1)`. Arguments to `git-archive` are reordered
   so that *tree-ish* comes last.

.. option:: checkout [-f|--force]

Update the notmuch database from Git.

This is mainly useful to discard your changes in notmuch relative
to Git.

   .. describe:: [-f|--force]

   Override checks that prevent modifying tags for large fractions of
   messages in the database. See also :nmconfig:`git.safe_fraction`.

.. option:: clone <repository>

Create a local `notmuch git` repository from a remote source.

This wraps 'git clone', adding some options to avoid creating a
working tree while preserving remote-tracking branches and
upstreams.

    .. describe:: repository

    The (possibly remote) repository to clone from. See the URLS
    section of :manpage:`git-clone(1)` for more information on
    specifying repositories.

.. option:: commit [-f|--force] [message]

Commit prefix-matching tags from the notmuch database to Git.

   .. describe:: message

   Optional text for the commit message.

   .. describe:: -f|--force

   Override checks that prevent modifying tags for large fractions of
   messages in the database. See also :nmconfig:`git.safe_fraction`.

.. option:: fetch [remote]

Fetch changes from the remote repository.

    .. describe:: remote

    Override the default configured in `branch.<name>.remote` to fetch
    from a particular remote repository (e.g. `origin`).

.. option:: help

Show brief help for an `notmuch git` command.

.. option:: init [--format-version=N]

Create an empty `notmuch git` repository.

This wraps 'git init' with a few extra steps to support subsequent
status and commit commands.

   .. describe:: --format-version=N

   Create a repo in format version N. By default :any:`notmuch-git`
   uses the highest supported version, which is the best choice for
   most use-cases.

.. option:: log [arg ...]

A wrapper for 'git log'.

   .. describe:: arg

   Additional arguments are passed through to 'git log'.

After running `notmuch git fetch`, you can inspect the changes with

::

   $ notmuch git log HEAD..@{upstream}

.. option:: merge [reference]

Merge changes from 'reference' into HEAD and load the result into notmuch.

   .. describe:: reference

   Reference, usually other branch heads, to merge into our
   branch. Defaults to `@{upstream}`.

.. option:: pull [repository] [refspec ...]

Pull (merge) remote repository changes to notmuch.

**pull** is equivalent to **fetch** followed by **merge**.  We use the
Git-configured repository for your current branch
(`branch.<name>.repository`, likely `origin`, and `branch.<name>.merge`,
likely `master` or `main`).

   .. describe:: repository

   The "remote" repository that is the source of the pull. This parameter
   can be either a URL (see the section GIT URLS in :manpage:`git-pull(1)`) or the
   name of a remote (see the section REMOTES in :manpage:`git-pull(1)`).

   .. describe:: refspec

   Refspec (usually a branch name) to fetch and merge. See the
   *refspec* entry in the OPTIONS section of :manpage:`git-pull(1`) for
   other possibilities.

.. option:: push [repository] [refspec]

Push the local `notmuch git` Git state to a remote repository.

    .. describe::  repository

    The "remote" repository that is the destination of the push. This
    parameter can be either a URL (see the section GIT URLS in
    :manpage:`git-push(1)`) or the name of a remote (see the section
    REMOTES in :manpage:`git-push(1)`).

    .. describe:: refspec

    Refspec (usually a branch name) to push. See the *refspec* entry in the OPTIONS section of
    :manpage:`git-push(1)` for other possibilities.

.. option:: status

Show pending updates in notmuch or git repo.

Prints lines of the form

|  ng Message-Id tag

where n is a single character representing notmuch database status

   .. describe:: A

   Tag is present in notmuch database, but not committed to nmbug
   (equivalently, tag has been deleted in nmbug repo, e.g. by a
   pull, but not restored to notmuch database).

   .. describe:: D

   Tag is present in nmbug repo, but not restored to notmuch
   database (equivalently, tag has been deleted in notmuch).

   .. describe:: U

   Message is unknown (missing from local notmuch database).

The second character *g* (if present) represents a difference between
local and upstream branches. Typically `notmuch git fetch` needs to be
run to update this.

   .. describe:: a

   Tag is present in upstream, but not in the local Git branch.

   .. describe:: d

   Tag is present in local Git branch, but not upstream.

.. _format:

REPOSITORY CONTENTS
===================

The tags are stored in the git repo (and exported) as a set of empty
files. These empty files are contained within a directory named after
the message-id.

In what follows `encode()` represents a POSIX filesystem safe
encoding. The encoding preserves alphanumerics, and the characters
`+-_@=.,:`.  All other octets are replaced with `%` followed by a two
digit hex number.

Currently :any:`notmuch-git` can read any format version, but can only
create (via :any:`init`) :ref:`version 1 <format_version_1>` repositories.

.. _format_version_0:

Version 0
---------

This is the legacy format created by the `nmbug` tool prior to release
0.37.  For a message with Message-Id *id*, for each tag *tag*, there
is an empty file with path

       tags/ `encode` (*id*) / `encode` (*tag*)

.. _format_version_1:

Version 1
---------

In format version 1 and later, the format version is contained in a
top level file called FORMAT.

For a message with Message-Id *id*, for each tag *tag*, there
is an empty file with path

       tags/ `hash1` (*id*) / `hash2` (*id*) `encode` (*id*) / `encode` (*tag*)

The hash functions each represent one byte of the `blake2b` hex
digest.

Compared to :ref:`version 0 <format_version_0>`, this reduces the
number of subdirectories within each directory.

.. _repo_location:

REPOSITORY LOCATION
===================

:any:`notmuch-git` uses the first of the following with a non-empty
value to locate the git repository.

- Option :option:`--git-dir`.

- Environment variable :envvar:`NOTMUCH_GIT_DIR`.

- Configuration item :nmconfig:`git.path`

- If invoked as `nmbug` or with the :option:`--nmbug` option,
  :code:`$HOME/.nmbug`; otherwise
  :code:`$XDG_DATA_HOME/notmuch/$NOTMUCH_PROFILE/git`.

.. _prefix_val:

PREFIX VALUE
============

:any:`notmuch-git` uses the first of the following with a non-null
value to define the tag prefix.

- Option :option:`--tag-prefix`.

- Environment variable :envvar:`NOTMUCH_GIT_PREFIX`.

- Configuration item :nmconfig:`git.tag_prefix`.

- If invoked as `nmbug` or with the :option:`--nmbug` option,
  :code:`notmuch::`, otherwise the empty string.

ENVIRONMENT
===========

Variable :envvar:`NOTMUCH_PROFILE` influences :ref:`repo_location`.
If it is unset, 'default' is assumed.

.. envvar:: NOTMUCH_GIT_DIR

   Default location of git repository. Overriden by :option:`--git-dir`.

.. envvar:: NOTMUCH_GIT_PREFIX

   Default tag prefix (filter). Overriden by :option:`--tag-prefix`.

SEE ALSO
========

:any:`notmuch(1)`,
:any:`notmuch-dump(1)`,
:any:`notmuch-restore(1)`,
:any:`notmuch-tag(1)`
