.. _git-remote-notmuch(1):

==================
git-remote-notmuch
==================

Description
===========

git-remote-notmuch is a git remote helper to push and pull from
notmuch databases on the local machine. This remote helper handles
URIs prefixed with `notmuch:`. The remote helper is not usually
invoked directly by users, but indirectly by configuring a remote with
an appropriate URI (see :ref:`uri_format`, below) or by fetching or
pushing directly to such a URI.

Configuration
=============

This remote helper is configured via :any:`notmuch-config(1)` options.

Git needs an email address and name to construct a commit. This remote
helper uses the values proved by :nmconfig:`user.primary_email` and
:nmconfig:`user.name` respectively.

The handling of missing messages (messages referenced in the git
repository but not found in the database) is controlled by
:nmconfig:`git.fail_on_missing`.

The file system layout of the repository is influenced by
:nmconfig:`git.metadata_prefix`.

This remote helper ignores any commits that do not match
:nmconfig:`git.ref`

.. _uri_format:

URI format
==========

This remote helper supports the following forms of URI.

``notmuch://``
  The default notmuch database, located as documented
  in ``notmuch-config(1)``

``notmuch://?config=notmuch-config``
  Use configuration file ``notmuch-config``

``notmuch://?profile=default``
  Use profile ``default``

``notmuch://?config=notmuch-config&profile=default``
  Combine previous two.

``notmuch://home/blub/mail``
  Use the database / mail_root at ``/home/blub/mail``

``notmuch://home/blub/mail/?config=/home/blub/notmuch-config``
  Use the database / mail_root at ``/home/blub/mail``,
  with the config file specified.

``notmuch://home/blub/mail/?config=/home/blub/notmuch-config&profile=default``
  Set path, config file and profile.

``notmuch://?path=/home/blub/mail&config=notmuch-config&profile=default``
  As previous, but using query format for all parameters.

The prefix ``notmuch::`` can be substituted for ``notmuch://`` in all of the above and is essentially equivalent. See ``gitremote-helpers(1)`` for details.

Examples
========

For these examples, assume shell variable ``TAG_FILE`` is set
as (cf. :any:`repository_format`)::

    TAG_FILE="_notmuch_metadata/87/b1/4EFC743A.3060609@april.org/tags"

Add remote to an existing repo::

    cd repo
    git remote add database notmuch::
    git fetch database
    git merge database/master

Restore database state using push::

    git clone notmuch:: repo
    notmuch tag +foo id:bar
    git -C repo push origin master

Remove all tags on a single message via push::

    git clone notmuch:: repo && cd repo
    cp /dev/null $TAG_FILE
    git add $TAG_FILE
    git commit -m 'testing push'
    git push origin master

Recording database state to Git::

    git clone notmuch:: repo && cd repo
    cat $TAG_FILE
    > inbox
    > unread
    notmuch tag +zznew -- id:4EFC743A.3060609@april.org
    git pull
    cat $TAG_FILE
    > inbox
    > unread
    > zznew

.. _repository_format:

Repository format
=================

Work Tree
---------

Metadata (currently only tags) is stored under the path defined by the
option :nmconfig:`git.metadata_prefix` (by default `_notmuch_metadata`).
It is supported (and useful) to keep other files in the same
repository, outside this prefix. Such files will be ignored by
:any:`git-remote-notmuch(1)`.

Inside the prefix directory, there is a directory corresponding to
each message-id in the notmuch database. The path of this directory is
obtained by taking the first two bytes of the SHA1 hash of the
message-id, followed by the hex escaped (in the manner documented in
:any:`notmuch-dump(1)`) message name. For example, the message-id ``discourse/post/15986@community.mnt.re`` corresponds to directory::

      _notmuch_metadata/ae/27/discourse%2fpost%2f15986@community.mnt.re

Inside each such directory is a (possibly empty) file containing the tags for that message, one per line.

Repository Internals
--------------------

There is currently one file inside ``.git`` used by this remote
helper. The file ``.git/notmuch/lastmod`` stores the UUID and lastmod
counter of the most recent fetch of the database. This should match
the output of ``notmuch count --lastmod`` if the git repository and
the database are synchronized (but is not updated by git operations
not involving this remote helper).

Environment variables
=====================

.. envvar:: GIT_DIR

   Normally set by ``git``.

.. envvar:: GIT_REMOTE_NM_LOG

   If set, log debugging information to the named file.

.. envvar:: GIT_REMOTE_NM_DEBUG

   A string of single character flags to enable extra debugging. These are subject to change and should not be relied upon by e.g. scripts.

   ``d``
     Extra output about deletions
   ``m``
     Extra output about missing messages
   ``s``
     Sort output, for reproducibility

See also
========

:manpage:`git(1)`,
:manpage:`gitremote-helpers(1)`,
:any:`notmuch-config(1)`
:any:`notmuch-dump(1)`
:any:`notmuch-restore(1)`

