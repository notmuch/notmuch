#!/usr/bin/env python3
#
# Copyright (c) 2011-2014 David Bremner <david@tethera.net>
#                         W. Trevor King <wking@tremily.us>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see https://www.gnu.org/licenses/ .

"""
Manage notmuch tags with Git
"""

from __future__ import print_function
from __future__ import unicode_literals

import codecs as _codecs
import collections as _collections
import functools as _functools
import inspect as _inspect
import locale as _locale
import logging as _logging
import os as _os
import re as _re
import shutil as _shutil
import subprocess as _subprocess
import sys as _sys
import tempfile as _tempfile
import textwrap as _textwrap
from urllib.parse import quote as _quote
from urllib.parse import unquote as _unquote
import json as _json

_LOG = _logging.getLogger('notmuch-git')
_LOG.setLevel(_logging.WARNING)
_LOG.addHandler(_logging.StreamHandler())

NOTMUCH_GIT_DIR = None
TAG_PREFIX = None
FORMAT_VERSION = 1

_HEX_ESCAPE_REGEX = _re.compile('%[0-9A-F]{2}')
_TAG_DIRECTORY = 'tags/'
_TAG_FILE_REGEX = ( _re.compile(_TAG_DIRECTORY + '(?P<id>[^/]*)/(?P<tag>[^/]*)'),
                    _re.compile(_TAG_DIRECTORY + '([0-9a-f]{2}/){2}(?P<id>[^/]*)/(?P<tag>[^/]*)'))

# magic hash for Git (git hash-object -t blob /dev/null)
_EMPTYBLOB = 'e69de29bb2d1d6434b8b29ae775ad8c2e48c5391'

def _hex_quote(string, safe='+@=:,'):
    """
    quote('abc def') -> 'abc%20def'.

    Wrap urllib.parse.quote with additional safe characters (in
    addition to letters, digits, and '_.-') and lowercase hex digits
    (e.g. '%3a' instead of '%3A').
    """
    uppercase_escapes = _quote(string, safe)
    return _HEX_ESCAPE_REGEX.sub(
        lambda match: match.group(0).lower(),
        uppercase_escapes)

def _xapian_quote(string):
    """
    Quote a string for Xapian's QueryParser.

    Xapian uses double-quotes for quoting strings.  You can escape
    internal quotes by repeating them [1,2,3].

    [1]: https://trac.xapian.org/ticket/128#comment:2
    [2]: https://trac.xapian.org/ticket/128#comment:17
    [3]: https://trac.xapian.org/changeset/13823/svn
    """
    return '"{0}"'.format(string.replace('"', '""'))


def _xapian_unquote(string):
    """
    Unquote a Xapian-quoted string.
    """
    if string.startswith('"') and string.endswith('"'):
        return string[1:-1].replace('""', '"')
    return string


def timed(fn):
    """Timer decorator"""
    from time import perf_counter

    def inner(*args, **kwargs):
        start_time = perf_counter()
        rval = fn(*args, **kwargs)
        end_time = perf_counter()
        _LOG.info('{0}: {1:.8f}s elapsed'.format(fn.__name__, end_time - start_time))
        return rval

    return inner


class SubprocessError(RuntimeError):
    "A subprocess exited with a nonzero status"
    def __init__(self, args, status, stdout=None, stderr=None):
        self.status = status
        self.stdout = stdout
        self.stderr = stderr
        msg = '{args} exited with {status}'.format(args=args, status=status)
        if stderr:
            msg = '{msg}: {stderr}'.format(msg=msg, stderr=stderr)
        super(SubprocessError, self).__init__(msg)


class _SubprocessContextManager(object):
    """
    PEP 343 context manager for subprocesses.

    'expect' holds a tuple of acceptable exit codes, otherwise we'll
    raise a SubprocessError in __exit__.
    """
    def __init__(self, process, args, expect=(0,)):
        self._process = process
        self._args = args
        self._expect = expect

    def __enter__(self):
        return self._process

    def __exit__(self, type, value, traceback):
        for name in ['stdin', 'stdout', 'stderr']:
            stream = getattr(self._process, name)
            if stream:
                stream.close()
                setattr(self._process, name, None)
        status = self._process.wait()
        _LOG.debug(
            'collect {args} with status {status} (expected {expect})'.format(
                args=self._args, status=status, expect=self._expect))
        if status not in self._expect:
            raise SubprocessError(args=self._args, status=status)

    def wait(self):
        return self._process.wait()


def _spawn(args, input=None, additional_env=None, wait=False, stdin=None,
           stdout=None, stderr=None, encoding=_locale.getpreferredencoding(),
           expect=(0,), **kwargs):
    """Spawn a subprocess, and optionally wait for it to finish.

    This wrapper around subprocess.Popen has two modes, depending on
    the truthiness of 'wait'.  If 'wait' is true, we use p.communicate
    internally to write 'input' to the subprocess's stdin and read
    from it's stdout/stderr.  If 'wait' is False, we return a
    _SubprocessContextManager instance for fancier handling
    (e.g. piping between processes).

    For 'wait' calls when you want to write to the subprocess's stdin,
    you only need to set 'input' to your content.  When 'input' is not
    None but 'stdin' is, we'll automatically set 'stdin' to PIPE
    before calling Popen.  This avoids having the subprocess
    accidentally inherit the launching process's stdin.
    """
    _LOG.debug('spawn {args} (additional env. var.: {env})'.format(
        args=args, env=additional_env))
    if not stdin and input is not None:
        stdin = _subprocess.PIPE
    if additional_env:
        if not kwargs.get('env'):
            kwargs['env'] = dict(_os.environ)
        kwargs['env'].update(additional_env)
    p = _subprocess.Popen(
        args, stdin=stdin, stdout=stdout, stderr=stderr, **kwargs)
    if wait:
        if hasattr(input, 'encode'):
            input = input.encode(encoding)
        (stdout, stderr) = p.communicate(input=input)
        status = p.wait()
        _LOG.debug(
            'collect {args} with status {status} (expected {expect})'.format(
                args=args, status=status, expect=expect))
        if stdout is not None:
            stdout = stdout.decode(encoding)
        if stderr is not None:
            stderr = stderr.decode(encoding)
        if status not in expect:
            raise SubprocessError(
                args=args, status=status, stdout=stdout, stderr=stderr)
        return (status, stdout, stderr)
    if p.stdin and not stdin:
        p.stdin.close()
        p.stdin = None
    if p.stdin:
        p.stdin = _codecs.getwriter(encoding=encoding)(stream=p.stdin)
    stream_reader = _codecs.getreader(encoding=encoding)
    if p.stdout:
        p.stdout = stream_reader(stream=p.stdout)
    if p.stderr:
        p.stderr = stream_reader(stream=p.stderr)
    return _SubprocessContextManager(args=args, process=p, expect=expect)


def _git(args, **kwargs):
    args = ['git', '--git-dir', NOTMUCH_GIT_DIR] + list(args)
    return _spawn(args=args, **kwargs)


def _get_current_branch():
    """Get the name of the current branch.

    Return 'None' if we're not on a branch.
    """
    try:
        (status, branch, stderr) = _git(
            args=['symbolic-ref', '--short', 'HEAD'],
            stdout=_subprocess.PIPE, stderr=_subprocess.PIPE, wait=True)
    except SubprocessError as e:
        if 'not a symbolic ref' in e:
            return None
        raise
    return branch.strip()


def _get_remote():
    "Get the default remote for the current branch."
    local_branch = _get_current_branch()
    (status, remote, stderr) = _git(
        args=['config', 'branch.{0}.remote'.format(local_branch)],
        stdout=_subprocess.PIPE, wait=True)
    return remote.strip()

def _tag_query(prefix=None):
    if prefix is None:
        prefix = TAG_PREFIX
    return '(tag (starts-with "{:s}"))'.format(prefix.replace('"','\\\"'))

def count_messages(prefix=None):
    "count messages with a given prefix."
    (status, stdout, stderr) = _spawn(
        args=['notmuch', 'count', '--query=sexp', _tag_query(prefix)],
        stdout=_subprocess.PIPE, wait=True)
    if status != 0:
        _LOG.error("failed to run notmuch config")
        sys.exit(1)
    return int(stdout.rstrip())

def get_tags(prefix=None):
    "Get a list of tags with a given prefix."
    (status, stdout, stderr) = _spawn(
        args=['notmuch', 'search', '--query=sexp', '--output=tags', _tag_query(prefix)],
        stdout=_subprocess.PIPE, wait=True)
    return [tag for tag in stdout.splitlines()]

def archive(treeish='HEAD', args=()):
    """
    Dump a tar archive of the current notmuch-git tag set.

    Using 'git archive'.

    Each tag $tag for message with Message-Id $id is written to
    an empty file

      tags/hash1(id)/hash2(id)/encode($id)/encode($tag)

    The encoding preserves alphanumerics, and the characters
    "+-_@=.:," (not the quotes).  All other octets are replaced with
    '%' followed by a two digit hex number.
    """
    _git(args=['archive', treeish] + list(args), wait=True)


def clone(repository):
    """
    Create a local notmuch-git repository from a remote source.

    This wraps 'git clone', adding some options to avoid creating a
    working tree while preserving remote-tracking branches and
    upstreams.
    """
    with _tempfile.TemporaryDirectory(prefix='notmuch-git-clone.') as workdir:
        _spawn(
            args=[
                'git', 'clone', '--no-checkout', '--separate-git-dir', NOTMUCH_GIT_DIR,
                repository, workdir],
            wait=True)
    _git(args=['config', '--unset', 'core.worktree'], wait=True, expect=(0, 5))
    _git(args=['config', 'core.bare', 'true'], wait=True)
    (status, stdout, stderr) = _git(args=['show-ref', '--verify',
                                          '--quiet',
                                          'refs/remotes/origin/config'],
                                    expect=(0,1),
                                    wait=True)
    if status == 0:
        _git(args=['branch', 'config', 'origin/config'], wait=True)
    existing_tags = get_tags()
    if existing_tags:
        _LOG.warning(
            'Not checking out to avoid clobbering existing tags: {}'.format(
            ', '.join(existing_tags)))
    else:
        checkout()


def _is_committed(status):
    return len(status['added']) + len(status['deleted']) == 0


class CachedIndex:
    def __init__(self, repo, treeish):
        self.cache_path = _os.path.join(repo, 'notmuch', 'index_cache.json')
        self.index_path = _os.path.join(repo, 'index')
        self.current_treeish = treeish
        # cached values
        self.treeish = None
        self.hash = None
        self.index_checksum = None

        self._load_cache_file()

    def _load_cache_file(self):
        try:
            with open(self.cache_path) as f:
                data = _json.load(f)
                self.treeish = data['treeish']
                self.hash = data['hash']
                self.index_checksum = data['index_checksum']
        except FileNotFoundError:
            pass
        except _json.JSONDecodeError:
            _LOG.error("Error decoding cache")
            _sys.exit(1)

    def __enter__(self):
        self.read_tree()
        return self

    def __exit__(self, type, value, traceback):
        checksum = _read_index_checksum(self.index_path)
        (_, hash, _) = _git(
            args=['rev-parse', self.current_treeish],
            stdout=_subprocess.PIPE,
            wait=True)

        with open(self.cache_path, "w") as f:
            _json.dump({'treeish': self.current_treeish,
                        'hash': hash.rstrip(),  'index_checksum': checksum }, f)

    @timed
    def read_tree(self):
        current_checksum = _read_index_checksum(self.index_path)
        (_, hash, _) = _git(
            args=['rev-parse', self.current_treeish],
            stdout=_subprocess.PIPE,
            wait=True)
        current_hash = hash.rstrip()

        if self.current_treeish == self.treeish and \
           self.index_checksum and self.index_checksum == current_checksum and \
           self.hash and self.hash == current_hash:
            return

        _git(args=['read-tree', self.current_treeish], wait=True)


def check_safe_fraction(status):
    safe = 0.1
    conf = _notmuch_config_get ('git.safe_fraction')
    if conf and conf != '':
        safe=float(conf)

    total = count_messages (TAG_PREFIX)
    if total == 0:
        _LOG.error('No existing tags with given prefix, stopping.'.format(safe))
        _LOG.error('Use --force to override.')
        exit(1)
    change = len(status['added'])+len(status['deleted'])
    fraction = change/total
    _LOG.debug('total messages {:d}, change: {:d}, fraction: {:f}'.format(total,change,fraction))
    if fraction > safe:
        _LOG.error('safe fraction {:f} exceeded, stopping.'.format(safe))
        _LOG.error('Use --force to override or reconfigure git.safe_fraction.')
        exit(1)

def commit(treeish='HEAD', message=None, force=False):
    """
    Commit prefix-matching tags from the notmuch database to Git.
    """

    status = get_status()

    if _is_committed(status=status):
        _LOG.warning('Nothing to commit')
        return

    if not force:
        check_safe_fraction (status)

    with CachedIndex(NOTMUCH_GIT_DIR, treeish) as index:
        try:
            _update_index(status=status)
            (_, tree, _) = _git(
                args=['write-tree'],
                stdout=_subprocess.PIPE,
                wait=True)
            (_, parent, _) = _git(
                args=['rev-parse', treeish],
                stdout=_subprocess.PIPE,
                wait=True)
            (_, commit, _) = _git(
                args=['commit-tree', tree.strip(), '-p', parent.strip()],
                input=message,
                stdout=_subprocess.PIPE,
                wait=True)
            _git(
                args=['update-ref', treeish, commit.strip()],
                stdout=_subprocess.PIPE,
                wait=True)
        except Exception as e:
            _git(args=['read-tree', '--empty'], wait=True)
            _git(args=['read-tree', treeish], wait=True)
            raise

@timed
def _update_index(status):
    with _git(
            args=['update-index', '--index-info'],
            stdin=_subprocess.PIPE) as p:
        for id, tags in status['deleted'].items():
            for line in _index_tags_for_message(id=id, status='D', tags=tags):
                p.stdin.write(line)
        for id, tags in status['added'].items():
            for line in _index_tags_for_message(id=id, status='A', tags=tags):
                p.stdin.write(line)


def fetch(remote=None):
    """
    Fetch changes from the remote repository.

    See 'merge' to bring those changes into notmuch.
    """
    args = ['fetch']
    if remote:
        args.append(remote)
    _git(args=args, wait=True)


def init(remote=None,format_version=None):
    """
    Create an empty notmuch-git repository.

    This wraps 'git init' with a few extra steps to support subsequent
    status and commit commands.
    """
    from pathlib import Path
    parent = Path(NOTMUCH_GIT_DIR).parent
    try:
        _os.makedirs(parent)
    except FileExistsError:
        pass

    if not format_version:
        format_version = 1

    format_version=int(format_version)

    if format_version > 1 or format_version < 0:
        _LOG.error("Illegal format version {:d}".format(format_version))
        _sys.exit(1)

    _spawn(args=['git', '--git-dir', NOTMUCH_GIT_DIR, 'init',
                 '--initial-branch=master', '--quiet', '--bare'], wait=True)
    _git(args=['config', 'core.logallrefupdates', 'true'], wait=True)
    # create an empty blob (e69de29bb2d1d6434b8b29ae775ad8c2e48c5391)
    _git(args=['hash-object', '-w', '--stdin'], input='', wait=True)
    allow_empty=('--allow-empty',)
    if format_version >= 1:
        allow_empty=()
        # create a blob for the FORMAT file
        (status, stdout, _) = _git(args=['hash-object', '-w', '--stdin'], stdout=_subprocess.PIPE,
                                   input='{:d}\n'.format(format_version), wait=True)
        verhash=stdout.rstrip()
        _LOG.debug('hash of FORMAT blob = {:s}'.format(verhash))
        # Add FORMAT to the index
        _git(args=['update-index', '--add', '--cacheinfo', '100644,{:s},FORMAT'.format(verhash)], wait=True)

    _git(
        args=[
            'commit', *allow_empty, '-m', 'Start a new notmuch-git repository'
        ],
        additional_env={'GIT_WORK_TREE': NOTMUCH_GIT_DIR},
        wait=True)


def checkout(force=None):
    """
    Update the notmuch database from Git.

    This is mainly useful to discard your changes in notmuch relative
    to Git.
    """
    status = get_status()

    if not force:
        check_safe_fraction(status)

    with _spawn(
            args=['notmuch', 'tag', '--batch'], stdin=_subprocess.PIPE) as p:
        for id, tags in status['added'].items():
            p.stdin.write(_batch_line(action='-', id=id, tags=tags))
        for id, tags in status['deleted'].items():
            p.stdin.write(_batch_line(action='+', id=id, tags=tags))


def _batch_line(action, id, tags):
    """
    'notmuch tag --batch' line for adding/removing tags.

    Set 'action' to '-' to remove a tag or '+' to add the tags to a
    given message id.
    """
    tag_string = ' '.join(
        '{action}{prefix}{tag}'.format(
            action=action, prefix=_ENCODED_TAG_PREFIX, tag=_hex_quote(tag))
        for tag in tags)
    line = '{tags} -- id:{id}\n'.format(
        tags=tag_string, id=_xapian_quote(string=id))
    return line


def _insist_committed():
    "Die if the the notmuch tags don't match the current HEAD."
    status = get_status()
    if not _is_committed(status=status):
        _LOG.error('\n'.join([
            'Uncommitted changes to {prefix}* tags in notmuch',
            '',
            "For a summary of changes, run 'notmuch-git status'",
            "To save your changes,     run 'notmuch-git commit' before merging/pull",
            "To discard your changes,  run 'notmuch-git checkout'",
            ]).format(prefix=TAG_PREFIX))
        _sys.exit(1)


def pull(repository=None, refspecs=None):
    """
    Pull (merge) remote repository changes to notmuch.

    'pull' is equivalent to 'fetch' followed by 'merge'.  We use the
    Git-configured repository for your current branch
    (branch.<name>.repository, likely 'origin', and
    branch.<name>.merge, likely 'master').
    """
    _insist_committed()
    if refspecs and not repository:
        repository = _get_remote()
    args = ['pull']
    if repository:
        args.append(repository)
    if refspecs:
        args.extend(refspecs)
    with _tempfile.TemporaryDirectory(prefix='notmuch-git-pull.') as workdir:
        for command in [
                ['reset', '--hard'],
                args]:
            _git(
                args=command,
                additional_env={'GIT_WORK_TREE': workdir},
                wait=True)
    checkout()


def merge(reference='@{upstream}'):
    """
    Merge changes from 'reference' into HEAD and load the result into notmuch.

    The default reference is '@{upstream}'.
    """
    _insist_committed()
    with _tempfile.TemporaryDirectory(prefix='notmuch-git-merge.') as workdir:
        for command in [
                ['reset', '--hard'],
                ['merge', reference]]:
            _git(
                args=command,
                additional_env={'GIT_WORK_TREE': workdir},
                wait=True)
    checkout()


def log(args=()):
    """
    A simple wrapper for 'git log'.

    After running 'notmuch-git fetch', you can inspect the changes with
    'notmuch-git log HEAD..@{upstream}'.
    """
    # we don't want output trapping here, because we want the pager.
    args = ['log', '--name-status', '--no-renames'] + list(args)
    with _git(args=args, expect=(0, 1, -13)) as p:
        p.wait()


def push(repository=None, refspecs=None):
    "Push the local notmuch-git Git state to a remote repository."
    if refspecs and not repository:
        repository = _get_remote()
    args = ['push']
    if repository:
        args.append(repository)
    if refspecs:
        args.extend(refspecs)
    _git(args=args, wait=True)


def status():
    """
    Show pending updates in notmuch or git repo.

    Prints lines of the form

      ng Message-Id tag

    where n is a single character representing notmuch database status

    * A

      Tag is present in notmuch database, but not committed to notmuch-git
      (equivalently, tag has been deleted in notmuch-git repo, e.g. by a
      pull, but not restored to notmuch database).

    * D

      Tag is present in notmuch-git repo, but not restored to notmuch
      database (equivalently, tag has been deleted in notmuch).

    * U

      Message is unknown (missing from local notmuch database).

    The second character (if present) represents a difference between
    local and upstream branches. Typically 'notmuch-git fetch' needs to be
    run to update this.

    * a

      Tag is present in upstream, but not in the local Git branch.

    * d

      Tag is present in local Git branch, but not upstream.
    """
    status = get_status()
    # 'output' is a nested defaultdict for message status:
    # * The outer dict is keyed by message id.
    # * The inner dict is keyed by tag name.
    # * The inner dict values are status strings (' a', 'Dd', ...).
    output = _collections.defaultdict(
        lambda : _collections.defaultdict(lambda : ' '))
    for id, tags in status['added'].items():
        for tag in tags:
            output[id][tag] = 'A'
    for id, tags in status['deleted'].items():
        for tag in tags:
            output[id][tag] = 'D'
    for id, tags in status['missing'].items():
        for tag in tags:
            output[id][tag] = 'U'
    if _is_unmerged():
        for id, tag in _diff_refs(filter='A'):
            output[id][tag] += 'a'
        for id, tag in _diff_refs(filter='D'):
            output[id][tag] += 'd'
    for id, tag_status in sorted(output.items()):
        for tag, status in sorted(tag_status.items()):
            print('{status}\t{id}\t{tag}'.format(
                status=status, id=id, tag=tag))


def _is_unmerged(ref='@{upstream}'):
    try:
        (status, fetch_head, stderr) = _git(
            args=['rev-parse', ref],
            stdout=_subprocess.PIPE, stderr=_subprocess.PIPE, wait=True)
    except SubprocessError as e:
        if 'No upstream configured' in e.stderr:
            return
        raise
    (status, base, stderr) = _git(
        args=['merge-base', 'HEAD', ref],
        stdout=_subprocess.PIPE, wait=True)
    return base != fetch_head

class DatabaseCache:
    def __init__(self):
        try:
            from notmuch2 import Database
            self._notmuch = Database()
        except ImportError:
            self._notmuch = None
        self._known = {}

    def known(self,id):
        if id in self._known:
            return self._known[id];

        if self._notmuch:
            try:
                _ = self._notmuch.find(id)
                self._known[id] = True
            except LookupError:
                self._known[id] = False
        else:
            (_, stdout, stderr) = _spawn(
                args=['notmuch', 'search', '--output=files', 'id:{0}'.format(id)],
                stdout=_subprocess.PIPE,
                wait=True)
            self._known[id] = stdout != None
        return self._known[id]

@timed
def get_status():
    status = {
        'deleted': {},
        'missing': {},
        }
    db = DatabaseCache()
    with PrivateIndex(repo=NOTMUCH_GIT_DIR, prefix=TAG_PREFIX) as index:
        maybe_deleted = index.diff(filter='D')
        for id, tags in maybe_deleted.items():
            if db.known(id):
                status['deleted'][id] = tags
            else:
                status['missing'][id] = tags
        status['added'] = index.diff(filter='A')

    return status

class PrivateIndex:
    def __init__(self, repo, prefix):
        try:
            _os.makedirs(_os.path.join(repo, 'notmuch'))
        except FileExistsError:
            pass

        file_name = 'notmuch/index'
        self.index_path = _os.path.join(repo, file_name)
        self.cache_path = _os.path.join(repo, 'notmuch', '{:s}.json'.format(_hex_quote(file_name)))

        self.current_prefix = prefix

        self.prefix = None
        self.uuid = None
        self.lastmod = None
        self.checksum = None
        self._load_cache_file()
        self.file_tree = None
        self._index_tags()

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        checksum = _read_index_checksum(self.index_path)
        (count, uuid, lastmod) = _read_database_lastmod()
        with open(self.cache_path, "w") as f:
            _json.dump({'prefix': self.current_prefix, 'uuid': uuid, 'lastmod': lastmod,  'checksum': checksum }, f)

    def _load_cache_file(self):
        try:
            with open(self.cache_path) as f:
                data = _json.load(f)
                self.prefix = data['prefix']
                self.uuid = data['uuid']
                self.lastmod = data['lastmod']
                self.checksum = data['checksum']
        except FileNotFoundError:
            return None
        except _json.JSONDecodeError:
            _LOG.error("Error decoding cache")
            _sys.exit(1)

    @timed
    def _read_file_tree(self):
        self.file_tree = {}

        with _git(
                args=['ls-files', 'tags'],
                additional_env={'GIT_INDEX_FILE': self.index_path},
                stdout=_subprocess.PIPE) as git:
            for file in git.stdout:
                dir=_os.path.dirname(file)
                tag=_os.path.basename(file).rstrip()
                if dir not in self.file_tree:
                    self.file_tree[dir]=[tag]
                else:
                    self.file_tree[dir].append(tag)


    def _clear_tags_for_message(self, id):
        """
        Clear any existing index entries for message 'id'

        Neither 'id' nor the tags in 'tags' should be encoded/escaped.
        """

        if self.file_tree == None:
            self._read_file_tree()

        dir = _id_path(id)

        if dir not in self.file_tree:
            return

        for file in self.file_tree[dir]:
            line = '0 0000000000000000000000000000000000000000\t{:s}/{:s}\n'.format(dir,file)
            yield line


    @timed
    def _index_tags(self):
        "Write notmuch tags to private git index."
        prefix = '+{0}'.format(_ENCODED_TAG_PREFIX)
        current_checksum = _read_index_checksum(self.index_path)
        if (self.prefix == None or self.prefix != self.current_prefix
            or self.checksum == None or self.checksum != current_checksum):
            _git(
                args=['read-tree', '--empty'],
                additional_env={'GIT_INDEX_FILE': self.index_path}, wait=True)

        query = _tag_query()
        clear_tags = False
        (count,uuid,lastmod) = _read_database_lastmod()
        if self.prefix == self.current_prefix and self.uuid \
           and self.uuid == uuid and self.checksum == current_checksum:
            query = '(and (infix "lastmod:{:d}..")) {:s})'.format(self.lastmod+1, query)
            clear_tags = True
        with _spawn(
                args=['notmuch', 'dump', '--format=batch-tag', '--query=sexp', '--', query],
                stdout=_subprocess.PIPE) as notmuch:
            with _git(
                    args=['update-index', '--index-info'],
                    stdin=_subprocess.PIPE,
                    additional_env={'GIT_INDEX_FILE': self.index_path}) as git:
                for line in notmuch.stdout:
                    if line.strip().startswith('#'):
                        continue
                    (tags_string, id) = [_.strip() for _ in line.split(' -- id:')]
                    tags = [
                        _unquote(tag[len(prefix):])
                        for tag in tags_string.split()
                        if tag.startswith(prefix)]
                    id = _xapian_unquote(string=id)
                    if clear_tags:
                        for line in self._clear_tags_for_message(id=id):
                            git.stdin.write(line)
                    for line in _index_tags_for_message(
                            id=id, status='A', tags=tags):
                        git.stdin.write(line)

    @timed
    def diff(self, filter):
        """
        Get an {id: {tag, ...}} dict for a given filter.

        For example, use 'A' to find added tags, and 'D' to find deleted tags.
        """
        s = _collections.defaultdict(set)
        with _git(
                args=[
                    'diff-index', '--cached', '--diff-filter', filter,
                    '--name-only', 'HEAD'],
                additional_env={'GIT_INDEX_FILE': self.index_path},
                stdout=_subprocess.PIPE) as p:
            # Once we drop Python < 3.3, we can use 'yield from' here
            for id, tag in _unpack_diff_lines(stream=p.stdout):
                s[id].add(tag)
        return s

def _read_index_checksum (index_path):
    """Read the index checksum, as defined by index-format.txt in the git source
    WARNING: assumes SHA1 repo"""
    import binascii
    try:
        with open(index_path, 'rb') as f:
            size=_os.path.getsize(index_path)
            f.seek(size-20);
            return binascii.hexlify(f.read(20)).decode('ascii')
    except FileNotFoundError:
        return None

def _read_database_lastmod():
    with _spawn(
            args=['notmuch', 'count', '--lastmod', '*'],
            stdout=_subprocess.PIPE) as notmuch:
        (count,uuid,lastmod_str) = notmuch.stdout.readline().split()
        return (count,uuid,int(lastmod_str))

def _id_path(id):
    hid=_hex_quote(string=id)
    from hashlib import blake2b

    if FORMAT_VERSION==0:
        return 'tags/{hid}'.format(hid=hid)
    elif FORMAT_VERSION==1:
        idhash = blake2b(hid.encode('utf8'), digest_size=2).hexdigest()
        return 'tags/{dir1}/{dir2}/{hid}'.format(
            hid=hid,
            dir1=idhash[0:2],dir2=idhash[2:])
    else:
        _LOG.error("Unknown format version",FORMAT_VERSION)
        _sys.exit(1)

def _index_tags_for_message(id, status, tags):
    """
    Update the Git index to either create or delete an empty file.

    Neither 'id' nor the tags in 'tags' should be encoded/escaped.
    """
    mode = '100644'
    hash = _EMPTYBLOB

    if status == 'D':
        mode = '0'
        hash = '0000000000000000000000000000000000000000'

    for tag in tags:
        path = '{ipath}/{tag}'.format(ipath=_id_path(id),tag=_hex_quote(string=tag))
        yield '{mode} {hash}\t{path}\n'.format(mode=mode, hash=hash, path=path)


def _diff_refs(filter, a='HEAD', b='@{upstream}'):
    with _git(
            args=['diff', '--diff-filter', filter, '--name-only', a, b],
            stdout=_subprocess.PIPE) as p:
        # Once we drop Python < 3.3, we can use 'yield from' here
        for id, tag in _unpack_diff_lines(stream=p.stdout):
            yield id, tag


def _unpack_diff_lines(stream):
    "Iterate through (id, tag) tuples in a diff stream."
    for line in stream:
        match = _TAG_FILE_REGEX[FORMAT_VERSION].match(line.strip())
        if not match:
            message = 'non-tag line in diff: {!r}'.format(line.strip())
            if line.startswith(_TAG_DIRECTORY):
                raise ValueError(message)
            _LOG.info(message)
            continue
        id = _unquote(match.group('id'))
        tag = _unquote(match.group('tag'))
        yield (id, tag)


def _help(parser, command=None):
    """
    Show help for an notmuch-git command.

    Because some folks prefer:

      $ notmuch-git help COMMAND

    to

      $ notmuch-git COMMAND --help
    """
    if command:
        parser.parse_args([command, '--help'])
    else:
        parser.parse_args(['--help'])

def _notmuch_config_get(key):
    (status, stdout, stderr) = _spawn(
        args=['notmuch', 'config', 'get', key],
        stdout=_subprocess.PIPE, wait=True)
    if status != 0:
        _LOG.error("failed to run notmuch config")
        _sys.exit(1)
    return stdout.rstrip()

def read_format_version():
    try:
        (status, stdout, stderr) = _git(
            args=['cat-file', 'blob', 'master:FORMAT'],
            stdout=_subprocess.PIPE, stderr=_subprocess.PIPE, wait=True)
    except SubprocessError as e:
        _LOG.debug("failed to read FORMAT file from git, assuming format version 0")
        return 0

    return int(stdout)

# based on BaseDirectory.save_data_path from pyxdg (LGPL2+)
def xdg_data_path(profile):
    resource = _os.path.join('notmuch',profile,'git')
    assert not resource.startswith('/')
    _home = _os.path.expanduser('~')
    xdg_data_home = _os.environ.get('XDG_DATA_HOME') or \
        _os.path.join(_home, '.local', 'share')
    path = _os.path.join(xdg_data_home, resource)
    return path

if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(
        description=__doc__.strip(),
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument(
        '-C', '--git-dir', metavar='REPO',
        help='Git repository to operate on.')
    parser.add_argument(
        '-p', '--tag-prefix', metavar='PREFIX',
        default = None,
        help='Prefix of tags to operate on.')
    parser.add_argument(
        '-N', '--nmbug', action='store_true',
        help='Set defaults for --tag-prefix and --git-dir for the notmuch bug tracker')
    parser.add_argument(
        '-l', '--log-level',
        choices=['critical', 'error', 'warning', 'info', 'debug'],
        help='Log verbosity.  Defaults to {!r}.'.format(
            _logging.getLevelName(_LOG.level).lower()))

    help = _functools.partial(_help, parser=parser)
    help.__doc__ = _help.__doc__
    subparsers = parser.add_subparsers(
        title='commands',
        description=(
            'For help on a particular command, run: '
            "'%(prog)s ... <command> --help'."))
    for command in [
            'archive',
            'checkout',
            'clone',
            'commit',
            'fetch',
            'help',
            'init',
            'log',
            'merge',
            'pull',
            'push',
            'status',
            ]:
        func = locals()[command]
        doc = _textwrap.dedent(func.__doc__).strip().replace('%', '%%')
        subparser = subparsers.add_parser(
            command,
            help=doc.splitlines()[0],
            description=doc,
            formatter_class=argparse.RawDescriptionHelpFormatter)
        subparser.set_defaults(func=func)
        if command == 'archive':
            subparser.add_argument(
                'treeish', metavar='TREE-ISH', nargs='?', default='HEAD',
                help=(
                    'The tree or commit to produce an archive for.  Defaults '
                    "to 'HEAD'."))
            subparser.add_argument(
                'args', metavar='ARG', nargs='*',
                help=(
                    "Argument passed through to 'git archive'.  Set anything "
                    'before <tree-ish>, see git-archive(1) for details.'))
        elif command == 'checkout':
            subparser.add_argument(
                '-f', '--force', action='store_true',
                help='checkout a large fraction of tags.')
        elif command == 'clone':
            subparser.add_argument(
                'repository',
                help=(
                    'The (possibly remote) repository to clone from.  See the '
                    'URLS section of git-clone(1) for more information on '
                    'specifying repositories.'))
        elif command == 'commit':
            subparser.add_argument(
                '-f', '--force', action='store_true',
                help='commit a large fraction of tags.')
            subparser.add_argument(
                'message', metavar='MESSAGE', default='', nargs='?',
                help='Text for the commit message.')
        elif command == 'fetch':
            subparser.add_argument(
                'remote', metavar='REMOTE', nargs='?',
                help=(
                    'Override the default configured in branch.<name>.remote '
                    'to fetch from a particular remote repository (e.g. '
                    "'origin')."))
        elif command == 'help':
            subparser.add_argument(
                'command', metavar='COMMAND', nargs='?',
                help='The command to show help for.')
        elif command == 'init':
            subparser.add_argument(
                '--format-version', metavar='VERSION',
                default = None,
                help='create format VERSION repository.')
        elif command == 'log':
            subparser.add_argument(
                'args', metavar='ARG', nargs='*',
                help="Additional argument passed through to 'git log'.")
        elif command == 'merge':
            subparser.add_argument(
                'reference', metavar='REFERENCE', default='@{upstream}',
                nargs='?',
                help=(
                    'Reference, usually other branch heads, to merge into '
                    "our branch.  Defaults to '@{upstream}'."))
        elif command == 'pull':
            subparser.add_argument(
                'repository', metavar='REPOSITORY', default=None, nargs='?',
                help=(
                    'The "remote" repository that is the source of the pull.  '
                    'This parameter can be either a URL (see the section GIT '
                    'URLS in git-pull(1)) or the name of a remote (see the '
                    'section REMOTES in git-pull(1)).'))
            subparser.add_argument(
                'refspecs', metavar='REFSPEC', default=None, nargs='*',
                help=(
                    'Refspec (usually a branch name) to fetch and merge.  See '
                    'the <refspec> entry in the OPTIONS section of '
                    'git-pull(1) for other possibilities.'))
        elif command == 'push':
            subparser.add_argument(
               'repository', metavar='REPOSITORY', default=None, nargs='?',
                help=(
                    'The "remote" repository that is the destination of the '
                    'push.  This parameter can be either a URL (see the '
                    'section GIT URLS in git-push(1)) or the name of a remote '
                    '(see the section REMOTES in git-push(1)).'))
            subparser.add_argument(
                'refspecs', metavar='REFSPEC', default=None, nargs='*',
                help=(
                    'Refspec (usually a branch name) to push.  See '
                    'the <refspec> entry in the OPTIONS section of '
                    'git-push(1) for other possibilities.'))

    args = parser.parse_args()

    nmbug_mode = False
    notmuch_profile = _os.getenv('NOTMUCH_PROFILE','default')

    if args.nmbug or _os.path.basename(__file__) == 'nmbug':
        nmbug_mode = True

    if args.git_dir:
        NOTMUCH_GIT_DIR = args.git_dir
    else:
        if nmbug_mode:
            default = _os.path.join('~', '.nmbug')
        else:
            default = _notmuch_config_get ('git.path')
            if default == '':
                default = xdg_data_path(notmuch_profile)

        NOTMUCH_GIT_DIR = _os.path.expanduser(_os.getenv('NOTMUCH_GIT_DIR', default))

    _NOTMUCH_GIT_DIR = _os.path.join(NOTMUCH_GIT_DIR, '.git')
    if _os.path.isdir(_NOTMUCH_GIT_DIR):
        NOTMUCH_GIT_DIR = _NOTMUCH_GIT_DIR

    if args.tag_prefix:
        TAG_PREFIX = args.tag_prefix
    else:
        if nmbug_mode:
            prefix = 'notmuch::'
        else:
            prefix = _notmuch_config_get ('git.tag_prefix')

        TAG_PREFIX =  _os.getenv('NOTMUCH_GIT_PREFIX', prefix)

    _ENCODED_TAG_PREFIX = _hex_quote(TAG_PREFIX, safe='+@=,')  # quote ':'

    if args.log_level:
        level = getattr(_logging, args.log_level.upper())
        _LOG.setLevel(level)

    # for test suite
    for var in ['NOTMUCH_GIT_DIR', 'NOTMUCH_GIT_PREFIX', 'NOTMUCH_PROFILE', 'NOTMUCH_CONFIG' ]:
        _LOG.debug('env {:s} = {:s}'.format(var, _os.getenv(var,'%None%')))

    if _notmuch_config_get('built_with.sexp_queries') != 'true':
        _LOG.error("notmuch git needs sexp query support")
        _sys.exit(1)

    if not getattr(args, 'func', None):
        parser.print_usage()
        _sys.exit(1)

    # The following two lines are used by the test suite.
    _LOG.debug('prefix = {:s}'.format(TAG_PREFIX))
    _LOG.debug('repository = {:s}'.format(NOTMUCH_GIT_DIR))

    if args.func != init:
        FORMAT_VERSION = read_format_version()

    _LOG.debug('FORMAT_VERSION={:d}'.format(FORMAT_VERSION))

    if args.func == help:
        arg_names = ['command']
    else:
        (arg_names, varargs, varkw) = _inspect.getargs(args.func.__code__)
    kwargs = {key: getattr(args, key) for key in arg_names if key in args}
    try:
        args.func(**kwargs)
    except SubprocessError as e:
        if _LOG.level == _logging.DEBUG:
            raise  # don't mask the traceback
        _LOG.error(str(e))
        _sys.exit(1)
