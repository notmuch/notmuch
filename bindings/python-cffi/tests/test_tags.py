"""Tests for the behaviour of immutable and mutable tagsets.

This module tests the Pythonic behaviour of the sets.
"""

import collections
import subprocess
import textwrap

import pytest

from notdb import _database as database
from notdb import _tags as tags


class TestImmutable:

    @pytest.fixture
    def tagset(self, maildir, notmuch):
        """An non-empty immutable tagset.

        This will have the default new mail tags: inbox, unread.
        """
        maildir.deliver()
        notmuch('new')
        with database.Database(maildir.path) as db:
            yield db.tags

    def test_type(self, tagset):
        assert isinstance(tagset, tags.ImmutableTagSet)
        assert isinstance(tagset, collections.abc.Set)

    def test_hash(self, tagset, maildir, notmuch):
        h0 = hash(tagset)
        notmuch('tag', '+foo', '*')
        with database.Database(maildir.path) as db:
            h1 = hash(db.tags)
        assert h0 != h1

    def test_eq(self, tagset):
        assert tagset == tagset

    def test_neq(self, tagset, maildir, notmuch):
        notmuch('tag', '+foo', '*')
        with database.Database(maildir.path) as db:
            assert tagset != db.tags

    def test_contains(self, tagset):
        print(tuple(tagset))
        assert 'unread' in tagset
        assert 'foo' not in tagset

    def test_iter(self, tagset):
        expected = sorted(['unread', 'inbox'])
        found = []
        for tag in tagset:
            assert isinstance(tag, str)
            found.append(tag)
        assert expected == sorted(found)

    def test_special_iter(self, tagset):
        expected = sorted([b'unread', b'inbox'])
        found = []
        for tag in tagset.iter():
            assert isinstance(tag, bytes)
            found.append(tag)
        assert expected == sorted(found)

    def test_special_iter_codec(self, tagset):
        for tag in tagset.iter(encoding='ascii', errors='surrogateescape'):
            assert isinstance(tag, str)

    def test_len(self, tagset):
        assert len(tagset) == 2

    def test_and(self, tagset):
        common = tagset & {'unread'}
        assert isinstance(common, set)
        assert isinstance(common, collections.abc.Set)
        assert common == {'unread'}

    def test_or(self, tagset):
        res = tagset | {'foo'}
        assert isinstance(res, set)
        assert isinstance(res, collections.abc.Set)
        assert res == {'unread', 'inbox', 'foo'}

    def test_sub(self, tagset):
        res = tagset - {'unread'}
        assert isinstance(res, set)
        assert isinstance(res, collections.abc.Set)
        assert res == {'inbox'}

    def test_rsub(self, tagset):
        res = {'foo', 'unread'} - tagset
        assert isinstance(res, set)
        assert isinstance(res, collections.abc.Set)
        assert res == {'foo'}

    def test_xor(self, tagset):
        res = tagset ^ {'unread', 'foo'}
        assert isinstance(res, set)
        assert isinstance(res, collections.abc.Set)
        assert res == {'inbox', 'foo'}

    def test_rxor(self, tagset):
        res = {'unread', 'foo'} ^ tagset
        assert isinstance(res, set)
        assert isinstance(res, collections.abc.Set)
        assert res == {'inbox', 'foo'}


class TestMutableTagset:

    @pytest.fixture
    def tagset(self, maildir, notmuch):
        """An non-empty mutable tagset.

        This will have the default new mail tags: inbox, unread.
        """
        _, pathname = maildir.deliver()
        notmuch('new')
        with database.Database(maildir.path,
                               mode=database.Mode.READ_WRITE) as db:
            msg = db.get(pathname)
            yield msg.tags

    def test_type(self, tagset):
        assert isinstance(tagset, collections.abc.MutableSet)
        assert isinstance(tagset, tags.MutableTagSet)

    def test_hash(self, tagset):
        assert not isinstance(tagset, collections.abc.Hashable)
        with pytest.raises(TypeError):
            hash(tagset)

    def test_add(self, tagset):
        assert 'foo' not in tagset
        tagset.add('foo')
        assert 'foo' in tagset

    def test_discard(self, tagset):
        assert 'inbox' in tagset
        tagset.discard('inbox')
        assert 'inbox' not in tagset

    def test_discard_not_present(self, tagset):
        assert 'foo' not in tagset
        tagset.discard('foo')

    def test_clear(self, tagset):
        assert len(tagset) > 0
        tagset.clear()
        assert len(tagset) == 0

    def test_from_maildir_flags(self, maildir, notmuch):
        _, pathname = maildir.deliver(flagged=True)
        notmuch('new')
        with database.Database(maildir.path,
                               mode=database.Mode.READ_WRITE) as db:
            msg = db.get(pathname)
            msg.tags.discard('flagged')
            msg.tags.from_maildir_flags()
            assert 'flagged' in msg.tags

    def test_to_maildir_flags(self, maildir, notmuch):
        _, pathname = maildir.deliver(flagged=True)
        notmuch('new')
        with database.Database(maildir.path,
                               mode=database.Mode.READ_WRITE) as db:
            msg = db.get(pathname)
            flags = msg.path.name.split(',')[-1]
            assert 'F' in flags
            msg.tags.discard('flagged')
            msg.tags.to_maildir_flags()
            flags = msg.path.name.split(',')[-1]
            assert 'F' not in flags
