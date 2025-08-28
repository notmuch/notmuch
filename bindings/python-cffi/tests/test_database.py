import collections
import configparser
import os
import pathlib

import pytest

import notmuch2
import notmuch2._errors as errors
import notmuch2._database as dbmod
import notmuch2._message as message


@pytest.fixture
def db(maildir):
    with dbmod.Database.create(maildir.path, config=notmuch2.Database.CONFIG.EMPTY) as db:
        yield db


class TestDefaultDb:
    """Tests for reading the default database.

    The error cases are fairly undefined, some relevant Python error
    will come out if you give it a bad filename or if the file does
    not parse correctly.  So we're not testing this too deeply.
    """

    def test_config_pathname_default(self, monkeypatch):
        monkeypatch.delenv('NOTMUCH_CONFIG', raising=False)
        user = pathlib.Path('~/.notmuch-config').expanduser()
        assert dbmod._config_pathname() == user

    def test_config_pathname_env(self, monkeypatch):
        monkeypatch.setenv('NOTMUCH_CONFIG', '/some/random/path')
        assert dbmod._config_pathname() == pathlib.Path('/some/random/path')

    def test_default_path_nocfg(self, monkeypatch, tmppath):
        monkeypatch.setenv('NOTMUCH_CONFIG', str(tmppath/'foo'))
        with pytest.raises(FileNotFoundError):
            dbmod.Database.default_path()

    def test_default_path_cfg_is_dir(self, monkeypatch, tmppath):
        monkeypatch.setenv('NOTMUCH_CONFIG', str(tmppath))
        with pytest.raises(IsADirectoryError):
            dbmod.Database.default_path()

    def test_default_path_parseerr(self, monkeypatch, tmppath):
        cfg = tmppath / 'notmuch-config'
        with cfg.open('w') as fp:
            fp.write('invalid')
        monkeypatch.setenv('NOTMUCH_CONFIG', str(cfg))
        with pytest.raises(configparser.Error):
            dbmod.Database.default_path()

    def test_default_path_parse(self, monkeypatch, tmppath):
        cfg = tmppath / 'notmuch-config'
        with cfg.open('w') as fp:
            fp.write('[database]\n')
            fp.write('path={!s}'.format(tmppath))
        monkeypatch.setenv('NOTMUCH_CONFIG', str(cfg))
        assert dbmod.Database.default_path() == tmppath

    def test_default_path_param(self, monkeypatch, tmppath):
        cfg_dummy = tmppath / 'dummy'
        monkeypatch.setenv('NOTMUCH_CONFIG', str(cfg_dummy))
        cfg_real = tmppath / 'notmuch_config'
        with cfg_real.open('w') as fp:
            fp.write('[database]\n')
            fp.write('path={!s}'.format(cfg_real/'mail'))
        assert dbmod.Database.default_path(cfg_real) == cfg_real/'mail'


class TestCreate:

    def test_create(self, tmppath, db):
        assert tmppath.joinpath('.notmuch/xapian/').exists()

    def test_create_already_open(self, tmppath, db):
        with pytest.raises(errors.NotmuchError):
            db.create(tmppath)

    def test_create_existing(self, tmppath, db):
        with pytest.raises(errors.DatabaseExistsError):
            dbmod.Database.create(path=tmppath)

    def test_close(self, db):
        db.close()

    def test_del_noclose(self, db):
        del db

    def test_close_del(self, db):
        db.close()
        del db

    def test_closed_attr(self, db):
        assert not db.closed
        db.close()
        assert db.closed

    def test_ctx(self, db):
        with db as ctx:
            assert ctx is db
            assert not db.closed
        assert db.closed

    def test_path(self, db, tmppath):
        assert db.path == tmppath

    def test_version(self, db):
        assert db.version > 0

    def test_needs_upgrade(self, db):
        assert db.needs_upgrade in (True, False)


class TestAtomic:

    def test_exit_early(self, db):
        with pytest.raises(errors.UnbalancedAtomicError):
            with db.atomic() as ctx:
                ctx.force_end()

    def test_exit_late(self, db):
        with db.atomic() as ctx:
            pass
        with pytest.raises(errors.UnbalancedAtomicError):
            ctx.force_end()

    def test_abort(self, db):
        with db.atomic() as txn:
            txn.abort()
        assert db.closed


class TestRevision:

    def test_single_rev(self, db):
        r = db.revision()
        assert isinstance(r, dbmod.DbRevision)
        assert isinstance(r.rev, int)
        assert isinstance(r.uuid, bytes)
        assert r is r
        assert r == r
        assert r <= r
        assert r >= r
        assert not r < r
        assert not r > r

    def test_diff_db(self, tmppath):
        dbpath0 = tmppath.joinpath('db0')
        dbpath0.mkdir()
        dbpath1 = tmppath.joinpath('db1')
        dbpath1.mkdir()
        db0 = dbmod.Database.create(path=dbpath0)
        db1 = dbmod.Database.create(path=dbpath1)
        r_db0 = db0.revision()
        r_db1 = db1.revision()
        assert r_db0 != r_db1
        assert r_db0.uuid != r_db1.uuid

    def test_cmp(self, db, maildir):
        rev0 = db.revision()
        _, pathname = maildir.deliver()
        db.add(pathname, sync_flags=False)
        rev1 = db.revision()
        assert rev0 < rev1
        assert rev0 <= rev1
        assert not rev0 > rev1
        assert not rev0 >= rev1
        assert not rev0 == rev1
        assert rev0 != rev1

    # XXX add tests for revisions comparisons


class TestMode:

    def test_readonly_raises(self, db, maildir):
        with pytest.raises(errors.ReadOnlyDatabaseError):
            with dbmod.Database(maildir.path, 'ro',
                                config=notmuch2.Database.CONFIG.EMPTY) as db_ro:
                _, pathname = maildir.deliver()
                db_ro.add(pathname)

    def test_reopen_ro(self, db, maildir):
        db.reopen(mode = dbmod.Mode.READ_ONLY)
        with pytest.raises(errors.ReadOnlyDatabaseError):
            _, pathname = maildir.deliver()
            db.add(pathname)

    def test_reopen_rw(self, db, maildir):
        # release the write lock
        db.close()

        with dbmod.Database(maildir.path, 'ro',
                            config=notmuch2.Database.CONFIG.EMPTY) as db:
            _, pathname = maildir.deliver()
            db.reopen(mode = dbmod.Mode.READ_WRITE)
            db.add(pathname)

class TestMessages:

    def test_add_message(self, db, maildir):
        msgid, pathname = maildir.deliver()
        msg, dup = db.add(pathname, sync_flags=False)
        assert isinstance(msg, message.Message)
        assert msg.path == pathname
        assert msg.messageid == msgid

    def test_add_message_str(self, db, maildir):
        msgid, pathname = maildir.deliver()
        msg, dup = db.add(str(pathname), sync_flags=False)

    def test_add_message_bytes(self, db, maildir):
        msgid, pathname = maildir.deliver()
        msg, dup = db.add(os.fsencode(bytes(pathname)), sync_flags=False)

    def test_remove_message(self, db, maildir):
        msgid, pathname = maildir.deliver()
        msg, dup = db.add(pathname, sync_flags=False)
        assert db.find(msgid)
        dup = db.remove(pathname)
        with pytest.raises(LookupError):
            db.find(msgid)

    def test_remove_message_str(self, db, maildir):
        msgid, pathname = maildir.deliver()
        msg, dup = db.add(pathname, sync_flags=False)
        assert db.find(msgid)
        dup = db.remove(str(pathname))
        with pytest.raises(LookupError):
            db.find(msgid)

    def test_remove_message_bytes(self, db, maildir):
        msgid, pathname = maildir.deliver()
        msg, dup = db.add(pathname, sync_flags=False)
        assert db.find(msgid)
        dup = db.remove(os.fsencode(bytes(pathname)))
        with pytest.raises(LookupError):
            db.find(msgid)

    def test_find_message(self, db, maildir):
        msgid, pathname = maildir.deliver()
        msg0, dup = db.add(pathname, sync_flags=False)
        msg1 = db.find(msgid)
        assert isinstance(msg1, message.Message)
        assert msg1.messageid == msgid == msg0.messageid
        assert msg1.path == pathname == msg0.path

    def test_find_message_notfound(self, db):
        with pytest.raises(LookupError):
            db.find('foo')

    def test_get_message(self, db, maildir):
        msgid, pathname = maildir.deliver()
        msg0, _ = db.add(pathname, sync_flags=False)
        msg1 = db.get(pathname)
        assert isinstance(msg1, message.Message)
        assert msg1.messageid == msgid == msg0.messageid
        assert msg1.path == pathname == msg0.path

    def test_get_message_str(self, db, maildir):
        msgid, pathname = maildir.deliver()
        db.add(pathname, sync_flags=False)
        msg = db.get(str(pathname))
        assert msg.messageid == msgid

    def test_get_message_bytes(self, db, maildir):
        msgid, pathname = maildir.deliver()
        db.add(pathname, sync_flags=False)
        msg = db.get(os.fsencode(bytes(pathname)))
        assert msg.messageid == msgid


class TestTags:
    # We just want to test this behaves like a set at a hight level.
    # The set semantics are tested in detail in the test_tags module.

    def test_type(self, db):
        assert isinstance(db.tags, collections.abc.Set)

    def test_none(self, db):
        itags = iter(db.tags)
        with pytest.raises(StopIteration):
            next(itags)
        assert len(db.tags) == 0
        assert not db.tags

    def test_some(self, db, maildir):
        _, pathname = maildir.deliver()
        msg, _ = db.add(pathname, sync_flags=False)
        msg.tags.add('hello')
        itags = iter(db.tags)
        assert next(itags) == 'hello'
        with pytest.raises(StopIteration):
            next(itags)
        assert 'hello' in msg.tags

    def test_cache(self, db):
        assert db.tags is db.tags

    def test_iters(self, db):
        i1 = iter(db.tags)
        i2 = iter(db.tags)
        assert i1 is not i2


class TestQuery:

    @pytest.fixture
    def db(self, maildir, notmuch):
        """Return a read-only notmuch2.Database.

        The database will have 3 messages, 2 threads.
        """
        msgid, _ = maildir.deliver(body='foo')
        maildir.deliver(body='bar')
        maildir.deliver(body='baz',
                        headers=[('In-Reply-To', '<{}>'.format(msgid))])
        notmuch('new')
        with dbmod.Database(maildir.path, 'rw', config=notmuch2.Database.CONFIG.EMPTY) as db:
            yield db

    def _db_modified(self, maildir, notmuch, ret_prepare=None):
        # populate the database for the initial query
        with dbmod.Database.create(maildir.path, config=notmuch2.Database.CONFIG.EMPTY) as db:
            for i in range(32):
                pathname = maildir.deliver(body = str(i))[1]
                msg = db.add(str(pathname))[0]
                msg.tags.add(str(i))

        with dbmod.Database(maildir.path, 'ro', config=notmuch2.Database.CONFIG.EMPTY) as db:
            # prepare value to be returned to caller
            ret = ret_prepare(db) if ret_prepare else db

            # modify the database sufficiently to trigger DatabaseModifiedException
            for i in range(16):
                with dbmod.Database(maildir.path, 'rw', config=notmuch2.Database.CONFIG.EMPTY) as db_rw:
                    pathname = maildir.deliver(body = str(i))[1]
                    db_rw.add(str(pathname))

            yield ret

    @pytest.fixture
    def db_modified(self, maildir, notmuch):
        "A db triggering DatabaseModifiedException."
        yield from self._db_modified(maildir, notmuch)

    @pytest.fixture
    def db_modified_messages(self, maildir, notmuch):
        "A tuple of (db, messages) triggering DatabaseModifiedException."
        yield from self._db_modified(maildir, notmuch, lambda db: (db, db.messages('*')))

    @pytest.fixture
    def db_modified_threads(self, maildir, notmuch):
        "A tuple of (db, threads) triggering DatabaseModifiedException."
        yield from self._db_modified(maildir, notmuch, lambda db: (db, db.threads('*')))

    def test_count_messages(self, db):
        assert db.count_messages('*') == 3

    def test_messages_type(self, db):
        msgs = db.messages('*')
        assert isinstance(msgs, collections.abc.Iterator)

    def test_messages_iterator(self, db):
        for msg in db.messages('*'):
            assert isinstance(msg, notmuch2.Message)
            assert isinstance(msg.messageid, str)

    def test_messages_iterator_list(self, db):
        msgs = list(db.messages('*'))
        assert len(msgs) == 3
        for msg in msgs:
            assert isinstance(msg, notmuch2.Message)
            assert isinstance(msg.messageid, str)

    def test_message_no_results(self, db):
        msgs = db.messages('not_a_matching_query')
        with pytest.raises(StopIteration):
            next(msgs)

    def test_message_match(self, db):
        msgs = db.messages('*')
        msg = next(msgs)
        assert isinstance(msg, notmuch2.Message)

    def test_count_threads(self, db):
        assert db.count_threads('*') == 2

    def test_threads_type(self, db):
        threads = db.threads('*')
        assert isinstance(threads, collections.abc.Iterator)

    def test_threads_iterator(self, db):
        for t in db.threads('*'):
            assert isinstance(t, notmuch2.Thread)
            assert isinstance(t.threadid, str)
            for msg in t:
                assert isinstance(msg, notmuch2.Message)
                assert isinstance(msg.messageid, str)

    def test_threads_iterator_list(self, db):
        threads = list(db.threads('*'))
        assert len(threads) == 2
        for t in threads:
            assert isinstance(t, notmuch2.Thread)
            assert isinstance(t.threadid, str)
            msgs = list(t)
            for msg in msgs:
                assert isinstance(msg, notmuch2.Message)
                assert isinstance(msg.messageid, str)

    def test_threads_no_match(self, db):
        threads = db.threads('not_a_matching_query')
        with pytest.raises(StopIteration):
            next(threads)

    def test_threads_match(self, db):
        threads = db.threads('*')
        thread = next(threads)
        assert isinstance(thread, notmuch2.Thread)

    def test_use_threaded_message_twice(self, db):
        thread = next(db.threads('*'))
        for msg in thread.toplevel():
            assert isinstance(msg, notmuch2.Message)
            assert msg.alive
            del msg
        for msg in thread:
            assert isinstance(msg, notmuch2.Message)
            assert msg.alive
            del msg

    def test_operation_invalidated_query(self, db_modified):
        # Test OperationInvalidatedError raised by instantiating the query.
        for attempt in 1, 2:
            try:
                for msg in db_modified.messages('*'):
                    pass
                break
            except notmuch2.OperationInvalidatedError:
                if attempt == 1:
                    db_modified.reopen()
                    continue

                raise

    def test_operation_invalidated_messages(self, db_modified_messages):
        # Test OperationInvalidatedError raised by iterating over query results;
        # the query itself is created while the database is still usable.
        db, messages = db_modified_messages

        for attempt in 1, 2:
            try:
                for msg in messages:
                    pass
                break
            except notmuch2.OperationInvalidatedError:
                if attempt == 1:
                    db.reopen()
                    messages = db.messages('*')
                    continue

                raise

    def test_operation_invalidated_threads(self, db_modified_threads):
        db, threads = db_modified_threads

        for attempt in 1, 2:
            try:
                for t in threads:
                    for msg in t:
                        pass
                break
            except notmuch2.OperationInvalidatedError:
                if attempt == 1:
                    db.reopen()
                    threads = db.threads('*')
                    continue

                raise
