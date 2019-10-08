import collections.abc
import time
import pathlib

import pytest

import notdb


class TestMessage:
    MaildirMsg = collections.namedtuple('MaildirMsg', ['msgid', 'path'])

    @pytest.fixture
    def maildir_msg(self, maildir):
        msgid, path = maildir.deliver()
        return self.MaildirMsg(msgid, path)

    @pytest.fixture
    def db(self, maildir):
        with notdb.Database.create(maildir.path) as db:
            yield db

    @pytest.fixture
    def msg(self, db, maildir_msg):
        msg, dup = db.add(maildir_msg.path, sync_flags=False)
        yield msg

    def test_type(self, msg):
        assert isinstance(msg, notdb.NotmuchObject)
        assert isinstance(msg, notdb.Message)

    def test_alive(self, msg):
        assert msg.alive

    def test_hash(self, msg):
        assert hash(msg)

    def test_eq(self, db, msg):
        copy = db.get(msg.path)
        assert msg == copy

    def test_messageid_type(self, msg):
        assert isinstance(msg.messageid, str)
        assert isinstance(msg.messageid, notdb.BinString)
        assert isinstance(bytes(msg.messageid), bytes)

    def test_messageid(self, msg, maildir_msg):
        assert msg.messageid == maildir_msg.msgid

    def test_messageid_find(self, db, msg):
        copy = db.find(msg.messageid)
        assert msg.messageid == copy.messageid

    def test_threadid_type(self, msg):
        assert isinstance(msg.threadid, str)
        assert isinstance(msg.threadid, notdb.BinString)
        assert isinstance(bytes(msg.threadid), bytes)

    def test_path_type(self, msg):
        assert isinstance(msg.path, pathlib.Path)

    def test_path(self, msg, maildir_msg):
        assert msg.path == maildir_msg.path

    def test_pathb_type(self, msg):
        assert isinstance(msg.pathb, bytes)

    def test_pathb(self, msg, maildir_msg):
        assert msg.path == maildir_msg.path

    def test_filenames_type(self, msg):
        ifn = msg.filenames()
        assert isinstance(ifn, collections.abc.Iterator)

    def test_filenames(self, msg):
        ifn = msg.filenames()
        fn = next(ifn)
        assert fn == msg.path
        assert isinstance(fn, pathlib.Path)
        with pytest.raises(StopIteration):
            next(ifn)
        assert list(msg.filenames()) == [msg.path]

    def test_filenamesb_type(self, msg):
        ifn = msg.filenamesb()
        assert isinstance(ifn, collections.abc.Iterator)

    def test_filenamesb(self, msg):
        ifn = msg.filenamesb()
        fn = next(ifn)
        assert fn == msg.pathb
        assert isinstance(fn, bytes)
        with pytest.raises(StopIteration):
            next(ifn)
        assert list(msg.filenamesb()) == [msg.pathb]

    def test_ghost_no(self, msg):
        assert not msg.ghost

    def test_date(self, msg):
        # XXX Someone seems to treat things as local time instead of
        #     UTC or the other way around.
        now = int(time.time())
        assert abs(now - msg.date) < 3600*24

    def test_header(self, msg):
        assert msg.header('from') == 'src@example.com'

    def test_header_not_present(self, msg):
        with pytest.raises(LookupError):
            msg.header('foo')

    def test_freeze(self, msg):
        with msg.frozen():
            msg.tags.add('foo')
            msg.tags.add('bar')
            msg.tags.discard('foo')
        assert 'foo' not in msg.tags
        assert 'bar' in msg.tags

    def test_freeze_err(self, msg):
        msg.tags.add('foo')
        try:
            with msg.frozen():
                msg.tags.clear()
                raise Exception('oops')
        except Exception:
            assert 'foo' in msg.tags
        else:
            pytest.fail('Context manager did not raise')

    def test_replies_type(self, msg):
        assert isinstance(msg.replies(), collections.abc.Iterator)

    def test_replies(self, msg):
        with pytest.raises(StopIteration):
            next(msg.replies())


class TestProperties:

    @pytest.fixture
    def props(self, maildir):
        msgid, path = maildir.deliver()
        with notdb.Database.create(maildir.path) as db:
            msg, dup = db.add(path, sync_flags=False)
            yield msg.properties

    def test_type(self, props):
        assert isinstance(props, collections.abc.MutableMapping)

    def test_add_single(self, props):
        props['foo'] = 'bar'
        assert props['foo'] == 'bar'
        props.add('bar', 'baz')
        assert props['bar'] == 'baz'

    def test_add_dup(self, props):
        props.add('foo', 'bar')
        props.add('foo', 'baz')
        assert props['foo'] == 'bar'
        assert (set(props.getall('foo', exact=True))
                == {('foo', 'bar'), ('foo', 'baz')})

    def test_len(self, props):
        props.add('foo', 'a')
        props.add('foo', 'b')
        props.add('bar', 'a')
        assert len(props) == 3
        assert len(props.keys()) == 2
        assert len(props.values()) == 2
        assert len(props.items()) == 3

    def test_del(self, props):
        props.add('foo', 'a')
        props.add('foo', 'b')
        del props['foo']
        with pytest.raises(KeyError):
            props['foo']

    def test_remove(self, props):
        props.add('foo', 'a')
        props.add('foo', 'b')
        props.remove('foo', 'a')
        assert props['foo'] == 'b'

    def test_view_abcs(self, props):
        assert isinstance(props.keys(), collections.abc.KeysView)
        assert isinstance(props.values(), collections.abc.ValuesView)
        assert isinstance(props.items(), collections.abc.ItemsView)

    def test_pop(self, props):
        props.add('foo', 'a')
        props.add('foo', 'b')
        val = props.pop('foo')
        assert val == 'a'

    def test_pop_default(self, props):
        with pytest.raises(KeyError):
            props.pop('foo')
        assert props.pop('foo', 'default') == 'default'

    def test_popitem(self, props):
        props.add('foo', 'a')
        assert props.popitem() == ('foo', 'a')
        with pytest.raises(KeyError):
            props.popitem()

    def test_clear(self, props):
        props.add('foo', 'a')
        props.clear()
        assert len(props) == 0

    def test_getall(self, props):
        props.add('foo', 'a')
        assert set(props.getall('foo')) == {('foo', 'a')}

    def test_getall_prefix(self, props):
        props.add('foo', 'a')
        props.add('foobar', 'b')
        assert set(props.getall('foo')) == {('foo', 'a'), ('foobar', 'b')}

    def test_getall_exact(self, props):
        props.add('foo', 'a')
        props.add('foobar', 'b')
        assert set(props.getall('foo', exact=True)) == {('foo', 'a')}
