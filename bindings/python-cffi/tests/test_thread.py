import collections.abc
import time

import pytest

import notmuch2


@pytest.fixture
def thread(maildir, notmuch):
    """Return a single thread with one matched message."""
    msgid, _ = maildir.deliver(body='foo')
    maildir.deliver(body='bar',
                    headers=[('In-Reply-To', '<{}>'.format(msgid))])
    notmuch('new')
    with notmuch2.Database(maildir.path) as db:
        yield next(db.threads('foo'))


def test_type(thread):
    assert isinstance(thread, notmuch2.Thread)
    assert isinstance(thread, collections.abc.Iterable)


def test_threadid(thread):
    assert isinstance(thread.threadid, notmuch2.BinString)
    assert thread.threadid


def test_len(thread):
    assert len(thread) == 2


def test_toplevel_type(thread):
    assert isinstance(thread.toplevel(), collections.abc.Iterator)


def test_toplevel(thread):
    msgs = thread.toplevel()
    assert isinstance(next(msgs), notmuch2.Message)
    with pytest.raises(StopIteration):
        next(msgs)


def test_toplevel_reply(thread):
    msg = next(thread.toplevel())
    assert isinstance(next(msg.replies()), notmuch2.Message)


def test_iter(thread):
    msgs = list(iter(thread))
    assert len(msgs) == len(thread)
    for msg in msgs:
        assert isinstance(msg, notmuch2.Message)


def test_matched(thread):
    assert thread.matched == 1


def test_authors_type(thread):
    assert isinstance(thread.authors, notmuch2.BinString)


def test_authors(thread):
    assert thread.authors == 'src@example.com'


def test_subject(thread):
    assert thread.subject == 'Test mail'


def test_first(thread):
    # XXX Someone seems to treat things as local time instead of
    #     UTC or the other way around.
    now = int(time.time())
    assert abs(now - thread.first) < 3600*24


def test_last(thread):
    # XXX Someone seems to treat things as local time instead of
    #     UTC or the other way around.
    now = int(time.time())
    assert abs(now - thread.last) < 3600*24


def test_first_last(thread):
    # Sadly we only have second resolution so these will always be the
    # same time in our tests.
    assert thread.first <= thread.last


def test_tags_type(thread):
    assert isinstance(thread.tags, notmuch2.ImmutableTagSet)


def test_tags_cache(thread):
    assert thread.tags is thread.tags


def test_tags(thread):
    assert 'inbox' in thread.tags
