import collections.abc
import weakref

from notmuch2 import _base as base
from notmuch2 import _capi as capi
from notmuch2 import _errors as errors
from notmuch2 import _message as message
from notmuch2 import _tags as tags


__all__ = ['Thread']


class Thread(base.NotmuchObject, collections.abc.Iterable):
    _thread_p = base.MemoryPointer()

    def __init__(self, parent, thread_p, *, db):
        self._parent = parent
        self._thread_p = thread_p
        self._db = db

    @property
    def alive(self):
        if not self._parent.alive:
            return False
        try:
            self._thread_p
        except errors.ObjectDestroyedError:
            return False
        else:
            return True

    def __del__(self):
        self._destroy()

    def _destroy(self):
        if self.alive:
            capi.lib.notmuch_thread_destroy(self._thread_p)
        self._thread_p = None

    @property
    def threadid(self):
        """The thread ID as a :class:`BinString`.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        ret = capi.lib.notmuch_thread_get_thread_id(self._thread_p)
        return base.BinString.from_cffi(ret)

    def __len__(self):
        """Return the number of messages in the thread.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        return capi.lib.notmuch_thread_get_total_messages(self._thread_p)

    def toplevel(self):
        """Return an iterator of the toplevel messages.

        :returns: An iterator yielding :class:`Message` instances.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        msgs_p = capi.lib.notmuch_thread_get_toplevel_messages(self._thread_p)
        return message.MessageIter(self, msgs_p,
                                   db=self._db,
                                   msg_cls=message.OwnedMessage)

    def __iter__(self):
        """Return an iterator over all the messages in the thread.

        :returns: An iterator yielding :class:`Message` instances.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        msgs_p = capi.lib.notmuch_thread_get_messages(self._thread_p)
        return message.MessageIter(self, msgs_p,
                                   db=self._db,
                                   msg_cls=message.OwnedMessage)

    @property
    def matched(self):
        """The number of messages in this thread which matched the query.

        Of the messages in the thread this gives the count of messages
        which did directly match the search query which this thread
        originates from.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        return capi.lib.notmuch_thread_get_matched_messages(self._thread_p)

    @property
    def authors(self):
        """A comma-separated string of all authors in the thread.

        Authors of messages which matched the query the thread was
        retrieved from will be at the head of the string, ordered by
        date of their messages.  Following this will be the authors of
        the other messages in the thread, also ordered by date of
        their messages.  Both groups of authors are separated by the
        ``|`` character.

        :returns: The stringified list of authors.
        :rtype: BinString

        :raises ObjectDestroyedError: if used after destroyed.
        """
        ret = capi.lib.notmuch_thread_get_authors(self._thread_p)
        return base.BinString.from_cffi(ret)

    @property
    def subject(self):
        """The subject of the thread, taken from the first message.

        The thread's subject is taken to be the subject of the first
        message according to query sort order.

        :returns: The thread's subject.
        :rtype: BinString

        :raises ObjectDestroyedError: if used after destroyed.
        """
        ret = capi.lib.notmuch_thread_get_subject(self._thread_p)
        return base.BinString.from_cffi(ret)

    @property
    def first(self):
        """Return the date of the oldest message in the thread.

        The time the first message was sent as an integer number of
        seconds since the *epoch*, 1 Jan 1970.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        return capi.lib.notmuch_thread_get_oldest_date(self._thread_p)

    @property
    def last(self):
        """Return the date of the newest message in the thread.

        The time the last message was sent as an integer number of
        seconds since the *epoch*, 1 Jan 1970.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        return capi.lib.notmuch_thread_get_newest_date(self._thread_p)

    @property
    def tags(self):
        """Return an immutable set with all tags used in this thread.

        This returns an immutable set-like object implementing the
        collections.abc.Set Abstract Base Class.  Due to the
        underlying libnotmuch implementation some operations have
        different performance characteristics then plain set objects.
        Mainly any lookup operation is O(n) rather then O(1).

        Normal usage treats tags as UTF-8 encoded unicode strings so
        they are exposed to Python as normal unicode string objects.
        If you need to handle tags stored in libnotmuch which are not
        valid unicode do check the :class:`ImmutableTagSet` docs for
        how to handle this.

        :rtype: ImmutableTagSet

        :raises ObjectDestroyedError: if used after destroyed.
        """
        try:
            ref = self._cached_tagset
        except AttributeError:
            tagset = None
        else:
            tagset = ref()
        if tagset is None:
            tagset = tags.ImmutableTagSet(
                self, '_thread_p', capi.lib.notmuch_thread_get_tags)
            self._cached_tagset = weakref.ref(tagset)
        return tagset


class ThreadIter(base.NotmuchIter):

    def __init__(self, parent, threads_p, *, db):
        self._db = db
        super().__init__(parent, threads_p,
                         fn_destroy=capi.lib.notmuch_threads_destroy,
                         fn_valid=capi.lib.notmuch_threads_valid,
                         fn_get=capi.lib.notmuch_threads_get,
                         fn_next=capi.lib.notmuch_threads_move_to_next)

    def __next__(self):
        thread_p = super().__next__()
        return Thread(self, thread_p, db=self._db)
