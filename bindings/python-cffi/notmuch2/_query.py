from notmuch2 import _base as base
from notmuch2 import _capi as capi
from notmuch2 import _errors as errors
from notmuch2 import _message as message
from notmuch2 import _thread as thread


__all__ = []


class Query(base.NotmuchObject):
    """Private, minimal query object.

    This is not meant for users and is not a full implementation of
    the query API.  It is only an intermediate used internally to
    match libnotmuch's memory management.
    """
    _query_p = base.MemoryPointer()

    def __init__(self, db, query_p):
        self._db = db
        self._query_p = query_p

    @property
    def alive(self):
        if not self._db.alive:
            return False
        try:
            self._query_p
        except errors.ObjectDestroyedError:
            return False
        else:
            return True

    def __del__(self):
        self._destroy()

    def _destroy(self):
        if self.alive:
            capi.lib.notmuch_query_destroy(self._query_p)
        self._query_p = None

    @property
    def query(self):
        """The query string as seen by libnotmuch."""
        q = capi.lib.notmuch_query_get_query_string(self._query_p)
        return base.BinString.from_cffi(q)

    def messages(self):
        """Return an iterator over all the messages found by the query.

        This executes the query and returns an iterator over the
        :class:`Message` objects found.
        """
        msgs_pp = capi.ffi.new('notmuch_messages_t**')
        ret = capi.lib.notmuch_query_search_messages(self._query_p, msgs_pp)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)
        return message.MessageIter(self, msgs_pp[0], db=self._db)

    def count_messages(self):
        """Return the number of messages matching this query."""
        count_p = capi.ffi.new('unsigned int *')
        ret = capi.lib.notmuch_query_count_messages(self._query_p, count_p)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)
        return count_p[0]

    def threads(self):
        """Return an iterator over all the threads found by the query."""
        threads_pp = capi.ffi.new('notmuch_threads_t **')
        ret = capi.lib.notmuch_query_search_threads(self._query_p, threads_pp)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)
        return thread.ThreadIter(self, threads_pp[0], db=self._db)

    def count_threads(self):
        """Return the number of threads matching this query."""
        count_p = capi.ffi.new('unsigned int *')
        ret = capi.lib.notmuch_query_count_threads(self._query_p, count_p)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)
        return count_p[0]
