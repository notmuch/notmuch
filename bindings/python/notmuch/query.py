"""
This file is part of notmuch.

Notmuch is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

Notmuch is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with notmuch.  If not, see <https://www.gnu.org/licenses/>.

Copyright 2010 Sebastian Spaeth <Sebastian@SSpaeth.de>
"""

from ctypes import c_char_p, c_uint, POINTER, byref
from .globals import (
    nmlib,
    Enum,
    _str,
    NotmuchQueryP,
    NotmuchThreadsP,
    NotmuchDatabaseP,
    NotmuchMessagesP,
)
from .errors import (
    NotmuchError,
    NullPointerError,
    NotInitializedError,
)
from .threads import Threads
from .messages import Messages


class Query(object):
    """Represents a search query on an opened :class:`Database`.

    A query selects and filters a subset of messages from the notmuch
    database we derive from.

    :class:`Query` provides an instance attribute :attr:`sort`, which
    contains the sort order (if specified via :meth:`set_sort`) or
    `None`.

    Any function in this class may throw an :exc:`NotInitializedError`
    in case the underlying query object was not set up correctly.

    .. note:: Do remember that as soon as we tear down this object,
           all underlying derived objects such as threads,
           messages, tags etc will be freed by the underlying library
           as well. Accessing these objects will lead to segfaults and
           other unexpected behavior. See above for more details.
    """
    # constants
    SORT = Enum(['OLDEST_FIRST', 'NEWEST_FIRST', 'MESSAGE_ID', 'UNSORTED'])
    """Constants: Sort order in which to return results"""

    def __init__(self, db, querystr):
        """
        :param db: An open database which we derive the Query from.
        :type db: :class:`Database`
        :param querystr: The query string for the message.
        :type querystr: utf-8 encoded str or unicode
        """
        self._db = None
        self._query = None
        self.sort = None
        self.create(db, querystr)

    def _assert_query_is_initialized(self):
        """Raises :exc:`NotInitializedError` if self._query is `None`"""
        if not self._query:
            raise NotInitializedError()

    """notmuch_query_create"""
    _create = nmlib.notmuch_query_create
    _create.argtypes = [NotmuchDatabaseP, c_char_p]
    _create.restype = NotmuchQueryP

    def create(self, db, querystr):
        """Creates a new query derived from a Database

        This function is utilized by __init__() and usually does not need to
        be called directly.

        :param db: Database to create the query from.
        :type db: :class:`Database`
        :param querystr: The query string
        :type querystr: utf-8 encoded str or unicode
        :raises:
            :exc:`NullPointerError` if the query creation failed
                (e.g. too little memory).
            :exc:`NotInitializedError` if the underlying db was not
                intitialized.
        """
        db._assert_db_is_initialized()
        # create reference to parent db to keep it alive
        self._db = db
        # create query, return None if too little mem available
        query_p = Query._create(db._db, _str(querystr))
        if not query_p:
            raise NullPointerError
        self._query = query_p

    _set_sort = nmlib.notmuch_query_set_sort
    _set_sort.argtypes = [NotmuchQueryP, c_uint]
    _set_sort.argtypes = None

    def set_sort(self, sort):
        """Set the sort order future results will be delivered in

        :param sort: Sort order (see :attr:`Query.SORT`)
        """
        self._assert_query_is_initialized()
        self.sort = sort
        self._set_sort(self._query, sort)

    _exclude_tag = nmlib.notmuch_query_add_tag_exclude
    _exclude_tag.argtypes = [NotmuchQueryP, c_char_p]
    _exclude_tag.resttype = None

    def exclude_tag(self, tagname):
        """Add a tag that will be excluded from the query results by default.

        This exclusion will be overridden if this tag appears explicitly in the
        query.

        :param tagname: Name of the tag to be excluded
        """
        self._assert_query_is_initialized()
        self._exclude_tag(self._query, _str(tagname))

    """notmuch_query_search_threads"""
    _search_threads = nmlib.notmuch_query_search_threads
    _search_threads.argtypes = [NotmuchQueryP, POINTER(NotmuchThreadsP)]
    _search_threads.restype = c_uint

    def search_threads(self):
        """Execute a query for threads

        Execute a query for threads, returning a :class:`Threads` iterator.
        The returned threads are owned by the query and as such, will only be
        valid until the Query is deleted.

        The method sets :attr:`Message.FLAG`\.MATCH for those messages that
        match the query. The method :meth:`Message.get_flag` allows us
        to get the value of this flag.

        :returns: :class:`Threads`
        :raises: :exc:`NullPointerError` if search_threads failed
        """
        self._assert_query_is_initialized()
        threads_p = NotmuchThreadsP() # == NULL
        status = Query._search_threads(self._query, byref(threads_p))
        if status != 0:
            raise NotmuchError(status)

        if not threads_p:
            raise NullPointerError
        return Threads(threads_p, self)

    """notmuch_query_search_messages_st"""
    _search_messages = nmlib.notmuch_query_search_messages
    _search_messages.argtypes = [NotmuchQueryP, POINTER(NotmuchMessagesP)]
    _search_messages.restype = c_uint

    def search_messages(self):
        """Filter messages according to the query and return
        :class:`Messages` in the defined sort order

        :returns: :class:`Messages`
        :raises: :exc:`NullPointerError` if search_messages failed
        """
        self._assert_query_is_initialized()
        msgs_p = NotmuchMessagesP() # == NULL
        status = Query._search_messages(self._query, byref(msgs_p))
        if status != 0:
            raise NotmuchError(status)

        if not msgs_p:
            raise NullPointerError
        return Messages(msgs_p, self)

    _count_messages = nmlib.notmuch_query_count_messages
    _count_messages.argtypes = [NotmuchQueryP, POINTER(c_uint)]
    _count_messages.restype = c_uint

    def count_messages(self):
        '''
        This function performs a search and returns Xapian's best
        guess as to the number of matching messages.

        :returns: the estimated number of messages matching this query
        :rtype:   int
        '''
        self._assert_query_is_initialized()
        count = c_uint(0)
        status = Query._count_messages(self._query, byref(count))
        if status != 0:
            raise NotmuchError(status)
        return count.value

    _count_threads = nmlib.notmuch_query_count_threads
    _count_threads.argtypes = [NotmuchQueryP, POINTER(c_uint)]
    _count_threads.restype = c_uint

    def count_threads(self):
        '''
        This function performs a search and returns the number of
        unique thread IDs in the matching messages. This is the same
        as number of threads matching a search.

        Note that this is a significantly heavier operation than
        meth:`Query.count_messages`.

        :returns: the number of threads returned by this query
        :rtype:   int
        '''
        self._assert_query_is_initialized()
        count = c_uint(0)
        status = Query._count_threads(self._query, byref(count))
        if status != 0:
            raise NotmuchError(status)
        return count.value

    _destroy = nmlib.notmuch_query_destroy
    _destroy.argtypes = [NotmuchQueryP]
    _destroy.restype = None

    def __del__(self):
        """Close and free the Query"""
        if self._query:
            self._destroy(self._query)
