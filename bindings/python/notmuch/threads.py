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

from .globals import (
    nmlib,
    Python3StringMixIn,
    NotmuchThreadP,
    NotmuchThreadsP,
)
from .errors import (
    NullPointerError,
    NotInitializedError,
)
from .thread import Thread

class Threads(Python3StringMixIn):
    """Represents a list of notmuch threads

    This object provides an iterator over a list of notmuch threads
    (Technically, it provides a wrapper for the underlying
    *notmuch_threads_t* structure). Do note that the underlying
    library only provides a one-time iterator (it cannot reset the
    iterator to the start). Thus iterating over the function will
    "exhaust" the list of threads, and a subsequent iteration attempt
    will raise a :exc:`NotInitializedError`. Also
    note, that any function that uses iteration will also
    exhaust the messages. So both::

      for thread in threads: print thread

    as well as::

       list_of_threads = list(threads)

    will "exhaust" the threads. If you need to re-iterate over a list of
    messages you will need to retrieve a new :class:`Threads` object.

    Things are not as bad as it seems though, you can store and reuse
    the single Thread objects as often as you want as long as you
    keep the parent Threads object around. (Recall that due to
    hierarchical memory allocation, all derived Threads objects will
    be invalid when we delete the parent Threads() object, even if it
    was already "exhausted".) So this works::

      db   = Database()
      threads = Query(db,'').search_threads() #get a Threads() object
      threadlist = []
      for thread in threads:
         threadlist.append(thread)

      # threads is "exhausted" now.
      # However it will be kept around until all retrieved Thread() objects are
      # also deleted. If you did e.g. an explicit del(threads) here, the
      # following lines would fail.

      # You can reiterate over *threadlist* however as often as you want.
      # It is simply a list with Thread objects.

      print (threadlist[0].get_thread_id())
      print (threadlist[1].get_thread_id())
      print (threadlist[0].get_total_messages())
    """

    #notmuch_threads_get
    _get = nmlib.notmuch_threads_get
    _get.argtypes = [NotmuchThreadsP]
    _get.restype = NotmuchThreadP

    def __init__(self, threads_p, parent=None):
        """
        :param threads_p:  A pointer to an underlying *notmuch_threads_t*
             structure. These are not publically exposed, so a user
             will almost never instantiate a :class:`Threads` object
             herself. They are usually handed back as a result,
             e.g. in :meth:`Query.search_threads`.  *threads_p* must be
             valid, we will raise an :exc:`NullPointerError` if it is
             `None`.
        :type threads_p: :class:`ctypes.c_void_p`
        :param parent: The parent object
             (ie :class:`Query`) these tags are derived from. It saves
             a reference to it, so we can automatically delete the db
             object once all derived objects are dead.
        :TODO: Make the iterator work more than once and cache the tags in
               the Python object.(?)
        """
        if not threads_p:
            raise NullPointerError()

        self._threads = threads_p
        #store parent, so we keep them alive as long as self  is alive
        self._parent = parent

    def __iter__(self):
        """ Make Threads an iterator """
        return self

    _valid = nmlib.notmuch_threads_valid
    _valid.argtypes = [NotmuchThreadsP]
    _valid.restype = bool

    _move_to_next = nmlib.notmuch_threads_move_to_next
    _move_to_next.argtypes = [NotmuchThreadsP]
    _move_to_next.restype = None

    def __next__(self):
        if not self._threads:
            raise NotInitializedError()

        if not self._valid(self._threads):
            self._threads = None
            raise StopIteration

        thread = Thread(Threads._get(self._threads), self)
        self._move_to_next(self._threads)
        return thread
    next = __next__ # python2.x iterator protocol compatibility

    def __nonzero__(self):
        '''
        Implement truth value testing. If __nonzero__ is not
        implemented, the python runtime would fall back to `len(..) >
        0` thus exhausting the iterator.

        :returns: True if the wrapped iterator has at least one more object
                  left.
        '''
        return self._threads and self._valid(self._threads)

    _destroy = nmlib.notmuch_threads_destroy
    _destroy.argtypes = [NotmuchThreadsP]
    _destroy.restype = None

    def __del__(self):
        """Close and free the notmuch Threads"""
        if self._threads:
            self._destroy(self._threads)
