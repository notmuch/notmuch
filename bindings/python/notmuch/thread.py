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
along with notmuch.  If not, see <http://www.gnu.org/licenses/>.

Copyright 2010 Sebastian Spaeth <Sebastian@SSpaeth.de>'
"""

from ctypes import c_char_p, c_long, c_int
from notmuch.globals import (nmlib, STATUS,
    NotmuchError, NotmuchThreadP, NotmuchThreadsP, NotmuchMessagesP,
    NotmuchTagsP,)
from notmuch.message import Messages
from notmuch.tag import Tags
from datetime import date


class Threads(object):
    """Represents a list of notmuch threads

    This object provides an iterator over a list of notmuch threads
    (Technically, it provides a wrapper for the underlying
    *notmuch_threads_t* structure). Do note that the underlying
    library only provides a one-time iterator (it cannot reset the
    iterator to the start). Thus iterating over the function will
    "exhaust" the list of threads, and a subsequent iteration attempt
    will raise a :exc:`NotmuchError` STATUS.NOT_INITIALIZED. Also
    note, that any function that uses iteration will also
    exhaust the messages. So both::

      for thread in threads: print thread

    as well as::

       number_of_msgs = len(threads)

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

      # threads is "exhausted" now and even len(threads) will raise an
      # exception.
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
             valid, we will raise an :exc:`NotmuchError`
             (STATUS.NULL_POINTER) if it is `None`.
        :type threads_p: :class:`ctypes.c_void_p`
        :param parent: The parent object
             (ie :class:`Query`) these tags are derived from. It saves
             a reference to it, so we can automatically delete the db
             object once all derived objects are dead.
        :TODO: Make the iterator work more than once and cache the tags in
               the Python object.(?)
        """
        if threads_p is None:
            raise NotmuchError(STATUS.NULL_POINTER)

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

    def next(self):
        if self._threads is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        if not self._valid(self._threads):
            self._threads = None
            raise StopIteration

        thread = Thread(Threads._get(self._threads), self)
        self._move_to_next(self._threads)
        return thread

    def __len__(self):
        """len(:class:`Threads`) returns the number of contained Threads

        .. note:: As this iterates over the threads, we will not be able to
               iterate over them again! So this will fail::

                 #THIS FAILS
                 threads = Database().create_query('').search_threads()
                 if len(threads) > 0:              #this 'exhausts' threads
                     # next line raises NotmuchError(STATUS.NOT_INITIALIZED)!!!
                     for thread in threads: print thread
        """
        if self._threads is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        i = 0
        # returns 'bool'. On out-of-memory it returns None
        while self._valid(self._threads):
            self._move_to_next(self._threads)
            i += 1
        # reset self._threads to mark as "exhausted"
        self._threads = None
        return i

    def __nonzero__(self):
        """Check if :class:`Threads` contains at least one more valid thread

        The existence of this function makes 'if Threads: foo' work, as
        that will implicitely call len() exhausting the iterator if
        __nonzero__ does not exist. This function makes `bool(Threads())`
        work repeatedly.

        :return: True if there is at least one more thread in the
           Iterator, False if not. None on a "Out-of-memory" error.
        """
        return self._threads is not None and \
            self._valid(self._threads) > 0

    _destroy = nmlib.notmuch_threads_destroy
    _destroy.argtypes = [NotmuchThreadsP]
    _destroy.argtypes = None

    def __del__(self):
        """Close and free the notmuch Threads"""
        if self._threads is not None:
            self._destroy(self._threads)


class Thread(object):
    """Represents a single message thread."""

    """notmuch_thread_get_thread_id"""
    _get_thread_id = nmlib.notmuch_thread_get_thread_id
    _get_thread_id.argtypes = [NotmuchThreadP]
    _get_thread_id.restype = c_char_p

    """notmuch_thread_get_authors"""
    _get_authors = nmlib.notmuch_thread_get_authors
    _get_authors.argtypes = [NotmuchThreadP]
    _get_authors.restype = c_char_p

    """notmuch_thread_get_subject"""
    _get_subject = nmlib.notmuch_thread_get_subject
    _get_subject.argtypes = [NotmuchThreadP]
    _get_subject.restype = c_char_p

    """notmuch_thread_get_toplevel_messages"""
    _get_toplevel_messages = nmlib.notmuch_thread_get_toplevel_messages
    _get_toplevel_messages.argtypes = [NotmuchThreadP]
    _get_toplevel_messages.restype = NotmuchMessagesP

    _get_newest_date = nmlib.notmuch_thread_get_newest_date
    _get_newest_date.argtypes = [NotmuchThreadP]
    _get_newest_date.restype = c_long

    _get_oldest_date = nmlib.notmuch_thread_get_oldest_date
    _get_oldest_date.argtypes = [NotmuchThreadP]
    _get_oldest_date.restype = c_long

    """notmuch_thread_get_tags"""
    _get_tags = nmlib.notmuch_thread_get_tags
    _get_tags.argtypes = [NotmuchThreadP]
    _get_tags.restype = NotmuchTagsP

    def __init__(self, thread_p, parent=None):
        """
        :param thread_p: A pointer to an internal notmuch_thread_t
            Structure.  These are not publically exposed, so a user
            will almost never instantiate a :class:`Thread` object
            herself. They are usually handed back as a result,
            e.g. when iterating through :class:`Threads`. *thread_p*
            must be valid, we will raise an :exc:`NotmuchError`
            (STATUS.NULL_POINTER) if it is `None`.

        :param parent: A 'parent' object is passed which this message is
              derived from. We save a reference to it, so we can
              automatically delete the parent object once all derived
              objects are dead.
        """
        if thread_p is None:
            raise NotmuchError(STATUS.NULL_POINTER)
        self._thread = thread_p
        #keep reference to parent, so we keep it alive
        self._parent = parent

    def get_thread_id(self):
        """Get the thread ID of 'thread'

        The returned string belongs to 'thread' and will only be valid
        for as long as the thread is valid.

        :returns: String with a message ID
        :exception: :exc:`NotmuchError` STATUS.NOT_INITIALIZED if the thread
                    is not initialized.
        """
        if self._thread is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        return Thread._get_thread_id(self._thread)

    _get_total_messages = nmlib.notmuch_thread_get_total_messages
    _get_total_messages.argtypes = [NotmuchThreadP]
    _get_total_messages.restype = c_int

    def get_total_messages(self):
        """Get the total number of messages in 'thread'

        :returns: The number of all messages in the database
                  belonging to this thread. Contrast with
                  :meth:`get_matched_messages`.
        :exception: :exc:`NotmuchError` STATUS.NOT_INITIALIZED if the thread
                    is not initialized.
        """
        if self._thread is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        return self._get_total_messages(self._thread)

    def get_toplevel_messages(self):
        """Returns a :class:`Messages` iterator for the top-level messages in
           'thread'

           This iterator will not necessarily iterate over all of the messages
           in the thread. It will only iterate over the messages in the thread
           which are not replies to other messages in the thread.

           To iterate over all messages in the thread, the caller will need to
           iterate over the result of :meth:`Message.get_replies` for each
           top-level message (and do that recursively for the resulting
           messages, etc.).

        :returns: :class:`Messages`
        :exception: :exc:`NotmuchError`

                      * STATUS.NOT_INITIALIZED if query is not inited
                      * STATUS.NULL_POINTER if search_messages failed
        """
        if self._thread is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        msgs_p = Thread._get_toplevel_messages(self._thread)

        if msgs_p is None:
            raise NotmuchError(STATUS.NULL_POINTER)

        return Messages(msgs_p, self)

    _get_matched_messages = nmlib.notmuch_thread_get_matched_messages
    _get_matched_messages.argtypes = [NotmuchThreadP]
    _get_matched_messages.restype = c_int

    def get_matched_messages(self):
        """Returns the number of messages in 'thread' that matched the query

        :returns: The number of all messages belonging to this thread that
                  matched the :class:`Query`from which this thread was created.
                  Contrast with :meth:`get_total_messages`.
        :exception: :exc:`NotmuchError` STATUS.NOT_INITIALIZED if the thread
                    is not initialized.
        """
        if self._thread is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        return self._get_matched_messages(self._thread)

    def get_authors(self):
        """Returns the authors of 'thread'

        The returned string is a comma-separated list of the names of the
        authors of mail messages in the query results that belong to this
        thread.

        The returned string belongs to 'thread' and will only be valid for
        as long as this Thread() is not deleted.
        """
        if self._thread is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        authors = Thread._get_authors(self._thread)
        if authors is None:
            return None
        return authors.decode('UTF-8')

    def get_subject(self):
        """Returns the Subject of 'thread'

        The returned string belongs to 'thread' and will only be valid for
        as long as this Thread() is not deleted.
        """
        if self._thread is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        subject = Thread._get_subject(self._thread)
        if subject is None:
            return None
        return subject.decode('UTF-8')

    def get_newest_date(self):
        """Returns time_t of the newest message date

        :returns: A time_t timestamp.
        :rtype: c_unit64
        :exception: :exc:`NotmuchError` STATUS.NOT_INITIALIZED if the message
                    is not initialized.
        """
        if self._thread is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        return Thread._get_newest_date(self._thread)

    def get_oldest_date(self):
        """Returns time_t of the oldest message date

        :returns: A time_t timestamp.
        :rtype: c_unit64
        :exception: :exc:`NotmuchError` STATUS.NOT_INITIALIZED if the message
                    is not initialized.
        """
        if self._thread is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        return Thread._get_oldest_date(self._thread)

    def get_tags(self):
        """ Returns the message tags

        In the Notmuch database, tags are stored on individual
        messages, not on threads. So the tags returned here will be all
        tags of the messages which matched the search and which belong to
        this thread.

        The :class:`Tags` object is owned by the thread and as such, will only
        be valid for as long as this :class:`Thread` is valid (e.g. until the
        query from which it derived is explicitely deleted).

        :returns: A :class:`Tags` iterator.
        :exception: :exc:`NotmuchError`

                      * STATUS.NOT_INITIALIZED if the thread
                        is not initialized.
                      * STATUS.NULL_POINTER, on error
        """
        if self._thread is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        tags_p = Thread._get_tags(self._thread)
        if tags_p == None:
            raise NotmuchError(STATUS.NULL_POINTER)
        return Tags(tags_p, self)

    def __str__(self):
        """A str(Thread()) is represented by a 1-line summary"""
        thread = {}
        thread['id'] = self.get_thread_id()

        ###TODO: How do we find out the current sort order of Threads?
        ###Add a "sort" attribute to the Threads() object?
        #if (sort == NOTMUCH_SORT_OLDEST_FIRST)
        #         date = notmuch_thread_get_oldest_date (thread);
        #else
        #         date = notmuch_thread_get_newest_date (thread);
        thread['date'] = date.fromtimestamp(self.get_newest_date())
        thread['matched'] = self.get_matched_messages()
        thread['total'] = self.get_total_messages()
        thread['authors'] = self.get_authors()
        thread['subject'] = self.get_subject()
        thread['tags'] = self.get_tags()

        return "thread:%s %12s [%d/%d] %s; %s (%s)" % (thread['id'],
                                                       thread['date'],
                                                       thread['matched'],
                                                       thread['total'],
                                                       thread['authors'],
                                                       thread['subject'],
                                                       thread['tags'])

    _destroy = nmlib.notmuch_thread_destroy
    _destroy.argtypes = [NotmuchThreadP]
    _destroy.restype = None

    def __del__(self):
        """Close and free the notmuch Thread"""
        if self._thread is not None:
            self._destroy(self._thread)
