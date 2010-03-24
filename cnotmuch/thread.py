from ctypes import c_char_p, c_void_p, c_uint64
from cnotmuch.globals import nmlib, STATUS, NotmuchError, Enum
from cnotmuch.tags import Tags
from datetime import date

#------------------------------------------------------------------------------
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
    _get.restype = c_void_p

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
            NotmuchError(STATUS.NULL_POINTER)

        self._threads = threads_p
        #store parent, so we keep them alive as long as self  is alive
        self._parent = parent

    def __iter__(self):
        """ Make Threads an iterator """
        return self

    def next(self):
        if self._threads is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        if not nmlib.notmuch_threads_valid(self._threads):
            self._threads = None
            raise StopIteration

        thread = Thread(Threads._get (self._threads), self)
        nmlib.notmuch_threads_move_to_next(self._threads)
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

        i=0
        # returns 'bool'. On out-of-memory it returns None
        while nmlib.notmuch_threads_valid(self._threads):
            nmlib.notmuch_threads_move_to_next(self._threads)
            i += 1
        # reset self._threads to mark as "exhausted"
        self._threads = None
        return i



    def __del__(self):
        """Close and free the notmuch Threads"""
        if self._threads is not None:
            nmlib.notmuch_messages_destroy (self._threads)

#------------------------------------------------------------------------------
class Thread(object):
    """Represents a single message thread."""

    """notmuch_thread_get_thread_id"""
    _get_thread_id = nmlib.notmuch_thread_get_thread_id
    _get_thread_id.restype = c_char_p

    """notmuch_thread_get_authors"""
    _get_authors = nmlib.notmuch_thread_get_authors
    _get_authors.restype = c_char_p

    """notmuch_thread_get_subject"""
    _get_subject = nmlib.notmuch_thread_get_subject
    _get_subject.restype = c_char_p

    _get_newest_date = nmlib.notmuch_thread_get_newest_date
    _get_newest_date.restype = c_uint64

    _get_oldest_date = nmlib.notmuch_thread_get_oldest_date
    _get_oldest_date.restype = c_uint64

    """notmuch_thread_get_tags"""
    _get_tags = nmlib.notmuch_thread_get_tags
    _get_tags.restype = c_void_p

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
            NotmuchError(STATUS.NULL_POINTER)
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
        return nmlib.notmuch_thread_get_total_messages(self._thread)


    ###TODO: notmuch_messages_t * notmuch_thread_get_toplevel_messages (notmuch_thread_t *thread);

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
        return nmlib.notmuch_thread_get_matched_messages(self._thread)

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
        return Thread._get_authors(self._thread)

    def get_subject(self):
        """Returns the Subject of 'thread'

        The returned string belongs to 'thread' and will only be valid for 
        as long as this Thread() is not deleted.
        """
        if self._thread is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        return Thread._get_subject(self._thread)

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

        return "thread:%(id)s %(date)12s [%(matched)d/%(total)d] %(authors)s; %(subject)s (%(tags)s)" % (thread)

    def __del__(self):
        """Close and free the notmuch Thread"""
        if self._thread is not None:
            nmlib.notmuch_thread_destroy (self._thread)
