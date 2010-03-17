import ctypes
from ctypes import c_int, c_char_p, c_void_p, c_uint64
from cnotmuch.globals import nmlib, STATUS, NotmuchError, Enum
import logging
from datetime import date

class Database(object):
    """Represents a notmuch database (wraps notmuch_database_t)

    .. note:: Do remember that as soon as we tear down this object,
           all underlying derived objects such as queries, threads,
           messages, tags etc will be freed by the underlying library
           as well. Accessing these objects will lead to segfaults and
           other unexpected behavior. See above for more details.
    """
    MODE = Enum(['READ_ONLY','READ_WRITE'])
    """Constants: Mode in which to open the database"""

    _std_db_path = None
    """Class attribute to cache user's default database"""

    """notmuch_database_get_path (notmuch_database_t *database)"""
    _get_path = nmlib.notmuch_database_get_path
    _get_path.restype = c_char_p

    """notmuch_database_open (const char *path, notmuch_database_mode_t mode)"""
    _open = nmlib.notmuch_database_open 
    _open.restype = c_void_p

    """ notmuch_database_find_message """
    _find_message = nmlib.notmuch_database_find_message
    _find_message.restype = c_void_p

    """notmuch_database_get_all_tags (notmuch_database_t *database)"""
    _get_all_tags = nmlib.notmuch_database_get_all_tags
    _get_all_tags.restype = c_void_p

    """ notmuch_database_create(const char *path):"""
    _create = nmlib.notmuch_database_create
    _create.restype = c_void_p

    def __init__(self, path=None, create=False, mode= MODE.READ_ONLY):
        """If *path* is *None*, we will try to read a users notmuch
        configuration and use his default database. If *create* is `True`,
        the database will always be created in
        :attr:`MODE`.READ_WRITE mode.

        :param path:   Directory to open/create the database in (see
                       above for behavior if `None`)
        :type path:    `str` or `None`
        :param create: Pass `False` to open an existing, `True` to create a new
                       database.  
        :type create:  bool
        :param mode:   Mode to open a database in. Is always 
                       :attr:`MODE`.READ_WRITE when creating a new one.
        :type mode:    :attr:`MODE`
        :returns:      Nothing
        :exception:    :exc:`NotmuchError` in case of failure.
        """
        self._db = None
        if path is None:
            # no path specified. use a user's default database
            if Database._std_db_path is None:
                #the following line throws a NotmuchError if it fails
                Database._std_db_path = self._get_user_default_db()
            path = Database._std_db_path

        if create == False:
            self.open(path, mode)
        else:
            self.create(path)

    def create(self, path):
        """Creates a new notmuch database

        This function is used by __init__() and usually does not need
        to be called directly. It wraps the underlying
        *notmuch_database_create* function and creates a new notmuch
        database at *path*. It will always return a database in
        :attr:`MODE`.READ_WRITE mode as creating an empty database for
        reading only does not make a great deal of sense.

        :param path: A directory in which we should create the database.
        :type path: str
        :returns: Nothing
        :exception: :exc:`NotmuchError` in case of any failure
                    (after printing an error message on stderr).
        """
        if self._db is not None:
            raise NotmuchError(
            message="Cannot create db, this Database() already has an open one.")

        res = Database._create(path, MODE.READ_WRITE)

        if res is None:
            raise NotmuchError(
                message="Could not create the specified database")
        self._db = res

    def open(self, path, mode= MODE.READ_ONLY): 
        """Opens an existing database

        This function is used by __init__() and usually does not need
        to be called directly. It wraps the underlying
        *notmuch_database_open* function.

        :param status: Open the database in read-only or read-write mode
        :type status:  :attr:`MODE` 
        :returns: Nothing
        :exception: Raises :exc:`NotmuchError` in case
                    of any failure (after printing an error message on stderr).
        """

        res = Database._open(path, mode)

        if res is None:
            raise NotmuchError(
                message="Could not open the specified database")
        self._db = res

    def get_path(self):
        """Returns the file path of an open database

        Wraps notmuch_database_get_path"""
        return Database._get_path(self._db)

    def find_message(self, msgid):
        """Returns a :class:`Message` as identified by its message ID

        Wraps the underlying *notmuch_database_find_message* function.

        :param msgid: The message ID
        :type msgid: string
        :returns: :class:`Message` or `None` if no message is found or if an
                  out-of-memory situation occurs.
        :exception: :exc:`NotmuchError` with STATUS.NOT_INITIALIZED if
                  the database was not intitialized.
        """
        if self._db is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        msg_p = Database._find_message(self._db, msgid)
        if msg_p is None:
            return None
        return Message(msg_p, self)

    def get_all_tags(self):
        """Returns :class:`Tags` with a list of all tags found in the database

        :returns: :class:`Tags`
        :execption: :exc:`NotmuchError` with STATUS.NULL_POINTER on error
        """
        if self._db is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        tags_p = Database._get_all_tags (self._db)
        if tags_p == None:
            raise NotmuchError(STATUS.NULL_POINTER)
        return Tags(tags_p, self)

    def __repr__(self):
        return "'Notmuch DB " + self.get_path() + "'"

    def __del__(self):
        """Close and free the notmuch database if needed"""
        if self._db is not None:
            logging.debug("Freeing the database now")
            nmlib.notmuch_database_close(self._db)

    def _get_user_default_db(self):
        """ Reads a user's notmuch config and returns his db location

        Throws a NotmuchError if it cannot find it"""
        from ConfigParser import SafeConfigParser
        import os.path
        config = SafeConfigParser()
        config.read(os.path.expanduser('~/.notmuch-config'))
        if not config.has_option('database','path'):
            raise NotmuchError(message=
                               "No DB path specified and no user default found")
        return config.get('database','path')

    @property
    def db_p(self):
        """Property returning a pointer to the notmuch_database_t or `None`.

        This should normally not be needed by a user."""
        return self._db

#------------------------------------------------------------------------------
class Query(object):
    """ Represents a search query on an opened :class:`Database`.

    A query selects and filters a subset of messages from the notmuch
    database we derive from.

    Technically, it wraps the underlying *notmuch_query_t* struct.

    .. note:: Do remember that as soon as we tear down this object,
           all underlying derived objects such as queries, threads,
           messages, tags etc will be freed by the underlying library
           as well. Accessing these objects will lead to segfaults and
           other unexpected behavior. See above for more details.
    """
    # constants
    SORT = Enum(['OLDEST_FIRST','NEWEST_FIRST','MESSAGE_ID'])
    """Constants: Sort order in which to return results"""

    """notmuch_query_create"""
    _create = nmlib.notmuch_query_create
    _create.restype = c_void_p

    """notmuch_query_search_messages"""
    _search_messages = nmlib.notmuch_query_search_messages
    _search_messages.restype = c_void_p

    def __init__(self, db, querystr):
        """
        :param db: An open database which we derive the Query from.
        :type db: :class:`Database`
        :param querystr: The query string for the message.
        :type querystr: str
        """
        self._db = None
        self._query = None
        self.create(db, querystr)

    def create(self, db, querystr):
        """Creates a new query derived from a Database.

        This function is utilized by __init__() and usually does not need to 
        be called directly.

        :param db: Database to create the query from.
        :type db: :class:`Database`
        :param querystr: The query string
        :type querystr: str
        :returns: Nothing
        :exception: :exc:`NotmuchError`

                      * STATUS.NOT_INITIALIZED if db is not inited
                      * STATUS.NULL_POINTER if the query creation failed 
                        (too little memory)
        """
        if db.db_p is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)            
        # create reference to parent db to keep it alive
        self._db = db
        
        # create query, return None if too little mem available
        query_p = Query._create(db.db_p, querystr)
        if query_p is None:
            NotmuchError(STATUS.NULL_POINTER)
        self._query = query_p

    def set_sort(self, sort):
        """Set the sort order future results will be delivered in

        Wraps the underlying *notmuch_query_set_sort* function.

        :param sort: Sort order (see :attr:`Query.SORT`)
        :returns: Nothing
        :exception: :exc:`NotmuchError` STATUS.NOT_INITIALIZED if query has not 
                    been initialized.
        """
        if self._query is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        nmlib.notmuch_query_set_sort(self._query, sort)

    def search_messages(self):
        """Filter messages according to query and return
        :class:`Messages` in the defined sort order.

        Technically, it wraps the underlying
        *notmuch_query_search_messages* function.

        :returns: :class:`Messages`
        :exception: :exc:`NotmuchError`

                      * STATUS.NOT_INITIALIZED if query is not inited
                      * STATUS.NULL_POINTER if search_messages failed 
        """
        if self._query is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)            

        msgs_p = Query._search_messages(self._query)

        if msgs_p is None:
            NotmuchError(STATUS.NULL_POINTER)

        return Messages(msgs_p,self)


    def __del__(self):
        """Close and free the Query"""
        if self._query is not None:
            logging.debug("Freeing the Query now")
            nmlib.notmuch_query_destroy (self._query)

#------------------------------------------------------------------------------
class Tags(object):
    """Represents a list of notmuch tags

    This object provides an iterator over a list of notmuch tags. Do
    note that the underlying library only provides a one-time iterator
    (it cannot reset the iterator to the start). Thus iterating over
    the function will "exhaust" the list of tags, and a subsequent
    iteration attempt will raise a :exc:`NotmuchError`
    STATUS.NOT_INITIALIZED. Also note, that any function that uses
    iteration (nearly all) will also exhaust the tags. So both::

      for tag in tags: print tag 

    as well as::

       number_of_tags = len(tags)

    and even a simple::

       #str() iterates over all tags to construct a space separated list
       print(str(tags))

    will "exhaust" the Tags. If you need to re-iterate over a list of
    tags you will need to retrieve a new :class:`Tags` object.
    """

    #notmuch_tags_get
    _get = nmlib.notmuch_tags_get
    _get.restype = c_char_p

    def __init__(self, tags_p, parent=None):
        """
        :param tags_p: A pointer to an underlying *notmuch_tags_t*
             structure. These are not publically exposed, so a user
             will almost never instantiate a :class:`Tags` object
             herself. They are usually handed back as a result,
             e.g. in :meth:`Database.get_all_tags`.  *tags_p* must be
             valid, we will raise an :exc:`NotmuchError`
             (STATUS.NULL_POINTER) if it is `None`.
        :type tags_p: :class:`ctypes.c_void_p`
        :param parent: The parent object (ie :class:`Database` or 
             :class:`Message` these tags are derived from, and saves a
             reference to it, so we can automatically delete the db object
             once all derived objects are dead.
        :TODO: Make the iterator optionally work more than once by
               cache the tags in the Python object(?)
        """
        if tags_p is None:
            NotmuchError(STATUS.NULL_POINTER)

        self._tags = tags_p
        #save reference to parent object so we keep it alive
        self._parent = parent
        logging.debug("Inited Tags derived from %s" %(repr(parent)))
    
    def __iter__(self):
        """ Make Tags an iterator """
        return self

    def next(self):
        if self._tags is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        if not nmlib.notmuch_tags_valid(self._tags):
            self._tags = None
            raise StopIteration

        tag = Tags._get (self._tags)
        nmlib.notmuch_tags_move_to_next(self._tags)
        return tag

    def __len__(self):
        """len(:class:`Tags`) returns the number of contained tags

        .. note:: As this iterates over the tags, we will not be able
               to iterate over them again (as in retrieve them)! If
               the tags have been exhausted already, this will raise a
               :exc:`NotmuchError` STATUS.NOT_INITIALIZED on
               subsequent attempts.
        """
        if self._tags is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        i=0
        while nmlib.notmuch_tags_valid(self._msgs):
            nmlib.notmuch_tags_move_to_next(self._msgs)
            i += 1
        self._tags = None
        return i

    def __str__(self):
        """The str() representation of Tags() is a space separated list of tags

        .. note:: As this iterates over the tags, we will not be able
               to iterate over them again (as in retrieve them)! If
               the tags have been exhausted already, this will raise a
               :exc:`NotmuchError` STATUS.NOT_INITIALIZED on
               subsequent attempts.
        """
        return " ".join(self)

    def __del__(self):
        """Close and free the notmuch tags"""
        if self._tags is not None:
            logging.debug("Freeing the Tags now")
            nmlib.notmuch_tags_destroy (self._tags)


#------------------------------------------------------------------------------
class Messages(object):
    """Represents a list of notmuch messages

    This object provides an iterator over a list of notmuch messages
    (Technically, it provides a wrapper for the underlying
    *notmuch_messages_t* structure). Do note that the underlying
    library only provides a one-time iterator (it cannot reset the
    iterator to the start). Thus iterating over the function will
    "exhaust" the list of messages, and a subsequent iteration attempt
    will raise a :exc:`NotmuchError` STATUS.NOT_INITIALIZED. Also
    note, that any function that uses iteration will also
    exhaust the messages. So both::

      for msg in msgs: print msg 

    as well as::

       number_of_msgs = len(msgs)

    will "exhaust" the Messages. If you need to re-iterate over a list of
    messages you will need to retrieve a new :class:`Messages` object.

    Things are not as bad as it seems though, you can store and reuse
    the single Message objects as often as you want as long as you
    keep the parent Messages object around. (Recall that due to
    hierarchical memory allocation, all derived Message objects will
    be invalid when we delete the parent Messages() object, even if it
    was already "exhausted".) So this works::

      db   = Database()
      msgs = Query(db,'').search_messages() #get a Messages() object
      msglist = []
      for m in msgs:
         msglist.append(m)

      # msgs is "exhausted" now and even len(msgs) will raise an exception.
      # However it will be kept around until all retrieved Message() objects are
      # also deleted. If you did e.g. an explicit del(msgs) here, the 
      # following lines would fail.
      
      # You can reiterate over *msglist* however as often as you want. 
      # It is simply a list with Message objects.

      print (msglist[0].get_filename())
      print (msglist[1].get_filename())
      print (msglist[0].get_message_id())
    """

    #notmuch_tags_get
    _get = nmlib.notmuch_messages_get
    _get.restype = c_void_p

    _collect_tags = nmlib.notmuch_messages_collect_tags
    _collect_tags.restype = c_void_p

    def __init__(self, msgs_p, parent=None):
        """
        :param msgs_p:  A pointer to an underlying *notmuch_messages_t*
             structure. These are not publically exposed, so a user
             will almost never instantiate a :class:`Messages` object
             herself. They are usually handed back as a result,
             e.g. in :meth:`Query.search_messages`.  *msgs_p* must be
             valid, we will raise an :exc:`NotmuchError`
             (STATUS.NULL_POINTER) if it is `None`.
        :type msgs_p: :class:`ctypes.c_void_p`
        :param parent: The parent object
             (ie :class:`Query`) these tags are derived from. It saves
             a reference to it, so we can automatically delete the db
             object once all derived objects are dead.
        :TODO: Make the iterator work more than once and cache the tags in 
               the Python object.(?)
        """
        if msgs_p is None:
            NotmuchError(STATUS.NULL_POINTER)

        self._msgs = msgs_p
        #store parent, so we keep them alive as long as self  is alive
        self._parent = parent
        logging.debug("Inited Messages derived from %s" %(str(parent)))

    def collect_tags(self):
        """Return the unique :class:`Tags` in the contained messages

        :returns: :class:`Tags`
        :exceptions: :exc:`NotmuchError` STATUS.NOT_INITIALIZED if not inited

        .. note:: :meth:`collect_tags` will iterate over the messages and
          therefore will not allow further iterations.
        """
        if self._msgs is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        # collect all tags (returns NULL on error)
        tags_p = Messages._collect_tags (self._msgs)
        #reset _msgs as we iterated over it and can do so only once
        self._msgs = None

        if tags_p == None:
            raise NotmuchError(STATUS.NULL_POINTER)
        return Tags(tags_p, self)

    def __iter__(self):
        """ Make Messages an iterator """
        return self

    def next(self):
        if self._msgs is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        if not nmlib.notmuch_messages_valid(self._msgs):
            self._msgs = None
            raise StopIteration

        msg = Message(Messages._get (self._msgs), self)
        nmlib.notmuch_messages_move_to_next(self._msgs)
        return msg

    def __len__(self):
        """len(:class:`Messages`) returns the number of contained messages

        .. note:: As this iterates over the messages, we will not be able to 
               iterate over them again (as in retrieve them)!
        """
        if self._msgs is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        i=0
        while nmlib.notmuch_messages_valid(self._msgs):
            nmlib.notmuch_messages_move_to_next(self._msgs)
            i += 1
        self._msgs = None
        return i



    def __del__(self):
        """Close and free the notmuch Messages"""
        if self._msgs is not None:
            logging.debug("Freeing the Messages now")
            nmlib.notmuch_messages_destroy (self._msgs)


#------------------------------------------------------------------------------
class Message(object):
    """Wrapper around notmuch_message_t"""

    """notmuch_message_get_filename (notmuch_message_t *message)"""
    _get_filename = nmlib.notmuch_message_get_filename
    _get_filename.restype = c_char_p 
    """notmuch_message_get_message_id (notmuch_message_t *message)"""
    _get_message_id = nmlib.notmuch_message_get_message_id
    _get_message_id.restype = c_char_p 

    """notmuch_message_get_tags (notmuch_message_t *message)"""
    _get_tags = nmlib.notmuch_message_get_tags
    _get_tags.restype = c_void_p

    _get_date = nmlib.notmuch_message_get_date
    _get_date.restype = c_uint64

    _get_header = nmlib.notmuch_message_get_header
    _get_header.restype = c_char_p

    def __init__(self, msg_p, parent=None):
        """
        msg_p is a pointer to an notmuch_message_t Structure. If it is None,
        we will raise an NotmuchError(STATUS.NULL_POINTER).

        Is a 'parent' object is passed which this message is derived from,
        we save a reference to it, so we can automatically delete the parent
        object once all derived objects are dead.
        """
        if msg_p is None:
            NotmuchError(STATUS.NULL_POINTER)
        self._msg = msg_p
        #keep reference to parent, so we keep it alive
        self._parent = parent
        logging.debug("Inited Message derived from %s" %(str(parent)))


    def get_message_id(self):
        """ return the msg id
        
        Raises NotmuchError(STATUS.NOT_INITIALIZED) if not inited
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        return Message._get_message_id(self._msg)

    def get_date(self):
        """returns time_t of the message date

        For the original textual representation of the Date header from the
        message call notmuch_message_get_header() with a header value of
        "date".
        Raises NotmuchError(STATUS.NOT_INITIALIZED) if not inited
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        return Message._get_date(self._msg)

    def get_header(self, header):
        """ TODO document me"""
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        #Returns NULL if any error occurs.
        header = Message._get_header (self._msg, header)
        if header == None:
            raise NotmuchError(STATUS.NULL_POINTER)
        return header

    def get_filename(self):
        """ return the msg filename
        
        Raises NotmuchError(STATUS.NOT_INITIALIZED) if not inited
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        return Message._get_filename(self._msg)

    def get_tags(self):
        """ return the msg tags
        
        Raises NotmuchError(STATUS.NOT_INITIALIZED) if not inited
        Raises NotmuchError(STATUS.NULL_POINTER) on error.
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        tags_p = Message._get_tags(self._msg)
        if tags_p == None:
            raise NotmuchError(STATUS.NULL_POINTER)
        return Tags(tags_p, self)

    def __str__(self):
        """A message() is represented by a 1-line summary"""
        msg = {}
        msg['from'] = self.get_header('from')
        msg['tags'] = str(self.get_tags())
        msg['date'] = date.fromtimestamp(self.get_date())
        return "%(from)s (%(date)s) (%(tags)s)" % (msg)

    def format_as_text(self):
        """ Output like notmuch show """
        return str(self)

    def __del__(self):
        """Close and free the notmuch Message"""
        if self._msg is not None:
            logging.debug("Freeing the Message now")
            nmlib.notmuch_message_destroy (self._msg)
