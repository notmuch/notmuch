import ctypes
from ctypes import c_int, c_char_p, c_void_p, c_uint64
from cnotmuch.globals import nmlib, STATUS, NotmuchError
import logging
from datetime import date

class Database(object):
    """ Wrapper around a notmuch_database_t

    Do note that as soon as we tear down this object, all derived queries,
    threads, and messages will be freed as well.
    """
    #constants
    MODE_READ_ONLY = 0
    MODE_READ_WRITE = 1

    _std_db_path = None
    """Class attribute of users default database"""

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

    def __init__(self, path=None, create=False, status= MODE_READ_ONLY):
        """ Open or create a notmuch database

        If path is None, we will try to read a users notmuch configuration and
        use his default database.
        Throws a NotmuchError in case of failure.
        """
        self._db = None
        if path is None:
            # no path specified. use a user's default database
            if Database._std_db_path is None:
                #the following line throws a NotmuchError if it fails
                Database._std_db_path = self._get_user_default_db()
            path = Database._std_db_path

        if create == False:
            self.open(path, status)
        else:
            self.create(path, status)

    def create(self, path, status=MODE_READ_ONLY):
        """ notmuch_database_create(const char *path)

        :returns: Raises :exc:`notmuch.NotmuchError` in case
                  of any failure (after printing an error message on stderr).
        """
        res = Database._create(path, status)

        if res is None:
            raise NotmuchError(
                message="Could not create the specified database")
        self._db = res

    def open(self, path, status= MODE_READ_ONLY): 
        """calls notmuch_database_open

        :returns: Raises :exc:`notmuch.NotmuchError` in case
                  of any failure (after printing an error message on stderr).
        """
        res = Database._open(path, status)

        if res is None:
            raise NotmuchError(
                message="Could not open the specified database")
        self._db = res

    def get_path(self):
        """notmuch_database_get_path (notmuch_database_t *database);  """
        return Database._get_path(self._db)

    def find_message(self, msgid):
        """notmuch_database_find_message
        :param msgid: The message id
        :ptype msgid: string

        :returns: Message() or None if no message is found or if an
                  out-of-memory situation occurs.
        """
        if self._db is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        msg_p = Database._find_message(self._db, msgid)
        if msg_p is None:
            return None
        return Message(msg_p, self)

    def get_all_tags(self):
        """Return a Tags() object (list of all tags found in the database)

        :returns: Tags() object or raises :exc:`NotmuchError` with 
                  STATUS.NULL_POINTER on error
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
        """Returns a pointer to the current notmuch_database_t or None"""
        return self._db

#------------------------------------------------------------------------------
class Query(object):
    """ Wrapper around a notmuch_query_t

    Do note that as soon as we tear down this object, all derived
    threads, and messages will be freed as well.
    """
    # constants
    SORT_OLDEST_FIRST = 0
    SORT_NEWEST_FIRST = 1
    SORT_MESSAGE_ID = 2

    """notmuch_query_create"""
    _create = nmlib.notmuch_query_create
    _create.restype = c_void_p

    """notmuch_query_search_messages"""
    _search_messages = nmlib.notmuch_query_search_messages
    _search_messages.restype = c_void_p

    def __init__(self, db, querystr):
        """TODO: document"""
        self._db = None
        self._query = None
        self.create(db, querystr)

    def create(self, db, querystr):
        """db is a Database() and querystr a string

        raises NotmuchError STATUS.NOT_INITIALIZED if db is not inited and
        STATUS.NULL_POINTER if the query creation failed (too little memory)
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
        """notmuch_query_set_sort

        :param sort: one of Query.SORT_OLDEST_FIRST|SORT_NEWEST_FIRST|SORT_MESSAGE_ID
        :returns: Nothing, but raises NotmuchError if query is not inited
        """
        if self._query is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        nmlib.notmuch_query_set_sort(self._query, sort)

    def search_messages(self):
        """notmuch_query_search_messages
        Returns Messages() or a raises a NotmuchError()
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
    """Wrapper around notmuch_tags_t"""

    #notmuch_tags_get
    _get = nmlib.notmuch_tags_get
    _get.restype = c_char_p

    def __init__(self, tags_p, parent=None):
        """
        msg_p is a pointer to an notmuch_message_t Structure. If it is None,
        we will raise an NotmuchError(STATUS.NULL_POINTER).

        Is passed the parent these tags are derived from, and saves a
        reference to it, so we can automatically delete the db object
        once all derived objects are dead.

        Tags() provides an iterator over all contained tags. However, you will
        only be able to iterate over the Tags once, because the underlying C
        function only allows iterating once.
        #TODO: make the iterator work more than once and cache the tags in 
               the Python object.
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

    def __str__(self):
        """str() of Tags() is a space separated list of tags

        This iterates over the list of Tags and will therefore 'exhaust' Tags()
        """
        return " ".join(self)

    def __del__(self):
        """Close and free the notmuch tags"""
        if self._tags is not None:
            logging.debug("Freeing the Tags now")
            nmlib.notmuch_tags_destroy (self._tags)


#------------------------------------------------------------------------------
class Messages(object):
    """Wrapper around notmuch_messages_t"""

    #notmuch_tags_get
    _get = nmlib.notmuch_messages_get
    _get.restype = c_void_p

    _collect_tags = nmlib.notmuch_messages_collect_tags
    _collect_tags.restype = c_void_p

    def __init__(self, msgs_p, parent=None):
        """
        msg_p is a pointer to an notmuch_messages_t Structure. If it is None,
        we will raise an NotmuchError(STATUS.NULL_POINTER).

        If passed the parent query this Messages() is derived from, it saves a
        reference to it, so we can automatically delete the parent query object
        once all derived objects are dead.

        Messages() provides an iterator over all contained Message()s.
        However, you will only be able to iterate over it once,
        because the underlying C function only allows iterating once.
        #TODO: make the iterator work more than once and cache the tags in 
               the Python object.
        """
        if msgs_p is None:
            NotmuchError(STATUS.NULL_POINTER)

        self._msgs = msgs_p
        #store parent, so we keep them alive as long as self  is alive
        self._parent = parent
        logging.debug("Inited Messages derived from %s" %(str(parent)))

    def collect_tags(self):
        """ return the Tags() belonging to the messages
        
        Do note that collect_tags will iterate over the messages and
        therefore will not allow further iterationsl
        Raises NotmuchError(STATUS.NOT_INITIALIZED) if not inited
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
        """ Returns the number of contained messages

        :note: As this iterates over the messages, we will not be able to 
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
