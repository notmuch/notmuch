import ctypes, os
from ctypes import c_int, c_char_p, c_void_p, c_uint, byref
from cnotmuch.globals import nmlib, STATUS, NotmuchError, Enum
from cnotmuch.thread import Threads
from cnotmuch.message import Messages
from cnotmuch.tag import Tags

class Database(object):
    """Represents a notmuch database (wraps notmuch_database_t)

    .. note:: Do remember that as soon as we tear down this object,
           all underlying derived objects such as queries, threads,
           messages, tags etc will be freed by the underlying library
           as well. Accessing these objects will lead to segfaults and
           other unexpected behavior. See above for more details.
    """
    _std_db_path = None
    """Class attribute to cache user's default database"""

    MODE = Enum(['READ_ONLY','READ_WRITE'])
    """Constants: Mode in which to open the database"""

    """notmuch_database_get_path (notmuch_database_t *database)"""
    _get_path = nmlib.notmuch_database_get_path
    _get_path.restype = c_char_p

    """notmuch_database_get_version"""
    _get_version = nmlib.notmuch_database_get_version
    _get_version.restype = c_uint

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

    def __init__(self, path=None, create=False, mode= 0):
        """If *path* is *None*, we will try to read a users notmuch 
        configuration and use his configured database. The location of the 
        configuration file can be specified through the environment variable
        *NOTMUCH_CONFIG*, falling back to the default `~/.notmuch-config`.

        If *create* is `True`, the database will always be created in
        :attr:`MODE`.READ_WRITE mode. Default mode for opening is READ_ONLY.

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

    def _verify_initialized_db(self):
        """Raises a NotmuchError in case self._db is still None"""
        if self._db is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)            

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

        res = Database._create(path, Database.MODE.READ_WRITE)

        if res is None:
            raise NotmuchError(
                message="Could not create the specified database")
        self._db = res

    def open(self, path, mode= 0): 
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
        # Raise a NotmuchError if not initialized
        self._verify_initialized_db()

        return Database._get_path(self._db)

    def get_version(self):
        """Returns the database format version

        :returns: The database version as positive integer
        :exception: :exc:`NotmuchError` with STATUS.NOT_INITIALIZED if
                    the database was not intitialized.
        """
        # Raise a NotmuchError if not initialized
        self._verify_initialized_db()

        return Database._get_version (self._db)

    def needs_upgrade(self):
        """Does this database need to be upgraded before writing to it?

        If this function returns True then no functions that modify the
        database (:meth:`add_message`, :meth:`add_tag`,
        :meth:`Directory.set_mtime`, etc.) will work unless :meth:`upgrade` 
        is called successfully first.

        :returns: `True` or `False`
        :exception: :exc:`NotmuchError` with STATUS.NOT_INITIALIZED if
                    the database was not intitialized.
        """
        # Raise a NotmuchError if not initialized
        self._verify_initialized_db()

        return notmuch_database_needs_upgrade(self.db) 

    def add_message(self, filename):
        """Adds a new message to the database

        `filename` should be a path relative to the path of the open
        database (see :meth:`get_path`), or else should be an absolute
        filename with initial components that match the path of the
        database.

        The file should be a single mail message (not a multi-message mbox)
        that is expected to remain at its current location, since the
        notmuch database will reference the filename, and will not copy the
        entire contents of the file.

        :returns: On success, we return 

           1) a :class:`Message` object that can be used for things
              such as adding tags to the just-added message.
           2) one of the following STATUS values:

              STATUS.SUCCESS
                  Message successfully added to database.
              STATUS.DUPLICATE_MESSAGE_ID
                  Message has the same message ID as another message already
                  in the database. The new filename was successfully added
                  to the message in the database.

        :rtype:   2-tuple(:class:`Message`, STATUS)

        :exception: Raises a :exc:`NotmuchError` with the following meaning.
              If such an exception occurs, nothing was added to the database.

              STATUS.FILE_ERROR
                      An error occurred trying to open the file, (such as 
                      permission denied, or file not found, etc.).
              STATUS.FILE_NOT_EMAIL
                      The contents of filename don't look like an email message.
              STATUS.READ_ONLY_DATABASE
                      Database was opened in read-only mode so no message can
                      be added.
              STATUS.NOT_INITIALIZED
                      The database has not been initialized.
        """
        # Raise a NotmuchError if not initialized
        self._verify_initialized_db()

        msg_p = c_void_p()
        status = nmlib.notmuch_database_add_message(self._db,
                                                  filename,
                                                  byref(msg_p))
 
        if not status in [STATUS.SUCCESS,STATUS.DUPLICATE_MESSAGE_ID]:
            raise NotmuchError(status)

        #construct Message() and return
        msg = Message(msg_p, self)
        return (msg, status)

    def remove_message(self, filename):
        """Removes a message from the given notmuch database

        Note that only this particular filename association is removed from
        the database. If the same message (as determined by the message ID)
        is still available via other filenames, then the message will
        persist in the database for those filenames. When the last filename
        is removed for a particular message, the database content for that
        message will be entirely removed.

        :returns: A STATUS.* value with the following meaning:

             STATUS.SUCCESS
               The last filename was removed and the message was removed 
               from the database.
             STATUS.DUPLICATE_MESSAGE_ID
               This filename was removed but the message persists in the 
               database with at least one other filename.

        :exception: Raises a :exc:`NotmuchError` with the following meaning.
             If such an exception occurs, nothing was removed from the database.

             STATUS.READ_ONLY_DATABASE
               Database was opened in read-only mode so no message can be 
               removed.
             STATUS.NOT_INITIALIZED
               The database has not been initialized.
        """
        # Raise a NotmuchError if not initialized
        self._verify_initialized_db()

        status = nmlib.notmuch_database_remove_message(self._db,
                                                       filename)

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
        # Raise a NotmuchError if not initialized
        self._verify_initialized_db()

        msg_p = Database._find_message(self._db, msgid)
        if msg_p is None:
            return None
        return Message(msg_p, self)

    def get_all_tags(self):
        """Returns :class:`Tags` with a list of all tags found in the database

        :returns: :class:`Tags`
        :execption: :exc:`NotmuchError` with STATUS.NULL_POINTER on error
        """
        # Raise a NotmuchError if not initialized
        self._verify_initialized_db()

        tags_p = Database._get_all_tags (self._db)
        if tags_p == None:
            raise NotmuchError(STATUS.NULL_POINTER)
        return Tags(tags_p, self)

    def create_query(self, querystring):
        """Returns a :class:`Query` derived from this database

        This is a shorthand method for doing::

          # short version
          # Automatically frees the Database() when 'q' is deleted

          q  = Database(dbpath).create_query('from:"Biene Maja"')

          # long version, which is functionally equivalent but will keep the
          # Database in the 'db' variable around after we delete 'q':

          db = Database(dbpath)
          q  = Query(db,'from:"Biene Maja"')

        This function is a python extension and not in the underlying C API.
        """
        # Raise a NotmuchError if not initialized
        self._verify_initialized_db()

        return Query(self, querystring)

    def __repr__(self):
        return "'Notmuch DB " + self.get_path() + "'"

    def __del__(self):
        """Close and free the notmuch database if needed"""
        if self._db is not None:
            nmlib.notmuch_database_close(self._db)

    def _get_user_default_db(self):
        """ Reads a user's notmuch config and returns his db location

        Throws a NotmuchError if it cannot find it"""
        from ConfigParser import SafeConfigParser
        config = SafeConfigParser()
        conf_f = os.getenv('NOTMUCH_CONFIG',
                           os.path.expanduser('~/.notmuch-config'))
        config.read(conf_f)
        if not config.has_option('database','path'):
            raise NotmuchError(message=
                               "No DB path specified and no user default found")
        return config.get('database','path')

    @property
    def db_p(self):
        """Property returning a pointer to `notmuch_database_t` or `None`

        This should normally not be needed by a user (and is not yet
        guaranteed to remain stable in future versions).
        """
        return self._db

#------------------------------------------------------------------------------
class Query(object):
    """Represents a search query on an opened :class:`Database`.

    A query selects and filters a subset of messages from the notmuch
    database we derive from.

    Query() provides an instance attribute :attr:`sort`, which
    contains the sort order (if specified via :meth:`set_sort`) or
    `None`.

    Technically, it wraps the underlying *notmuch_query_t* struct.

    .. note:: Do remember that as soon as we tear down this object,
           all underlying derived objects such as threads,
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

    """notmuch_query_search_threads"""
    _search_threads = nmlib.notmuch_query_search_threads
    _search_threads.restype = c_void_p

    """notmuch_query_search_messages"""
    _search_messages = nmlib.notmuch_query_search_messages
    _search_messages.restype = c_void_p


    """notmuch_query_count_messages"""
    _count_messages = nmlib.notmuch_query_count_messages
    _count_messages.restype = c_uint

    def __init__(self, db, querystr):
        """
        :param db: An open database which we derive the Query from.
        :type db: :class:`Database`
        :param querystr: The query string for the message.
        :type querystr: str
        """
        self._db = None
        self._query = None
        self.sort = None
        self.create(db, querystr)

    def create(self, db, querystr):
        """Creates a new query derived from a Database

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

        self.sort = sort
        nmlib.notmuch_query_set_sort(self._query, sort)

    def search_threads(self):
        """Execute a query for threads

        Execute a query for threads, returning a :class:`Threads` iterator.
        The returned threads are owned by the query and as such, will only be 
        valid until the Query is deleted.

        Technically, it wraps the underlying
        *notmuch_query_search_threads* function.

        :returns: :class:`Threads`
        :exception: :exc:`NotmuchError`

                      * STATUS.NOT_INITIALIZED if query is not inited
                      * STATUS.NULL_POINTER if search_threads failed 
        """
        if self._query is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)            

        threads_p = Query._search_threads(self._query)

        if threads_p is None:
            NotmuchError(STATUS.NULL_POINTER)

        return Threads(threads_p,self)

    def search_messages(self):
        """Filter messages according to the query and return
        :class:`Messages` in the defined sort order

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

    def count_messages(self):
        """Estimate the number of messages matching the query

        This function performs a search and returns Xapian's best
        guess as to the number of matching messages. It is much faster
        than performing :meth:`search_messages` and counting the
        result with `len()` (although it always returned the same
        result in my tests). Technically, it wraps the underlying
        *notmuch_query_count_messages* function.

        :returns: :class:`Messages`
        :exception: :exc:`NotmuchError`

                      * STATUS.NOT_INITIALIZED if query is not inited
        """
        if self._query is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)            

        return Query._count_messages(self._query)

    def __del__(self):
        """Close and free the Query"""
        if self._query is not None:
            nmlib.notmuch_query_destroy (self._query)
