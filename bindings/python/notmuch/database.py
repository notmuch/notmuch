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

import os
from ctypes import c_int, c_char_p, c_void_p, c_uint, c_long, byref
from notmuch.globals import nmlib, STATUS, NotmuchError, Enum, _str
from notmuch.thread import Threads
from notmuch.message import Messages, Message
from notmuch.tag import Tags

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

    MODE = Enum(['READ_ONLY', 'READ_WRITE'])
    """Constants: Mode in which to open the database"""

    """notmuch_database_get_directory"""
    _get_directory = nmlib.notmuch_database_get_directory
    _get_directory.restype = c_void_p

    """notmuch_database_get_path"""
    _get_path = nmlib.notmuch_database_get_path
    _get_path.restype = c_char_p

    """notmuch_database_get_version"""
    _get_version = nmlib.notmuch_database_get_version
    _get_version.restype = c_uint

    """notmuch_database_open"""
    _open = nmlib.notmuch_database_open
    _open.restype = c_void_p

    """notmuch_database_upgrade"""
    _upgrade = nmlib.notmuch_database_upgrade
    _upgrade.argtypes = [c_void_p, c_void_p, c_void_p]

    """ notmuch_database_find_message"""
    _find_message = nmlib.notmuch_database_find_message
    _find_message.restype = c_void_p

    """notmuch_database_find_message_by_filename"""
    _find_message_by_filename = nmlib.notmuch_database_find_message_by_filename
    _find_message_by_filename.restype = c_void_p

    """notmuch_database_get_all_tags"""
    _get_all_tags = nmlib.notmuch_database_get_all_tags
    _get_all_tags.restype = c_void_p

    """notmuch_database_create"""
    _create = nmlib.notmuch_database_create
    _create.restype = c_void_p

    def __init__(self, path=None, create=False, mode=0):
        """If *path* is `None`, we will try to read a users notmuch
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

    def _assert_db_is_initialized(self):
        """Raises a NotmuchError in case self._db is still None"""
        if self._db is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

    def create(self, path):
        """Creates a new notmuch database

        This function is used by __init__() and usually does not need
        to be called directly. It wraps the underlying
        *notmuch_database_create* function and creates a new notmuch
        database at *path*. It will always return a database in :attr:`MODE`
        .READ_WRITE mode as creating an empty database for
        reading only does not make a great deal of sense.

        :param path: A directory in which we should create the database.
        :type path: str
        :returns: Nothing
        :exception: :exc:`NotmuchError` in case of any failure
                    (after printing an error message on stderr).
        """
        if self._db is not None:
            raise NotmuchError(message="Cannot create db, this Database() "
                                       "already has an open one.")

        res = Database._create(_str(path), Database.MODE.READ_WRITE)

        if res is None:
            raise NotmuchError(
                message="Could not create the specified database")
        self._db = res

    def open(self, path, mode=0):
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
        res = Database._open(_str(path), mode)

        if res is None:
            raise NotmuchError(
                message="Could not open the specified database")
        self._db = res

    def get_path(self):
        """Returns the file path of an open database

        Wraps *notmuch_database_get_path*."""
        self._assert_db_is_initialized()
        return Database._get_path(self._db).decode('utf-8')

    def get_version(self):
        """Returns the database format version

        :returns: The database version as positive integer
        :exception: :exc:`NotmuchError` with STATUS.NOT_INITIALIZED if
                    the database was not intitialized.
        """
        self._assert_db_is_initialized()
        return Database._get_version(self._db)

    def needs_upgrade(self):
        """Does this database need to be upgraded before writing to it?

        If this function returns `True` then no functions that modify the
        database (:meth:`add_message`,
        :meth:`Message.add_tag`, :meth:`Directory.set_mtime`,
        etc.) will work unless :meth:`upgrade` is called successfully first.

        :returns: `True` or `False`
        :exception: :exc:`NotmuchError` with STATUS.NOT_INITIALIZED if
                    the database was not intitialized.
        """
        self._assert_db_is_initialized()
        return nmlib.notmuch_database_needs_upgrade(self._db)

    def upgrade(self):
        """Upgrades the current database

        After opening a database in read-write mode, the client should
        check if an upgrade is needed (notmuch_database_needs_upgrade) and
        if so, upgrade with this function before making any modifications.

        NOT IMPLEMENTED: The optional progress_notify callback can be
        used by the caller to provide progress indication to the
        user. If non-NULL it will be called periodically with
        'progress' as a floating-point value in the range of [0.0..1.0]
        indicating the progress made so far in the upgrade process.

        :TODO: catch exceptions, document return values and etc...
        """
        self._assert_db_is_initialized()
        status = Database._upgrade(self._db, None, None)
        #TODO: catch exceptions, document return values and etc
        return status

    def begin_atomic(self):
        """Begin an atomic database operation

        Any modifications performed between a successful
        :meth:`begin_atomic` and a :meth:`end_atomic` will be applied to
        the database atomically.  Note that, unlike a typical database
        transaction, this only ensures atomicity, not durability;
        neither begin nor end necessarily flush modifications to disk.

        :returns: STATUS.SUCCESS or raises

        :exception: :exc:`NotmuchError` STATUS.XAPIAN_EXCEPTION::

                        A Xapian exception occurred; atomic section not
                        entered."""
        self._assert_db_is_initialized()
        status = nmlib.notmuch_database_begin_atomic(self._db)
        if status != STATUS.SUCCESS:
            raise NotmuchError(status)
        return status

    def end_atomic(self):
        """Indicate the end of an atomic database operation

        See :meth:`begin_atomic` for details.

        :returns: STATUS.SUCCESS or raises

        :exception:
            :exc:`NotmuchError`:
                STATUS.XAPIAN_EXCEPTION
                    A Xapian exception occurred; atomic section not
                    ended.
                STATUS.UNBALANCED_ATOMIC:
                    end_atomic has been called more times than begin_atomic."""
        self._assert_db_is_initialized()
        status = nmlib.notmuch_database_end_atomic(self._db)
        if status != STATUS.SUCCESS:
            raise NotmuchError(status)
        return status

    def get_directory(self, path):
        """Returns a :class:`Directory` of path,
        (creating it if it does not exist(?))

        .. warning:: This call needs a writeable database in
           Database.MODE.READ_WRITE mode. The underlying library will exit the
           program if this method is used on a read-only database!

        :param path: An unicode string containing the path relative to the path
              of database (see :meth:`get_path`), or else should be an absolute path
              with initial components that match the path of 'database'.
        :returns: :class:`Directory` or raises an exception.
        :exception: :exc:`NotmuchError`

                  STATUS.NOT_INITIALIZED
                    If the database was not intitialized.

                  STATUS.FILE_ERROR
                    If path is not relative database or absolute with initial
                    components same as database.

        """
        self._assert_db_is_initialized()
        # sanity checking if path is valid, and make path absolute
        if path[0] == os.sep:
            # we got an absolute path
            if not path.startswith(self.get_path()):
                # but its initial components are not equal to the db path
                raise NotmuchError(STATUS.FILE_ERROR,
                                   message="Database().get_directory() called "
                                           "with a wrong absolute path.")
            abs_dirpath = path
        else:
            #we got a relative path, make it absolute
            abs_dirpath = os.path.abspath(os.path.join(self.get_path(), path))

        dir_p = Database._get_directory(self._db, _str(path))

        # return the Directory, init it with the absolute path
        return Directory(_str(abs_dirpath), dir_p, self)

    def add_message(self, filename, sync_maildir_flags=False):
        """Adds a new message to the database

        :param filename: should be a path relative to the path of the open
        database (see :meth:`get_path`), or else should be an absolute
        filename with initial components that match the path of the
        database.

        The file should be a single mail message (not a multi-message mbox)
        that is expected to remain at its current location, since the
        notmuch database will reference the filename, and will not copy the
        entire contents of the file.

        :param sync_maildir_flags: If the message contains Maildir
            flags, we will -depending on the notmuch configuration- sync
            those tags to initial notmuch tags, if set to `True`. It is
            `False` by default to remain consistent with the libnotmuch
            API. You might want to look into the underlying method
            :meth:`Message.maildir_flags_to_tags`.

        :returns: On success, we return

           1) a :class:`Message` object that can be used for things
              such as adding tags to the just-added message.
           2) one of the following STATUS values:

              STATUS.SUCCESS
                  Message successfully added to database.
              STATUS.DUPLICATE_MESSAGE_ID
                  Message has the same message ID as another message already
                  in the database. The new filename was successfully added
                  to the list of the filenames for the existing message.

        :rtype:   2-tuple(:class:`Message`, STATUS)

        :exception: Raises a :exc:`NotmuchError` with the following meaning.
              If such an exception occurs, nothing was added to the database.

              STATUS.FILE_ERROR
                      An error occurred trying to open the file, (such as
                      permission denied, or file not found, etc.).
              STATUS.FILE_NOT_EMAIL
                      The contents of filename don't look like an email
                      message.
              STATUS.READ_ONLY_DATABASE
                      Database was opened in read-only mode so no message can
                      be added.
              STATUS.NOT_INITIALIZED
                      The database has not been initialized.
        """
        self._assert_db_is_initialized()
        msg_p = c_void_p()
        status = nmlib.notmuch_database_add_message(self._db,
                                                  _str(filename),
                                                  byref(msg_p))

        if not status in [STATUS.SUCCESS, STATUS.DUPLICATE_MESSAGE_ID]:
            raise NotmuchError(status)

        #construct Message() and return
        msg = Message(msg_p, self)
        #automatic sync initial tags from Maildir flags
        if sync_maildir_flags:
            msg.maildir_flags_to_tags()
        return (msg, status)

    def remove_message(self, filename):
        """Removes a message (filename) from the given notmuch database

        Note that only this particular filename association is removed from
        the database. If the same message (as determined by the message ID)
        is still available via other filenames, then the message will
        persist in the database for those filenames. When the last filename
        is removed for a particular message, the database content for that
        message will be entirely removed.

        :returns: A STATUS value with the following meaning:

             STATUS.SUCCESS
               The last filename was removed and the message was removed
               from the database.
             STATUS.DUPLICATE_MESSAGE_ID
               This filename was removed but the message persists in the
               database with at least one other filename.

        :exception: Raises a :exc:`NotmuchError` with the following meaning.
             If such an exception occurs, nothing was removed from the
             database.

             STATUS.READ_ONLY_DATABASE
               Database was opened in read-only mode so no message can be
               removed.
             STATUS.NOT_INITIALIZED
               The database has not been initialized.
        """
        self._assert_db_is_initialized()
        return nmlib.notmuch_database_remove_message(self._db,
                                                       filename)

    def find_message(self, msgid):
        """Returns a :class:`Message` as identified by its message ID

        Wraps the underlying *notmuch_database_find_message* function.

        :param msgid: The message ID
        :type msgid: string
        :returns: :class:`Message` or `None` if no message is found or
                  if any xapian exception or out-of-memory situation
                  occurs. Do note that Xapian Exceptions include
                  "Database modified" situations, e.g. when the
                  notmuch database has been modified by
                  another program in the meantime. A return value of
                  `None` is therefore no guarantee that the message
                  does not exist.
        :exception: :exc:`NotmuchError` with STATUS.NOT_INITIALIZED if
                  the database was not intitialized.
        """
        self._assert_db_is_initialized()
        msg_p = Database._find_message(self._db, _str(msgid))
        return msg_p and Message(msg_p, self) or None

    def find_message_by_filename(self, filename):
        """Find a message with the given filename

        :returns: If the database contains a message with the given
            filename, then a class:`Message:` is returned.  This
            function returns None in the following situations:

                * No message is found with the given filename
                * An out-of-memory situation occurs
                * A Xapian exception occurs"""
        self._assert_db_is_initialized()
        msg_p = Database._find_message_by_filename(self._db, _str(filename))
        return msg_p and Message(msg_p, self) or None

    def get_all_tags(self):
        """Returns :class:`Tags` with a list of all tags found in the database

        :returns: :class:`Tags`
        :execption: :exc:`NotmuchError` with STATUS.NULL_POINTER on error
        """
        self._assert_db_is_initialized()
        tags_p = Database._get_all_tags(self._db)
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
        self._assert_db_is_initialized()
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
        if not config.has_option('database', 'path'):
            raise NotmuchError(message="No DB path specified"
                                       " and no user default found")
        return config.get('database', 'path').decode('utf-8')

    @property
    def db_p(self):
        """Property returning a pointer to `notmuch_database_t` or `None`

        This should normally not be needed by a user (and is not yet
        guaranteed to remain stable in future versions).
        """
        return self._db


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
    SORT = Enum(['OLDEST_FIRST', 'NEWEST_FIRST', 'MESSAGE_ID', 'UNSORTED'])
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
        :type querystr: utf-8 encoded str or unicode
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
        :type querystr: utf-8 encoded str or unicode
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
        query_p = Query._create(db.db_p, _str(querystr))
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

        The method sets :attr:`Message.FLAG`\.MATCH for those messages that
        match the query. The method :meth:`Message.get_flag` allows us
        to get the value of this flag.

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
            raise NotmuchError(STATUS.NULL_POINTER)

        return Threads(threads_p, self)

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

        return Messages(msgs_p, self)

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
            nmlib.notmuch_query_destroy(self._query)


class Directory(object):
    """Represents a directory entry in the notmuch directory

    Modifying attributes of this object will modify the
    database, not the real directory attributes.

    The Directory object is usually derived from another object
    e.g. via :meth:`Database.get_directory`, and will automatically be
    become invalid whenever that parent is deleted. You should
    therefore initialized this object handing it a reference to the
    parent, preventing the parent from automatically being garbage
    collected.
    """

    """notmuch_directory_get_mtime"""
    _get_mtime = nmlib.notmuch_directory_get_mtime
    _get_mtime.restype = c_long

    """notmuch_directory_set_mtime"""
    _set_mtime = nmlib.notmuch_directory_set_mtime
    _set_mtime.argtypes = [c_char_p, c_long]

    """notmuch_directory_get_child_files"""
    _get_child_files = nmlib.notmuch_directory_get_child_files
    _get_child_files.restype = c_void_p

    """notmuch_directory_get_child_directories"""
    _get_child_directories = nmlib.notmuch_directory_get_child_directories
    _get_child_directories.restype = c_void_p

    def _verify_dir_initialized(self):
        """Raises a NotmuchError(STATUS.NOT_INITIALIZED) if dir_p is None"""
        if self._dir_p is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

    def __init__(self, path, dir_p, parent):
        """
        :param path:   The absolute path of the directory object as unicode.
        :param dir_p:  The pointer to an internal notmuch_directory_t object.
        :param parent: The object this Directory is derived from
                       (usually a :class:`Database`). We do not directly use
                       this, but store a reference to it as long as
                       this Directory object lives. This keeps the
                       parent object alive.
        """
        assert isinstance(path, unicode), "Path needs to be an UNICODE object"
        self._path = path
        self._dir_p = dir_p
        self._parent = parent

    def set_mtime(self, mtime):
        """Sets the mtime value of this directory in the database

        The intention is for the caller to use the mtime to allow efficient
        identification of new messages to be added to the database. The
        recommended usage is as follows:

        * Read the mtime of a directory from the filesystem

        * Call :meth:`Database.add_message` for all mail files in
          the directory

        * Call notmuch_directory_set_mtime with the mtime read from the
          filesystem.  Then, when wanting to check for updates to the
          directory in the future, the client can call :meth:`get_mtime`
          and know that it only needs to add files if the mtime of the
          directory and files are newer than the stored timestamp.

          .. note:: :meth:`get_mtime` function does not allow the caller
                 to distinguish a timestamp of 0 from a non-existent
                 timestamp. So don't store a timestamp of 0 unless you are
                 comfortable with that.

          :param mtime: A (time_t) timestamp
          :returns: Nothing on success, raising an exception on failure.
          :exception: :exc:`NotmuchError`:

                        STATUS.XAPIAN_EXCEPTION
                          A Xapian exception occurred, mtime not stored.
                        STATUS.READ_ONLY_DATABASE
                          Database was opened in read-only mode so directory
                          mtime cannot be modified.
                        STATUS.NOT_INITIALIZED
                          The directory has not been initialized
        """
        #Raise a NotmuchError(STATUS.NOT_INITIALIZED) if the dir_p is None
        self._verify_dir_initialized()

        #TODO: make sure, we convert the mtime parameter to a 'c_long'
        status = Directory._set_mtime(self._dir_p, mtime)

        #return on success
        if status == STATUS.SUCCESS:
            return
        #fail with Exception otherwise
        raise NotmuchError(status)

    def get_mtime(self):
        """Gets the mtime value of this directory in the database

        Retrieves a previously stored mtime for this directory.

        :param mtime: A (time_t) timestamp
        :returns: Nothing on success, raising an exception on failure.
        :exception: :exc:`NotmuchError`:

                        STATUS.NOT_INITIALIZED
                          The directory has not been initialized
        """
        #Raise a NotmuchError(STATUS.NOT_INITIALIZED) if self.dir_p is None
        self._verify_dir_initialized()

        return Directory._get_mtime(self._dir_p)

    # Make mtime attribute a property of Directory()
    mtime = property(get_mtime, set_mtime, doc="""Property that allows getting
                     and setting of the Directory *mtime* (read-write)

                     See :meth:`get_mtime` and :meth:`set_mtime` for usage and
                     possible exceptions.""")

    def get_child_files(self):
        """Gets a Filenames iterator listing all the filenames of
        messages in the database within the given directory.

        The returned filenames will be the basename-entries only (not
        complete paths.
        """
        #Raise a NotmuchError(STATUS.NOT_INITIALIZED) if self._dir_p is None
        self._verify_dir_initialized()

        files_p = Directory._get_child_files(self._dir_p)
        return Filenames(files_p, self)

    def get_child_directories(self):
        """Gets a :class:`Filenames` iterator listing all the filenames of
        sub-directories in the database within the given directory

        The returned filenames will be the basename-entries only (not
        complete paths.
        """
        #Raise a NotmuchError(STATUS.NOT_INITIALIZED) if self._dir_p is None
        self._verify_dir_initialized()

        files_p = Directory._get_child_directories(self._dir_p)
        return Filenames(files_p, self)

    @property
    def path(self):
        """Returns the absolute path of this Directory (read-only)"""
        return self._path

    def __repr__(self):
        """Object representation"""
        return "<notmuch Directory object '%s'>" % self._path

    def __del__(self):
        """Close and free the Directory"""
        if self._dir_p is not None:
            nmlib.notmuch_directory_destroy(self._dir_p)


class Filenames(object):
    """An iterator over File- or Directory names stored in the database"""

    #notmuch_filenames_get
    _get = nmlib.notmuch_filenames_get
    _get.restype = c_char_p

    def __init__(self, files_p, parent):
        """
        :param files_p: The pointer to an internal notmuch_filenames_t object.
        :param parent: The object this Directory is derived from
                       (usually a Directory()). We do not directly use
                       this, but store a reference to it as long as
                       this Directory object lives. This keeps the
                       parent object alive.
        """
        self._files_p = files_p
        self._parent = parent

    def __iter__(self):
        """ Make Filenames an iterator """
        return self

    def next(self):
        if self._files_p is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        if not nmlib.notmuch_filenames_valid(self._files_p):
            self._files_p = None
            raise StopIteration

        file = Filenames._get(self._files_p)
        nmlib.notmuch_filenames_move_to_next(self._files_p)
        return file

    def __len__(self):
        """len(:class:`Filenames`) returns the number of contained files

        .. note:: As this iterates over the files, we will not be able to
               iterate over them again! So this will fail::

                 #THIS FAILS
                 files = Database().get_directory('').get_child_files()
                 if len(files) > 0:              #this 'exhausts' msgs
                     # next line raises NotmuchError(STATUS.NOT_INITIALIZED)!!!
                     for file in files: print file
        """
        if self._files_p is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        i = 0
        while nmlib.notmuch_filenames_valid(self._files_p):
            nmlib.notmuch_filenames_move_to_next(self._files_p)
            i += 1
        self._files_p = None
        return i

    def __del__(self):
        """Close and free Filenames"""
        if self._files_p is not None:
            nmlib.notmuch_filenames_destroy(self._files_p)
