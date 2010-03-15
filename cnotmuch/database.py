import ctypes
from ctypes import c_int, c_char_p, c_void_p
from cnotmuch.globals import nmlib, STATUS, NotmuchError


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

    """notmuch_database_get_all_tags (notmuch_database_t *database)"""
    _get_all_tags = nmlib.notmuch_database_get_all_tags
    _get_all_tags.restype = c_void_p

    class notmuch_database_t(ctypes.Structure):
        """the opaque database that is returned by functions."""
        pass

    def __init__(self, path=None, create=False, status= MODE_READ_ONLY):
        """ Open or create a notmuch database"""
        self._db = None
        if create == False:
            self.open(path, status)
        else:
            #TODO: implement
            raise NotmuchError(message="Not implemented yet")

    #TODO: make a proper function
    create=nmlib.notmuch_database_create
    """ notmuch_database_create(const char *path):"""

    def open(self, path=None, status= MODE_READ_ONLY): 
        """calls notmuch_database_open

        If path is None, we will try to read a users notmuch configuration and
        use his default database.
        :returns: Raises :exc:`notmuch.NotmuchError` in case
                  of any failure (after printing an error message on stderr).
        """
        if path is None:
            if Database._std_db_path is None:
                from ConfigParser import SafeConfigParser
                import os.path
                config = SafeConfigParser()
                config.read(os.path.expanduser('~/.notmuch-config'))
                if not config.has_option('database','path'):
                    raise NotmuchError(message=
                              "No DB path specified and no user default found")
                Database._std_db_path=config.get('database','path')
            path = Database._std_db_path

        res = Database._open(path, status)

        if res is None:
            raise NotmuchError(
                message="Could not open the specified database")
        self._db = res

    def get_path(self):
        """notmuch_database_get_path (notmuch_database_t *database);  """
        return Database._get_path(self._db)

    #TODO:implement
    #If no message is found with the given message_id or if an
    #out-of-memory situation occurs, this function returns NULL.
    #notmuch_message_t *
    #notmuch_database_find_message (notmuch_database_t *database,
    #                               const char *message_id);

    def get_all_tags(self):
        """Return a Tags() object (list of all tags found in the database)

        :returns: Tags() object or raises :exc:`NotmuchError` with 
                  STATUS.NULL_POINTER on error
        """
        tags_p = Database._get_all_tags (self._db)
        if tags_p == None:
            raise NotmuchError(STATUS.NULL_POINTER)
        return Tags(tags_p, self)

    def __repr__(self):
        return "'Notmuch DB " + self.get_path() + "'"

    def __del__(self):
        """Close and free the notmuch database if needed"""
        if self._db is not None:
            print("Freeing the database now")
            nmlib.notmuch_database_close(self._db)

    @property
    def db_p(self):
        """Returns a pointer to the current notmuch_database_t or None"""
        return self._db


#------------------------------------------------------------------------------
class Tags(object):
    """Wrapper around notmuch_tags_t"""
    class notmuch_tags_t(ctypes.Structure):
        """the opaque tags struct that is returned by functions."""
        pass

    #notmuch_tags_get
    _get = nmlib.notmuch_tags_get
    _get.restype = c_char_p

    def __init__(self, tags_p, db=None):
        """ Is passed the db these tags are derived from, and saves a
        reference to it, so we can automatically delete the db object
        once all derived objects are dead."""
        self._tags = tags_p
        self._db = db
        print "inited tags with %d %s" %(tags_p, str(db))
    
    def __iter__(self):
        """ Make Tags an iterator """
        return self

    def next(self):
        if self._tags is None:
            raise StopIteration
        nmlib.notmuch_tags_move_to_next(self._tags)
        if not nmlib.notmuch_tags_valid(self._tags):
            raise StopIteration
        return Tags._get (self._tags)

    def __del__(self):
        """Close and free the notmuch tags"""
        if self._tags is not None:
            print("Freeing the Tags now")
            nmlib.notmuch_tags_destroy (self._tags)
