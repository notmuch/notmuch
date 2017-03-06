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

import os
import codecs
from ctypes import c_char_p, c_void_p, c_uint, byref, POINTER
from .compat import SafeConfigParser
from .globals import (
    nmlib,
    Enum,
    _str,
    NotmuchDatabaseP,
    NotmuchDirectoryP,
    NotmuchMessageP,
    NotmuchTagsP,
)
from .errors import (
    STATUS,
    FileError,
    NotmuchError,
    NullPointerError,
    NotInitializedError,
)
from .message import Message
from .tag import Tags
from .query import Query
from .directory import Directory

class Database(object):
    """The :class:`Database` is the highest-level object that notmuch
    provides. It references a notmuch database, and can be opened in
    read-only or read-write mode. A :class:`Query` can be derived from
    or be applied to a specific database to find messages. Also adding
    and removing messages to the database happens via this
    object. Modifications to the database are not atmic by default (see
    :meth:`begin_atomic`) and once a database has been modified, all
    other database objects pointing to the same data-base will throw an
    :exc:`XapianError` as the underlying database has been
    modified. Close and reopen the database to continue working with it.

    :class:`Database` objects implement the context manager protocol
    so you can use the :keyword:`with` statement to ensure that the
    database is properly closed. See :meth:`close` for more
    information.

    .. note::

        Any function in this class can and will throw an
        :exc:`NotInitializedError` if the database was not intitialized
        properly.
    """
    _std_db_path = None
    """Class attribute to cache user's default database"""

    MODE = Enum(['READ_ONLY', 'READ_WRITE'])
    """Constants: Mode in which to open the database"""

    """notmuch_database_get_directory"""
    _get_directory = nmlib.notmuch_database_get_directory
    _get_directory.argtypes = [NotmuchDatabaseP, c_char_p, POINTER(NotmuchDirectoryP)]
    _get_directory.restype = c_uint

    """notmuch_database_get_path"""
    _get_path = nmlib.notmuch_database_get_path
    _get_path.argtypes = [NotmuchDatabaseP]
    _get_path.restype = c_char_p

    """notmuch_database_get_version"""
    _get_version = nmlib.notmuch_database_get_version
    _get_version.argtypes = [NotmuchDatabaseP]
    _get_version.restype = c_uint

    """notmuch_database_get_revision"""
    _get_revision = nmlib.notmuch_database_get_revision
    _get_revision.argtypes = [NotmuchDatabaseP, POINTER(c_char_p)]
    _get_revision.restype = c_uint

    """notmuch_database_open"""
    _open = nmlib.notmuch_database_open
    _open.argtypes = [c_char_p, c_uint, POINTER(NotmuchDatabaseP)]
    _open.restype = c_uint

    """notmuch_database_upgrade"""
    _upgrade = nmlib.notmuch_database_upgrade
    _upgrade.argtypes = [NotmuchDatabaseP, c_void_p, c_void_p]
    _upgrade.restype = c_uint

    """ notmuch_database_find_message"""
    _find_message = nmlib.notmuch_database_find_message
    _find_message.argtypes = [NotmuchDatabaseP, c_char_p,
                              POINTER(NotmuchMessageP)]
    _find_message.restype = c_uint

    """notmuch_database_find_message_by_filename"""
    _find_message_by_filename = nmlib.notmuch_database_find_message_by_filename
    _find_message_by_filename.argtypes = [NotmuchDatabaseP, c_char_p,
                                          POINTER(NotmuchMessageP)]
    _find_message_by_filename.restype = c_uint

    """notmuch_database_get_all_tags"""
    _get_all_tags = nmlib.notmuch_database_get_all_tags
    _get_all_tags.argtypes = [NotmuchDatabaseP]
    _get_all_tags.restype = NotmuchTagsP

    """notmuch_database_create"""
    _create = nmlib.notmuch_database_create
    _create.argtypes = [c_char_p, POINTER(NotmuchDatabaseP)]
    _create.restype = c_uint

    def __init__(self, path = None, create = False,
                 mode = MODE.READ_ONLY):
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
        :raises: :exc:`NotmuchError` or derived exception in case of
            failure.
        """
        self._db = None
        self.mode = mode
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

    _destroy = nmlib.notmuch_database_destroy
    _destroy.argtypes = [NotmuchDatabaseP]
    _destroy.restype = c_uint

    def __del__(self):
        if self._db:
            status = self._destroy(self._db)
            if status != STATUS.SUCCESS:
                raise NotmuchError(status)

    def _assert_db_is_initialized(self):
        """Raises :exc:`NotInitializedError` if self._db is `None`"""
        if not self._db:
            raise NotInitializedError()

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
        :raises: :exc:`NotmuchError` in case of any failure
                    (possibly after printing an error message on stderr).
        """
        if self._db:
            raise NotmuchError(message="Cannot create db, this Database() "
                                       "already has an open one.")

        db = NotmuchDatabaseP()
        status = Database._create(_str(path), byref(db))

        if status != STATUS.SUCCESS:
            raise NotmuchError(status)
        self._db = db
        return status

    def open(self, path, mode=0):
        """Opens an existing database

        This function is used by __init__() and usually does not need
        to be called directly. It wraps the underlying
        *notmuch_database_open* function.

        :param status: Open the database in read-only or read-write mode
        :type status:  :attr:`MODE`
        :raises: Raises :exc:`NotmuchError` in case of any failure
                    (possibly after printing an error message on stderr).
        """
        db = NotmuchDatabaseP()
        status = Database._open(_str(path), mode, byref(db))

        if status != STATUS.SUCCESS:
            raise NotmuchError(status)
        self._db = db
        return status

    _close = nmlib.notmuch_database_close
    _close.argtypes = [NotmuchDatabaseP]
    _close.restype = c_uint

    def close(self):
        '''
        Closes the notmuch database.

        .. warning::

            This function closes the notmuch database. From that point
            on every method invoked on any object ever derived from
            the closed database may cease to function and raise a
            NotmuchError.
        '''
        if self._db:
            status = self._close(self._db)
            if status != STATUS.SUCCESS:
                raise NotmuchError(status)

    def __enter__(self):
        '''
        Implements the context manager protocol.
        '''
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        '''
        Implements the context manager protocol.
        '''
        self.close()

    def get_path(self):
        """Returns the file path of an open database"""
        self._assert_db_is_initialized()
        return Database._get_path(self._db).decode('utf-8')

    def get_version(self):
        """Returns the database format version

        :returns: The database version as positive integer
        """
        self._assert_db_is_initialized()
        return Database._get_version(self._db)

    def get_revision (self):
        """Returns the committed database revison and UUID

        :returns: (revison, uuid) The database revision as a positive integer
        and the UUID of the database.
        """
        self._assert_db_is_initialized()
        uuid = c_char_p ()
        revision = Database._get_revision(self._db, byref (uuid))
        return (revision, uuid.value.decode ('utf-8'))

    _needs_upgrade = nmlib.notmuch_database_needs_upgrade
    _needs_upgrade.argtypes = [NotmuchDatabaseP]
    _needs_upgrade.restype = bool

    def needs_upgrade(self):
        """Does this database need to be upgraded before writing to it?

        If this function returns `True` then no functions that modify the
        database (:meth:`add_message`,
        :meth:`Message.add_tag`, :meth:`Directory.set_mtime`,
        etc.) will work unless :meth:`upgrade` is called successfully first.

        :returns: `True` or `False`
        """
        self._assert_db_is_initialized()
        return self._needs_upgrade(self._db)

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

    _begin_atomic = nmlib.notmuch_database_begin_atomic
    _begin_atomic.argtypes = [NotmuchDatabaseP]
    _begin_atomic.restype = c_uint

    def begin_atomic(self):
        """Begin an atomic database operation

        Any modifications performed between a successful
        :meth:`begin_atomic` and a :meth:`end_atomic` will be applied to
        the database atomically.  Note that, unlike a typical database
        transaction, this only ensures atomicity, not durability;
        neither begin nor end necessarily flush modifications to disk.

        :returns: :attr:`STATUS`.SUCCESS or raises
        :raises: :exc:`NotmuchError`: :attr:`STATUS`.XAPIAN_EXCEPTION
                    Xapian exception occurred; atomic section not entered.

        *Added in notmuch 0.9*"""
        self._assert_db_is_initialized()
        status = self._begin_atomic(self._db)
        if status != STATUS.SUCCESS:
            raise NotmuchError(status)
        return status

    _end_atomic = nmlib.notmuch_database_end_atomic
    _end_atomic.argtypes = [NotmuchDatabaseP]
    _end_atomic.restype = c_uint

    def end_atomic(self):
        """Indicate the end of an atomic database operation

        See :meth:`begin_atomic` for details.

        :returns: :attr:`STATUS`.SUCCESS or raises

        :raises:
            :exc:`NotmuchError`:
                :attr:`STATUS`.XAPIAN_EXCEPTION
                    A Xapian exception occurred; atomic section not
                    ended.
                :attr:`STATUS`.UNBALANCED_ATOMIC:
                    end_atomic has been called more times than begin_atomic.

        *Added in notmuch 0.9*"""
        self._assert_db_is_initialized()
        status = self._end_atomic(self._db)
        if status != STATUS.SUCCESS:
            raise NotmuchError(status)
        return status

    def get_directory(self, path):
        """Returns a :class:`Directory` of path,

        :param path: An unicode string containing the path relative to the path
              of database (see :meth:`get_path`), or else should be an absolute
              path with initial components that match the path of 'database'.
        :returns: :class:`Directory` or raises an exception.
        :raises: :exc:`FileError` if path is not relative database or absolute
                 with initial components same as database.
        """
        self._assert_db_is_initialized()

        # sanity checking if path is valid, and make path absolute
        if path and path[0] == os.sep:
            # we got an absolute path
            if not path.startswith(self.get_path()):
                # but its initial components are not equal to the db path
                raise FileError('Database().get_directory() called '
                                'with a wrong absolute path')
            abs_dirpath = path
        else:
            #we got a relative path, make it absolute
            abs_dirpath = os.path.abspath(os.path.join(self.get_path(), path))

        dir_p = NotmuchDirectoryP()
        status = Database._get_directory(self._db, _str(path), byref(dir_p))

        if status != STATUS.SUCCESS:
            raise NotmuchError(status)
        if not dir_p:
            return None

        # return the Directory, init it with the absolute path
        return Directory(abs_dirpath, dir_p, self)

    _add_message = nmlib.notmuch_database_add_message
    _add_message.argtypes = [NotmuchDatabaseP, c_char_p,
                             POINTER(NotmuchMessageP)]
    _add_message.restype = c_uint

    def add_message(self, filename, sync_maildir_flags=False):
        """Adds a new message to the database

        :param filename: should be a path relative to the path of the
            open database (see :meth:`get_path`), or else should be an
            absolute filename with initial components that match the
            path of the database.

            The file should be a single mail message (not a
            multi-message mbox) that is expected to remain at its
            current location, since the notmuch database will reference
            the filename, and will not copy the entire contents of the
            file.

        :param sync_maildir_flags: If the message contains Maildir
            flags, we will -depending on the notmuch configuration- sync
            those tags to initial notmuch tags, if set to `True`. It is
            `False` by default to remain consistent with the libnotmuch
            API. You might want to look into the underlying method
            :meth:`Message.maildir_flags_to_tags`.

        :returns: On success, we return

           1) a :class:`Message` object that can be used for things
              such as adding tags to the just-added message.
           2) one of the following :attr:`STATUS` values:

              :attr:`STATUS`.SUCCESS
                  Message successfully added to database.
              :attr:`STATUS`.DUPLICATE_MESSAGE_ID
                  Message has the same message ID as another message already
                  in the database. The new filename was successfully added
                  to the list of the filenames for the existing message.

        :rtype:   2-tuple(:class:`Message`, :attr:`STATUS`)

        :raises: Raises a :exc:`NotmuchError` with the following meaning.
              If such an exception occurs, nothing was added to the database.

              :attr:`STATUS`.FILE_ERROR
                      An error occurred trying to open the file, (such as
                      permission denied, or file not found, etc.).
              :attr:`STATUS`.FILE_NOT_EMAIL
                      The contents of filename don't look like an email
                      message.
              :attr:`STATUS`.READ_ONLY_DATABASE
                      Database was opened in read-only mode so no message can
                      be added.
        """
        self._assert_db_is_initialized()
        msg_p = NotmuchMessageP()
        status = self._add_message(self._db, _str(filename), byref(msg_p))

        if not status in [STATUS.SUCCESS, STATUS.DUPLICATE_MESSAGE_ID]:
            raise NotmuchError(status)

        #construct Message() and return
        msg = Message(msg_p, self)
        #automatic sync initial tags from Maildir flags
        if sync_maildir_flags:
            msg.maildir_flags_to_tags()
        return (msg, status)

    _remove_message = nmlib.notmuch_database_remove_message
    _remove_message.argtypes = [NotmuchDatabaseP, c_char_p]
    _remove_message.restype = c_uint

    def remove_message(self, filename):
        """Removes a message (filename) from the given notmuch database

        Note that only this particular filename association is removed from
        the database. If the same message (as determined by the message ID)
        is still available via other filenames, then the message will
        persist in the database for those filenames. When the last filename
        is removed for a particular message, the database content for that
        message will be entirely removed.

        :returns: A :attr:`STATUS` value with the following meaning:

             :attr:`STATUS`.SUCCESS
               The last filename was removed and the message was removed
               from the database.
             :attr:`STATUS`.DUPLICATE_MESSAGE_ID
               This filename was removed but the message persists in the
               database with at least one other filename.

        :raises: Raises a :exc:`NotmuchError` with the following meaning.
             If such an exception occurs, nothing was removed from the
             database.

             :attr:`STATUS`.READ_ONLY_DATABASE
               Database was opened in read-only mode so no message can be
               removed.
        """
        self._assert_db_is_initialized()
        status = self._remove_message(self._db, _str(filename))
        if status not in [STATUS.SUCCESS, STATUS.DUPLICATE_MESSAGE_ID]:
            raise NotmuchError(status)
        return status

    def find_message(self, msgid):
        """Returns a :class:`Message` as identified by its message ID

        Wraps the underlying *notmuch_database_find_message* function.

        :param msgid: The message ID
        :type msgid: unicode or str
        :returns: :class:`Message` or `None` if no message is found.
        :raises:
            :exc:`OutOfMemoryError`
                  If an Out-of-memory occured while constructing the message.
            :exc:`XapianError`
                  In case of a Xapian Exception. These exceptions
                  include "Database modified" situations, e.g. when the
                  notmuch database has been modified by another program
                  in the meantime. In this case, you should close and
                  reopen the database and retry.
            :exc:`NotInitializedError` if
                    the database was not intitialized.
        """
        self._assert_db_is_initialized()
        msg_p = NotmuchMessageP()
        status = Database._find_message(self._db, _str(msgid), byref(msg_p))
        if status != STATUS.SUCCESS:
            raise NotmuchError(status)
        return msg_p and Message(msg_p, self) or None

    def find_message_by_filename(self, filename):
        """Find a message with the given filename

        :returns: If the database contains a message with the given
            filename, then a class:`Message:` is returned.  This
            function returns None if no message is found with the given
            filename.

        :raises: :exc:`OutOfMemoryError` if an Out-of-memory occured while
                 constructing the message.
        :raises: :exc:`XapianError` in case of a Xapian Exception.
                 These exceptions include "Database modified"
                 situations, e.g. when the notmuch database has been
                 modified by another program in the meantime. In this
                 case, you should close and reopen the database and
                 retry.
        :raises: :exc:`NotInitializedError` if the database was not
                 intitialized.

        *Added in notmuch 0.9*"""
        self._assert_db_is_initialized()

        msg_p = NotmuchMessageP()
        status = Database._find_message_by_filename(self._db, _str(filename),
                                                    byref(msg_p))
        if status != STATUS.SUCCESS:
            raise NotmuchError(status)
        return msg_p and Message(msg_p, self) or None

    def get_all_tags(self):
        """Returns :class:`Tags` with a list of all tags found in the database

        :returns: :class:`Tags`
        :execption: :exc:`NotmuchError` with :attr:`STATUS`.NULL_POINTER
                    on error
        """
        self._assert_db_is_initialized()
        tags_p = Database._get_all_tags(self._db)
        if not tags_p:
            raise NullPointerError()
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
        return Query(self, querystring)

    """notmuch_database_status_string"""
    _status_string = nmlib.notmuch_database_status_string
    _status_string.argtypes = [NotmuchDatabaseP]
    _status_string.restype = c_char_p

    def status_string(self):
        """Returns the status string of the database

        This is sometimes used for additional error reporting
        """
        self._assert_db_is_initialized()
        s = Database._status_string(self._db)
        if s:
            return s.decode('utf-8', 'ignore')
        return s

    def __repr__(self):
        return "'Notmuch DB " + self.get_path() + "'"

    def _get_user_default_db(self):
        """ Reads a user's notmuch config and returns his db location

        Throws a NotmuchError if it cannot find it"""
        config = SafeConfigParser()
        conf_f = os.getenv('NOTMUCH_CONFIG',
                           os.path.expanduser('~/.notmuch-config'))
        config.readfp(codecs.open(conf_f, 'r', 'utf-8'))
        if not config.has_option('database', 'path'):
            raise NotmuchError(message="No DB path specified"
                                       " and no user default found")
        return config.get('database', 'path')
