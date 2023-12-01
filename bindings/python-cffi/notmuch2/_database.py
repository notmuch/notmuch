import collections
import configparser
import enum
import functools
import os
import pathlib
import weakref

import notmuch2._base as base
import notmuch2._config as config
import notmuch2._capi as capi
import notmuch2._errors as errors
import notmuch2._message as message
import notmuch2._query as querymod
import notmuch2._tags as tags


__all__ = ['Database', 'AtomicContext', 'DbRevision']


def _config_pathname():
    """Return the path of the configuration file.

    :rtype: pathlib.Path
    """
    cfgfname = os.getenv('NOTMUCH_CONFIG', '~/.notmuch-config')
    return pathlib.Path(os.path.expanduser(cfgfname))


class Mode(enum.Enum):
    READ_ONLY = capi.lib.NOTMUCH_DATABASE_MODE_READ_ONLY
    READ_WRITE = capi.lib.NOTMUCH_DATABASE_MODE_READ_WRITE

class ConfigFile(enum.Enum):
    EMPTY = b''
    SEARCH = capi.ffi.NULL

class QuerySortOrder(enum.Enum):
    OLDEST_FIRST = capi.lib.NOTMUCH_SORT_OLDEST_FIRST
    NEWEST_FIRST = capi.lib.NOTMUCH_SORT_NEWEST_FIRST
    MESSAGE_ID = capi.lib.NOTMUCH_SORT_MESSAGE_ID
    UNSORTED = capi.lib.NOTMUCH_SORT_UNSORTED


class QueryExclude(enum.Enum):
    TRUE = capi.lib.NOTMUCH_EXCLUDE_TRUE
    FLAG = capi.lib.NOTMUCH_EXCLUDE_FLAG
    FALSE = capi.lib.NOTMUCH_EXCLUDE_FALSE
    ALL = capi.lib.NOTMUCH_EXCLUDE_ALL


class DecryptionPolicy(enum.Enum):
    FALSE = capi.lib.NOTMUCH_DECRYPT_FALSE
    TRUE = capi.lib.NOTMUCH_DECRYPT_TRUE
    AUTO = capi.lib.NOTMUCH_DECRYPT_AUTO
    NOSTASH = capi.lib.NOTMUCH_DECRYPT_NOSTASH


class Database(base.NotmuchObject):
    """Toplevel access to notmuch.

    A :class:`Database` can be opened read-only or read-write.
    Modifications are not atomic by default, use :meth:`begin_atomic`
    for atomic updates.  If the underlying database has been modified
    outside of this class a :exc:`XapianError` will be raised and the
    instance must be closed and a new one created.

    You can use an instance of this class as a context-manager.

    :cvar MODE: The mode a database can be opened with, an enumeration
       of ``READ_ONLY`` and ``READ_WRITE``
    :cvar SORT: The sort order for search results, ``OLDEST_FIRST``,
       ``NEWEST_FIRST``, ``MESSAGE_ID`` or ``UNSORTED``.
    :cvar EXCLUDE: Which messages to exclude from queries, ``TRUE``,
       ``FLAG``, ``FALSE`` or ``ALL``.  See the query documentation
       for details.
    :cvar CONFIG: Control loading of config file. Enumeration of
       ``EMPTY`` (don't load a config file), and ``SEARCH`` (search as
       in :ref:`config_search`)
    :cvar AddedMessage: A namedtuple ``(msg, dup)`` used by
       :meth:`add` as return value.
    :cvar STR_MODE_MAP: A map mapping strings to :attr:`MODE` items.
       This is used to implement the ``ro`` and ``rw`` string
       variants.

    :ivar closed: Boolean indicating if the database is closed or
       still open.

    :param path: The directory of where the database is stored.  If
       ``None`` the location will be searched according to
       :ref:`database`
    :type path: str, bytes, os.PathLike or pathlib.Path
    :param mode: The mode to open the database in.  One of
       :attr:`MODE.READ_ONLY` OR :attr:`MODE.READ_WRITE`.  For
       convenience you can also use the strings ``ro`` for
       :attr:`MODE.READ_ONLY` and ``rw`` for :attr:`MODE.READ_WRITE`.
    :type mode: :attr:`MODE` or str.

    :param config: Where to load the configuration from, if any.
    :type config: :attr:`CONFIG.EMPTY`, :attr:`CONFIG.SEARCH`, str, bytes, os.PathLike, pathlib.Path
    :raises KeyError: if an unknown mode string is used.
    :raises OSError: or subclasses if the configuration file can not
       be opened.
    :raises configparser.Error: or subclasses if the configuration
       file can not be parsed.
    :raises NotmuchError: or subclasses for other failures.
    """

    MODE = Mode
    SORT = QuerySortOrder
    EXCLUDE = QueryExclude
    CONFIG = ConfigFile
    AddedMessage = collections.namedtuple('AddedMessage', ['msg', 'dup'])
    _db_p = base.MemoryPointer()
    STR_MODE_MAP = {
        'ro': MODE.READ_ONLY,
        'rw': MODE.READ_WRITE,
    }

    @staticmethod
    def _cfg_path_encode(path):
        if isinstance(path,ConfigFile):
            path = path.value
        elif path is None:
            path = capi.ffi.NULL
        elif not hasattr(os, 'PathLike') and isinstance(path, pathlib.Path):
            path = bytes(path)
        else:
            path = os.fsencode(path)
        return path

    @staticmethod
    def _db_path_encode(path):
        if path is None:
            path = capi.ffi.NULL
        elif not hasattr(os, 'PathLike') and isinstance(path, pathlib.Path):
            path = bytes(path)
        else:
            path = os.fsencode(path)
        return path

    def __init__(self, path=None, mode=MODE.READ_ONLY, config=CONFIG.SEARCH):
        if isinstance(mode, str):
            mode = self.STR_MODE_MAP[mode]
        self.mode = mode

        db_pp = capi.ffi.new('notmuch_database_t **')
        cmsg = capi.ffi.new('char**')
        ret = capi.lib.notmuch_database_open_with_config(self._db_path_encode(path),
                                                         mode.value,
                                                         self._cfg_path_encode(config),
                                                         capi.ffi.NULL,
                                                         db_pp, cmsg)
        if cmsg[0]:
            msg = capi.ffi.string(cmsg[0]).decode(errors='replace')
            capi.lib.free(cmsg[0])
        else:
            msg = None
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret, msg)
        self._db_p = db_pp[0]
        self.closed = False

    @classmethod
    def create(cls, path=None, config=ConfigFile.EMPTY):
        """Create and open database in READ_WRITE mode.

        This is creates a new notmuch database and returns an opened
        instance in :attr:`MODE.READ_WRITE` mode.

        :param path: The directory of where the database is stored.
           If ``None`` the location will be read searched by the
           notmuch library (see notmuch(3)::notmuch_open_with_config).
        :type path: str, bytes or os.PathLike

        :param config: The pathname of the notmuch configuration file.
        :type config: :attr:`CONFIG.EMPTY`, :attr:`CONFIG.SEARCH`, str, bytes, os.PathLike, pathlib.Path

        :raises OSError: or subclasses if the configuration file can not
           be opened.
        :raises configparser.Error: or subclasses if the configuration
           file can not be parsed.
        :raises NotmuchError: if the config file does not have the
           database.path setting.
        :raises FileError: if the database already exists.

        :returns: The newly created instance.
        """

        db_pp = capi.ffi.new('notmuch_database_t **')
        cmsg = capi.ffi.new('char**')
        ret = capi.lib.notmuch_database_create_with_config(cls._db_path_encode(path),
                                                           cls._cfg_path_encode(config),
                                                           capi.ffi.NULL,
                                                           db_pp, cmsg)
        if cmsg[0]:
            msg = capi.ffi.string(cmsg[0]).decode(errors='replace')
            capi.lib.free(cmsg[0])
        else:
            msg = None
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret, msg)

        # Now close the db and let __init__ open it.  Inefficient but
        # creating is not a hot loop while this allows us to have a
        # clean API.
        ret = capi.lib.notmuch_database_destroy(db_pp[0])
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)
        return cls(path, cls.MODE.READ_WRITE, config=config)

    @staticmethod
    def default_path(cfg_path=None):
        """Return the path of the user's default database.

        This reads the user's configuration file and returns the
        default path of the database.

        :param cfg_path: The pathname of the notmuch configuration file.
           If not specified tries to use the pathname provided in the
           :envvar:`NOTMUCH_CONFIG` environment variable and falls back
           to :file:`~/.notmuch-config`.
        :type cfg_path: str, bytes, os.PathLike or pathlib.Path.

        :returns: The path of the database, which does not necessarily
           exists.
        :rtype: pathlib.Path
        :raises OSError: or subclasses if the configuration file can not
           be opened.
        :raises configparser.Error: or subclasses if the configuration
           file can not be parsed.
        :raises NotmuchError: if the config file does not have the
           database.path setting.

        .. deprecated:: 0.35
           Use the ``config`` parameter to :meth:`__init__` or :meth:`__create__` instead.
        """
        if not cfg_path:
            cfg_path = _config_pathname()
        if not hasattr(os, 'PathLike') and isinstance(cfg_path, pathlib.Path):
            cfg_path = bytes(cfg_path)
        parser = configparser.ConfigParser()
        with open(cfg_path) as fp:
            parser.read_file(fp)
        try:
            return pathlib.Path(parser.get('database', 'path'))
        except configparser.Error:
            raise errors.NotmuchError(
                'No database.path setting in {}'.format(cfg_path))

    def __del__(self):
        self._destroy()

    @property
    def alive(self):
        try:
            self._db_p
        except errors.ObjectDestroyedError:
            return False
        else:
            return True

    def _destroy(self):
        try:
            ret = capi.lib.notmuch_database_destroy(self._db_p)
        except errors.ObjectDestroyedError:
            ret = capi.lib.NOTMUCH_STATUS_SUCCESS
        else:
            self._db_p = None
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)

    def close(self):
        """Close the notmuch database.

        Once closed most operations will fail.  This can still be
        useful however to explicitly close a database which is opened
        read-write as this would otherwise stop other processes from
        reading the database while it is open.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        ret = capi.lib.notmuch_database_close(self._db_p)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)
        self.closed = True

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self.close()

    @property
    def path(self):
        """The pathname of the notmuch database.

        This is returned as a :class:`pathlib.Path` instance.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        try:
            return self._cache_path
        except AttributeError:
            ret = capi.lib.notmuch_database_get_path(self._db_p)
            self._cache_path = pathlib.Path(os.fsdecode(capi.ffi.string(ret)))
            return self._cache_path

    @property
    def version(self):
        """The database format version.

        This is a positive integer.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        try:
            return self._cache_version
        except AttributeError:
            ret = capi.lib.notmuch_database_get_version(self._db_p)
            self._cache_version = ret
            return ret

    @property
    def needs_upgrade(self):
        """Whether the database should be upgraded.

        If *True* the database can be upgraded using :meth:`upgrade`.
        Not doing so may result in some operations raising
        :exc:`UpgradeRequiredError`.

        A read-only database will never be upgradable.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        ret = capi.lib.notmuch_database_needs_upgrade(self._db_p)
        return bool(ret)

    def upgrade(self, progress_cb=None):
        """Upgrade the database to the latest version.

        Upgrade the database, optionally with a progress callback
        which should be a callable which will be called with a
        floating point number in the range of [0.0 .. 1.0].
        """
        raise NotImplementedError

    def atomic(self):
        """Return a context manager to perform atomic operations.

        The returned context manager can be used to perform atomic
        operations on the database.

        .. note:: Unlinke a traditional RDBMS transaction this does
           not imply durability, it only ensures the changes are
           performed atomically.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        ctx = AtomicContext(self, '_db_p')
        return ctx

    def revision(self):
        """The currently committed revision in the database.

        Returned as a ``(revision, uuid)`` namedtuple.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        raw_uuid = capi.ffi.new('char**')
        rev = capi.lib.notmuch_database_get_revision(self._db_p, raw_uuid)
        return DbRevision(rev, capi.ffi.string(raw_uuid[0]))

    def get_directory(self, path):
        raise NotImplementedError

    def default_indexopts(self):
        """Returns default index options for the database.

        :raises ObjectDestroyedError: if used after destroyed.

        :returns: :class:`IndexOptions`.
        """
        opts = capi.lib.notmuch_database_get_default_indexopts(self._db_p)
        return IndexOptions(self, opts)

    def add(self, filename, *, sync_flags=False, indexopts=None):
        """Add a message to the database.

        Add a new message to the notmuch database.  The message is
        referred to by the pathname of the maildir file.  If the
        message ID of the new message already exists in the database,
        this adds ``pathname`` to the list of list of files for the
        existing message.

        :param filename: The path of the file containing the message.
        :type filename: str, bytes, os.PathLike or pathlib.Path.
        :param sync_flags: Whether to sync the known maildir flags to
           notmuch tags.  See :meth:`Message.flags_to_tags` for
           details.
        :type sync_flags: bool
        :param indexopts: The indexing options, see
           :meth:`default_indexopts`.  Leave as `None` to use the
           default options configured in the database.
        :type indexopts: :class:`IndexOptions` or `None`

        :returns: A tuple where the first item is the newly inserted
           messages as a :class:`Message` instance, and the second
           item is a boolean indicating if the message inserted was a
           duplicate.  This is the namedtuple ``AddedMessage(msg,
           dup)``.
        :rtype: Database.AddedMessage

        If an exception is raised, no message was added.

        :raises XapianError: A Xapian exception occurred.
        :raises FileError: The file referred to by ``pathname`` could
           not be opened.
        :raises FileNotEmailError: The file referreed to by
           ``pathname`` is not recognised as an email message.
        :raises ReadOnlyDatabaseError: The database is opened in
           READ_ONLY mode.
        :raises UpgradeRequiredError: The database must be upgraded
           first.
        :raises ObjectDestroyedError: if used after destroyed.
        """
        if not hasattr(os, 'PathLike') and isinstance(filename, pathlib.Path):
            filename = bytes(filename)
        msg_pp = capi.ffi.new('notmuch_message_t **')
        opts_p = indexopts._opts_p if indexopts else capi.ffi.NULL
        ret = capi.lib.notmuch_database_index_file(
            self._db_p, os.fsencode(filename), opts_p, msg_pp)
        ok = [capi.lib.NOTMUCH_STATUS_SUCCESS,
              capi.lib.NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID]
        if ret not in ok:
            raise errors.NotmuchError(ret)
        msg = message.Message(self, msg_pp[0], db=self)
        if sync_flags:
            msg.tags.from_maildir_flags()
        return self.AddedMessage(
            msg, ret == capi.lib.NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID)

    def remove(self, filename):
        """Remove a message from the notmuch database.

        Removing a message which is not in the database is just a
        silent nop-operation.

        :param filename: The pathname of the file containing the
           message to be removed.
        :type filename: str, bytes, os.PathLike or pathlib.Path.

        :returns: True if the message is still in the database.  This
           can happen when multiple files contain the same message ID.
           The true/false distinction is fairly arbitrary, but think
           of it as ``dup = db.remove_message(name); if dup: ...``.
        :rtype: bool

        :raises XapianError: A Xapian exception occurred.
        :raises ReadOnlyDatabaseError: The database is opened in
           READ_ONLY mode.
        :raises UpgradeRequiredError: The database must be upgraded
           first.
        :raises ObjectDestroyedError: if used after destroyed.
        """
        if not hasattr(os, 'PathLike') and isinstance(filename, pathlib.Path):
            filename = bytes(filename)
        ret = capi.lib.notmuch_database_remove_message(self._db_p,
                                                       os.fsencode(filename))
        ok = [capi.lib.NOTMUCH_STATUS_SUCCESS,
              capi.lib.NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID]
        if ret not in ok:
            raise errors.NotmuchError(ret)
        if ret == capi.lib.NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID:
            return True
        else:
            return False

    def find(self, msgid):
        """Return the message matching the given message ID.

        If a message with the given message ID is found a
        :class:`Message` instance is returned.  Otherwise a
        :exc:`LookupError` is raised.

        :param msgid: The message ID to look for.
        :type msgid: str

        :returns: The message instance.
        :rtype: Message

        :raises LookupError: If no message was found.
        :raises OutOfMemoryError: When there is no memory to allocate
           the message instance.
        :raises XapianError: A Xapian exception occurred.
        :raises ObjectDestroyedError: if used after destroyed.
        """
        msg_pp = capi.ffi.new('notmuch_message_t **')
        ret = capi.lib.notmuch_database_find_message(self._db_p,
                                                     msgid.encode(), msg_pp)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)
        msg_p = msg_pp[0]
        if msg_p == capi.ffi.NULL:
            raise LookupError
        msg = message.Message(self, msg_p, db=self)
        return msg

    def get(self, filename):
        """Return the :class:`Message` given a pathname.

        If a message with the given pathname exists in the database
        return the :class:`Message` instance for the message.
        Otherwise raise a :exc:`LookupError` exception.

        :param filename: The pathname of the message.
        :type filename: str, bytes, os.PathLike or pathlib.Path

        :returns: The message instance.
        :rtype: Message

        :raises LookupError: If no message was found.  This is also
           a subclass of :exc:`KeyError`.
        :raises OutOfMemoryError: When there is no memory to allocate
           the message instance.
        :raises XapianError: A Xapian exception occurred.
        :raises ObjectDestroyedError: if used after destroyed.
        """
        if not hasattr(os, 'PathLike') and isinstance(filename, pathlib.Path):
            filename = bytes(filename)
        msg_pp = capi.ffi.new('notmuch_message_t **')
        ret = capi.lib.notmuch_database_find_message_by_filename(
            self._db_p, os.fsencode(filename), msg_pp)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)
        msg_p = msg_pp[0]
        if msg_p == capi.ffi.NULL:
            raise LookupError
        msg = message.Message(self, msg_p, db=self)
        return msg

    @property
    def tags(self):
        """Return an immutable set with all tags used in this database.

        This returns an immutable set-like object implementing the
        collections.abc.Set Abstract Base Class.  Due to the
        underlying libnotmuch implementation some operations have
        different performance characteristics then plain set objects.
        Mainly any lookup operation is O(n) rather then O(1).

        Normal usage treats tags as UTF-8 encoded unicode strings so
        they are exposed to Python as normal unicode string objects.
        If you need to handle tags stored in libnotmuch which are not
        valid unicode do check the :class:`ImmutableTagSet` docs for
        how to handle this.

        :rtype: ImmutableTagSet

        :raises ObjectDestroyedError: if used after destroyed.
        """
        try:
            ref = self._cached_tagset
        except AttributeError:
            tagset = None
        else:
            tagset = ref()
        if tagset is None:
            tagset = tags.ImmutableTagSet(
                self, '_db_p', capi.lib.notmuch_database_get_all_tags)
            self._cached_tagset = weakref.ref(tagset)
        return tagset

    @property
    def config(self):
        """Return a mutable mapping with the settings stored in this database.

        This returns an mutable dict-like object implementing the
        collections.abc.MutableMapping Abstract Base Class.

        :rtype: Config

        :raises ObjectDestroyedError: if used after destroyed.
        """
        try:
            ref = self._cached_config
        except AttributeError:
            config_mapping = None
        else:
            config_mapping = ref()
        if config_mapping is None:
            config_mapping = config.ConfigMapping(self, '_db_p')
            self._cached_config = weakref.ref(config_mapping)
        return config_mapping

    def _create_query(self, query, *,
                      omit_excluded=EXCLUDE.TRUE,
                      sort=SORT.UNSORTED,  # Check this default
                      exclude_tags=None):
        """Create an internal query object.

        :raises OutOfMemoryError: if no memory is available to
           allocate the query.
        """
        if isinstance(query, str):
            query = query.encode('utf-8')
        query_p = capi.lib.notmuch_query_create(self._db_p, query)
        if query_p == capi.ffi.NULL:
            raise errors.OutOfMemoryError()
        capi.lib.notmuch_query_set_omit_excluded(query_p, omit_excluded.value)
        capi.lib.notmuch_query_set_sort(query_p, sort.value)
        if exclude_tags is not None:
            for tag in exclude_tags:
                if isinstance(tag, str):
                    tag = tag.encode('utf-8')
                capi.lib.notmuch_query_add_tag_exclude(query_p, tag)
        return querymod.Query(self, query_p)

    def messages(self, query, *,
                 omit_excluded=EXCLUDE.TRUE,
                 sort=SORT.UNSORTED,  # Check this default
                 exclude_tags=None):
        """Search the database for messages.

        :returns: An iterator over the messages found.
        :rtype: MessageIter

        :raises OutOfMemoryError: if no memory is available to
           allocate the query.
        :raises ObjectDestroyedError: if used after destroyed.
        """
        query = self._create_query(query,
                                   omit_excluded=omit_excluded,
                                   sort=sort,
                                   exclude_tags=exclude_tags)
        return query.messages()

    def count_messages(self, query, *,
                       omit_excluded=EXCLUDE.TRUE,
                       sort=SORT.UNSORTED,  # Check this default
                       exclude_tags=None):
        """Search the database for messages.

        :returns: An iterator over the messages found.
        :rtype: MessageIter

        :raises ObjectDestroyedError: if used after destroyed.
        """
        query = self._create_query(query,
                                   omit_excluded=omit_excluded,
                                   sort=sort,
                                   exclude_tags=exclude_tags)
        return query.count_messages()

    def threads(self,  query, *,
                omit_excluded=EXCLUDE.TRUE,
                sort=SORT.UNSORTED,  # Check this default
                exclude_tags=None):
        query = self._create_query(query,
                                   omit_excluded=omit_excluded,
                                   sort=sort,
                                   exclude_tags=exclude_tags)
        return query.threads()

    def count_threads(self, query, *,
                      omit_excluded=EXCLUDE.TRUE,
                      sort=SORT.UNSORTED,  # Check this default
                      exclude_tags=None):
        query = self._create_query(query,
                                   omit_excluded=omit_excluded,
                                   sort=sort,
                                   exclude_tags=exclude_tags)
        return query.count_threads()

    def status_string(self):
        raise NotImplementedError

    def __repr__(self):
        return 'Database(path={self.path}, mode={self.mode})'.format(self=self)


class AtomicContext:
    """Context manager for atomic support.

    This supports the notmuch_database_begin_atomic and
    notmuch_database_end_atomic API calls.  The object can not be
    directly instantiated by the user, only via ``Database.atomic``.
    It does keep a reference to the :class:`Database` instance to keep
    the C memory alive.

    :raises XapianError: When this is raised at enter time the atomic
       section is not active.  When it is raised at exit time the
       atomic section is still active and you may need to try using
       :meth:`force_end`.
    :raises ObjectDestroyedError: if used after destroyed.
    """

    def __init__(self, db, ptr_name):
        self._db = db
        self._ptr = lambda: getattr(db, ptr_name)
        self._exit_fn = lambda: None

    def __del__(self):
        self._destroy()

    @property
    def alive(self):
        return self.parent.alive

    def _destroy(self):
        pass

    def __enter__(self):
        ret = capi.lib.notmuch_database_begin_atomic(self._ptr())
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)
        self._exit_fn = self._end_atomic
        return self

    def _end_atomic(self):
        ret = capi.lib.notmuch_database_end_atomic(self._ptr())
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)

    def __exit__(self, exc_type, exc_value, traceback):
        self._exit_fn()

    def force_end(self):
        """Force ending the atomic section.

        This can only be called once __exit__ has been called.  It
        will attempt to close the atomic section (again).  This is
        useful if the original exit raised an exception and the atomic
        section is still open.  But things are pretty ugly by now.

        :raises XapianError: If exiting fails, the atomic section is
           not ended.
        :raises UnbalancedAtomicError: If the database was currently
           not in an atomic section.
        :raises ObjectDestroyedError: if used after destroyed.
        """
        ret = capi.lib.notmuch_database_end_atomic(self._ptr())
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)

    def abort(self):
        """Abort the transaction.

        Aborting a transaction will not commit any of the changes, but
        will also implicitly close the database.
        """
        self._exit_fn = lambda: None
        self._db.close()


@functools.total_ordering
class DbRevision:
    """A database revision.

    The database revision number increases monotonically with each
    commit to the database.  Which means user-visible changes can be
    ordered.  This object is sortable with other revisions.  It
    carries the UUID of the database to ensure it is only ever
    compared with revisions from the same database.
    """

    def __init__(self, rev, uuid):
        self._rev = rev
        self._uuid = uuid

    @property
    def rev(self):
        """The revision number, a positive integer."""
        return self._rev

    @property
    def uuid(self):
        """The UUID of the database, consider this opaque."""
        return self._uuid

    def __eq__(self, other):
        if isinstance(other, self.__class__):
            if self.uuid != other.uuid:
                return False
            return self.rev == other.rev
        else:
            return NotImplemented

    def __lt__(self, other):
        if self.__class__ is other.__class__:
            if self.uuid != other.uuid:
                return False
            return self.rev < other.rev
        else:
            return NotImplemented

    def __repr__(self):
        return 'DbRevision(rev={self.rev}, uuid={self.uuid})'.format(self=self)


class IndexOptions(base.NotmuchObject):
    """Indexing options.

    This represents the indexing options which can be used to index a
    message.  See :meth:`Database.default_indexopts` to create an
    instance of this.  It can be used e.g. when indexing a new message
    using :meth:`Database.add`.
    """
    _opts_p = base.MemoryPointer()

    def __init__(self, parent, opts_p):
        self._parent = parent
        self._opts_p = opts_p

    @property
    def alive(self):
        if not self._parent.alive:
            return False
        try:
            self._opts_p
        except errors.ObjectDestroyedError:
            return False
        else:
            return True

    def _destroy(self):
        if self.alive:
            capi.lib.notmuch_indexopts_destroy(self._opts_p)
        self._opts_p = None

    @property
    def decrypt_policy(self):
        """The decryption policy.

        This is an enum from the :class:`DecryptionPolicy`.  See the
        `index.decrypt` section in :man:`notmuch-config` for details
        on the options.  **Do not set this to
        :attr:`DecryptionPolicy.TRUE`** without considering the
        security of your index.

        You can change this policy by assigning a new
        :class:`DecryptionPolicy` to this property.

        :raises ObjectDestroyedError: if used after destroyed.

        :returns: A :class:`DecryptionPolicy` enum instance.
        """
        raw = capi.lib.notmuch_indexopts_get_decrypt_policy(self._opts_p)
        return DecryptionPolicy(raw)

    @decrypt_policy.setter
    def decrypt_policy(self, val):
        ret = capi.lib.notmuch_indexopts_set_decrypt_policy(
            self._opts_p, val.value)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret, msg)
