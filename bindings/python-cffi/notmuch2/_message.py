import collections
import contextlib
import os
import pathlib
import weakref

import notmuch2._base as base
import notmuch2._capi as capi
import notmuch2._errors as errors
import notmuch2._tags as tags


__all__ = ['Message']


class Message(base.NotmuchObject):
    """An email message stored in the notmuch database retrieved via a query.

    This should not be directly created, instead it will be returned
    by calling methods on :class:`Database`.  A message keeps a
    reference to the database object since the database object can not
    be released while the message is in use.

    Note that this represents a message in the notmuch database.  For
    full email functionality you may want to use the :mod:`email`
    package from Python's standard library.  You could e.g. create
    this as such::

       notmuch_msg = db.get_message(msgid)  # or from a query
       parser = email.parser.BytesParser(policy=email.policy.default)
       with notmuch_msg.path.open('rb) as fp:
           email_msg = parser.parse(fp)

    Most commonly the functionality provided by notmuch is sufficient
    to read email however.

    Messages are considered equal when they have the same message ID.
    This is how libnotmuch treats messages as well, the
    :meth:`pathnames` function returns multiple results for
    duplicates.

    :param parent: The parent object.  This is probably one off a
       :class:`Database`, :class:`Thread` or :class:`Query`.
    :type parent: NotmuchObject
    :param db: The database instance this message is associated with.
       This could be the same as the parent.
    :type db: Database
    :param msg_p: The C pointer to the ``notmuch_message_t``.
    :type msg_p: <cdata>
    :param dup: Whether the message was a duplicate on insertion.
    :type dup: None or bool
    """
    _msg_p = base.MemoryPointer()

    def __init__(self, parent, msg_p, *, db):
        self._parent = parent
        self._msg_p = msg_p
        self._db = db

    @property
    def alive(self):
        if not self._parent.alive:
            return False
        try:
            self._msg_p
        except errors.ObjectDestroyedError:
            return False
        else:
            return True

    def __del__(self):
        self._destroy()

    def _destroy(self):
        if self.alive:
            capi.lib.notmuch_message_destroy(self._msg_p)
        self._msg_p = None

    @property
    def messageid(self):
        """The message ID as a string.

        The message ID is decoded with the ignore error handler.  This
        is fine as long as the message ID is well formed.  If it is
        not valid ASCII then this will be lossy.  So if you need to be
        able to write the exact same message ID back you should use
        :attr:`messageidb`.

        Note that notmuch will decode the message ID value and thus
        strip off the surrounding ``<`` and ``>`` characters.  This is
        different from Python's :mod:`email` package behaviour which
        leaves these characters in place.

        :returns: The message ID.
        :rtype: :class:`BinString`, this is a normal str but calling
           bytes() on it will return the original bytes used to create
           it.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        ret = capi.lib.notmuch_message_get_message_id(self._msg_p)
        return base.BinString(capi.ffi.string(ret))

    @property
    def threadid(self):
        """The thread ID.

        The thread ID is decoded with the surrogateescape error
        handler so that it is possible to reconstruct the original
        thread ID if it is not valid UTF-8.

        :returns: The thread ID.
        :rtype: :class:`BinString`, this is a normal str but calling
           bytes() on it will return the original bytes used to create
           it.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        ret = capi.lib.notmuch_message_get_thread_id(self._msg_p)
        return base.BinString(capi.ffi.string(ret))

    @property
    def path(self):
        """A pathname of the message as a pathlib.Path instance.

        If multiple files in the database contain the same message ID
        this will be just one of the files, chosen at random.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        ret = capi.lib.notmuch_message_get_filename(self._msg_p)
        return pathlib.Path(os.fsdecode(capi.ffi.string(ret)))

    @property
    def pathb(self):
        """A pathname of the message as a bytes object.

        See :attr:`path` for details, this is the same but does return
        the path as a bytes object which is faster but less convenient.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        ret = capi.lib.notmuch_message_get_filename(self._msg_p)
        return capi.ffi.string(ret)

    def filenames(self):
        """Return an iterator of all files for this message.

        If multiple files contained the same message ID they will all
        be returned here.  The files are returned as instances of
        :class:`pathlib.Path`.

        :returns: Iterator yielding :class:`pathlib.Path` instances.
        :rtype: iter

        :raises ObjectDestroyedError: if used after destroyed.
        """
        fnames_p = capi.lib.notmuch_message_get_filenames(self._msg_p)
        return PathIter(self, fnames_p)

    def filenamesb(self):
        """Return an iterator of all files for this message.

        This is like :meth:`pathnames` but the files are returned as
        byte objects instead.

        :returns: Iterator yielding :class:`bytes` instances.
        :rtype: iter

        :raises ObjectDestroyedError: if used after destroyed.
        """
        fnames_p = capi.lib.notmuch_message_get_filenames(self._msg_p)
        return FilenamesIter(self, fnames_p)

    @property
    def ghost(self):
        """Indicates whether this message is a ghost message.

        A ghost message if a message which we know exists, but it has
        no files or content associated with it.  This can happen if
        it was referenced by some other message.  Only the
        :attr:`messageid` and :attr:`threadid` attributes are valid
        for it.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        ret = capi.lib.notmuch_message_get_flag(
            self._msg_p, capi.lib.NOTMUCH_MESSAGE_FLAG_GHOST)
        return bool(ret)

    @property
    def excluded(self):
        """Indicates whether this message was excluded from the query.

        When a message is created from a search, sometimes messages
        that where excluded by the search query could still be
        returned by it, e.g. because they are part of a thread
        matching the query.  the :meth:`Database.query` method allows
        these messages to be flagged, which results in this property
        being set to *True*.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        ret = capi.lib.notmuch_message_get_flag(
            self._msg_p, capi.lib.NOTMUCH_MESSAGE_FLAG_EXCLUDED)
        return bool(ret)

    @property
    def date(self):
        """The message date as an integer.

        The time the message was sent as an integer number of seconds
        since the *epoch*, 1 Jan 1970.  This is derived from the
        message's header, you can get the original header value with
        :meth:`header`.

        :raises ObjectDestroyedError: if used after destroyed.
        """
        return capi.lib.notmuch_message_get_date(self._msg_p)

    def header(self, name):
        """Return the value of the named header.

        Returns the header from notmuch, some common headers are
        stored in the database, others are read from the file.
        Headers are returned with their newlines stripped and
        collapsed concatenated together if they occur multiple times.
        You may be better off using the standard library email
        package's ``email.message_from_file(msg.path.open())`` if that
        is not sufficient for you.

        :param header: Case-insensitive header name to retrieve.
        :type header: str or bytes

        :returns: The header value, an empty string if the header is
           not present.
        :rtype: str

        :raises LookupError: if the header is not present.
        :raises NullPointerError: For unexpected notmuch errors.
        :raises ObjectDestroyedError: if used after destroyed.
        """
        # The returned is supposedly guaranteed to be UTF-8.  Header
        # names must be ASCII as per RFC x822.
        if isinstance(name, str):
            name = name.encode('ascii')
        ret = capi.lib.notmuch_message_get_header(self._msg_p, name)
        if ret == capi.ffi.NULL:
            raise errors.NullPointerError()
        hdr = capi.ffi.string(ret)
        if not hdr:
            raise LookupError
        return hdr.decode(encoding='utf-8')

    @property
    def tags(self):
        """The tags associated with the message.

        This behaves as a set.  But removing and adding items to the
        set removes and adds them to the message in the database.

        :raises ReadOnlyDatabaseError: When manipulating tags on a
           database opened in read-only mode.
        :raises ObjectDestroyedError: if used after destroyed.
        """
        try:
            ref = self._cached_tagset
        except AttributeError:
            tagset = None
        else:
            tagset = ref()
        if tagset is None:
            tagset = tags.MutableTagSet(
                self, '_msg_p', capi.lib.notmuch_message_get_tags)
            self._cached_tagset = weakref.ref(tagset)
        return tagset

    @contextlib.contextmanager
    def frozen(self):
        """Context manager to freeze the message state.

        This allows you to perform atomic tag updates::

           with msg.frozen():
               msg.tags.clear()
               msg.tags.add('foo')

        Using This would ensure the message never ends up with no tags
        applied at all.

        It is safe to nest calls to this context manager.

        :raises ReadOnlyDatabaseError: if the database is opened in
           read-only mode.
        :raises UnbalancedFreezeThawError: if you somehow managed to
           call __exit__ of this context manager more than once.  Why
           did you do that?
        :raises ObjectDestroyedError: if used after destroyed.
        """
        ret = capi.lib.notmuch_message_freeze(self._msg_p)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)
        self._frozen = True
        try:
            yield
        except Exception:
            # Only way to "rollback" these changes is to destroy
            # ourselves and re-create.  Behold.
            msgid = self.messageid
            self._destroy()
            with contextlib.suppress(Exception):
                new = self._db.find(msgid)
                self._msg_p = new._msg_p
                new._msg_p = None
                del new
            raise
        else:
            ret = capi.lib.notmuch_message_thaw(self._msg_p)
            if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
                raise errors.NotmuchError(ret)
            self._frozen = False

    @property
    def properties(self):
        """A map of arbitrary key-value pairs associated with the message.

        Be aware that properties may be used by other extensions to
        store state in.  So delete or modify with care.

        The properties map is somewhat special.  It is essentially a
        multimap-like structure where each key can have multiple
        values.  Therefore accessing a single item using
        :meth:`PropertiesMap.get` or :meth:`PropertiesMap.__getitem__`
        will only return you the *first* item if there are multiple
        and thus are only recommended if you know there to be only one
        value.

        Instead the map has an additional :meth:`PropertiesMap.all`
        method which can be used to retrieve all properties of a given
        key.  This method also allows iterating of a a subset of the
        keys starting with a given prefix.
        """
        try:
            ref = self._cached_props
        except AttributeError:
            props = None
        else:
            props = ref()
        if props is None:
            props = PropertiesMap(self, '_msg_p')
            self._cached_props = weakref.ref(props)
        return props

    def replies(self):
        """Return an iterator of all replies to this message.

        This method will only work if the message was created from a
        thread.  Otherwise it will yield no results.

        :returns: An iterator yielding :class:`Message` instances.
        :rtype: MessageIter
        """
        # The notmuch_messages_valid call accepts NULL and this will
        # become an empty iterator, raising StopIteration immediately.
        # Hence no return value checking here.
        msgs_p = capi.lib.notmuch_message_get_replies(self._msg_p)
        return MessageIter(self, msgs_p, db=self._db)

    def __hash__(self):
        return hash(self.messageid)

    def __eq__(self, other):
        if isinstance(other, self.__class__):
            return self.messageid == other.messageid


class OwnedMessage(Message):
    """An email message owned by parent thread object.

    This subclass of Message is used for messages that are retrieved
    from the notmuch database via a parent :class:`notmuch2.Thread`
    object, which "owns" this message.  This means that when this
    message object is destroyed, by calling :func:`del` or
    :meth:`_destroy` directly or indirectly, the message is not freed
    in the notmuch API and the parent :class:`notmuch2.Thread` object
    can return the same object again when needed.
    """

    @property
    def alive(self):
        return self._parent.alive

    def _destroy(self):
        pass


class FilenamesIter(base.NotmuchIter):
    """Iterator for binary filenames objects."""

    def __init__(self, parent, iter_p):
        super().__init__(parent, iter_p,
                         fn_destroy=capi.lib.notmuch_filenames_destroy,
                         fn_valid=capi.lib.notmuch_filenames_valid,
                         fn_get=capi.lib.notmuch_filenames_get,
                         fn_next=capi.lib.notmuch_filenames_move_to_next)

    def __next__(self):
        fname = super().__next__()
        return capi.ffi.string(fname)


class PathIter(FilenamesIter):
    """Iterator for pathlib.Path objects."""

    def __next__(self):
        fname = super().__next__()
        return pathlib.Path(os.fsdecode(fname))


class PropertiesMap(base.NotmuchObject, collections.abc.MutableMapping):
    """A mutable mapping to manage properties.

    Both keys and values of properties are supposed to be UTF-8
    strings in libnotmuch.  However since the uderlying API uses
    bytestrings you can use either str or bytes to represent keys and
    all returned keys and values use :class:`BinString`.

    Also be aware that ``iter(this_map)`` will return duplicate keys,
    while the :class:`collections.abc.KeysView` returned by
    :meth:`keys` is a :class:`collections.abc.Set` subclass.  This
    means the former will yield duplicate keys while the latter won't.
    It also means ``len(list(iter(this_map)))`` could be different
    than ``len(this_map.keys())``.  ``len(this_map)`` will correspond
    with the length of the default iterator.

    Be aware that libnotmuch exposes all of this as iterators, so
    quite a few operations have O(n) performance instead of the usual
    O(1).
    """
    Property = collections.namedtuple('Property', ['key', 'value'])
    _marker = object()

    def __init__(self, msg, ptr_name):
        self._msg = msg
        self._ptr = lambda: getattr(msg, ptr_name)

    @property
    def alive(self):
        if not self._msg.alive:
            return False
        try:
            self._ptr
        except errors.ObjectDestroyedError:
            return False
        else:
            return True

    def _destroy(self):
        pass

    def __iter__(self):
        """Return an iterator which iterates over the keys.

        Be aware that a single key may have multiple values associated
        with it, if so it will appear multiple times here.
        """
        iter_p = capi.lib.notmuch_message_get_properties(self._ptr(), b'', 0)
        return PropertiesKeyIter(self, iter_p)

    def __len__(self):
        iter_p = capi.lib.notmuch_message_get_properties(self._ptr(), b'', 0)
        it = base.NotmuchIter(
            self, iter_p,
            fn_destroy=capi.lib.notmuch_message_properties_destroy,
            fn_valid=capi.lib.notmuch_message_properties_valid,
            fn_get=capi.lib.notmuch_message_properties_key,
            fn_next=capi.lib.notmuch_message_properties_move_to_next,
        )
        return len(list(it))

    def __getitem__(self, key):
        """Return **the first** peroperty associated with a key."""
        if isinstance(key, str):
            key = key.encode('utf-8')
        value_pp = capi.ffi.new('char**')
        ret = capi.lib.notmuch_message_get_property(self._ptr(), key, value_pp)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)
        if value_pp[0] == capi.ffi.NULL:
            raise KeyError
        return base.BinString.from_cffi(value_pp[0])

    def keys(self):
        """Return a :class:`collections.abc.KeysView` for this map.

        Even when keys occur multiple times this is a subset of set()
        so will only contain them once.
        """
        return collections.abc.KeysView({k: None for k in self})

    def items(self):
        """Return a :class:`collections.abc.ItemsView` for this map.

        The ItemsView treats a ``(key, value)`` pair as unique, so
        dupcliate ``(key, value)`` pairs will be merged together.
        However duplicate keys with different values will be returned.
        """
        items = set()
        props_p = capi.lib.notmuch_message_get_properties(self._ptr(), b'', 0)
        while capi.lib.notmuch_message_properties_valid(props_p):
            key = capi.lib.notmuch_message_properties_key(props_p)
            value = capi.lib.notmuch_message_properties_value(props_p)
            items.add((base.BinString.from_cffi(key),
                       base.BinString.from_cffi(value)))
            capi.lib.notmuch_message_properties_move_to_next(props_p)
        capi.lib.notmuch_message_properties_destroy(props_p)
        return PropertiesItemsView(items)

    def values(self):
        """Return a :class:`collecions.abc.ValuesView` for this map.

        All unique property values are included in the view.
        """
        values = set()
        props_p = capi.lib.notmuch_message_get_properties(self._ptr(), b'', 0)
        while capi.lib.notmuch_message_properties_valid(props_p):
            value = capi.lib.notmuch_message_properties_value(props_p)
            values.add(base.BinString.from_cffi(value))
            capi.lib.notmuch_message_properties_move_to_next(props_p)
        capi.lib.notmuch_message_properties_destroy(props_p)
        return PropertiesValuesView(values)

    def __setitem__(self, key, value):
        """Add a key-value pair to the properties.

        You may prefer to use :meth:`add` for clarity since this
        method usually implies implicit overwriting of an existing key
        if it exists, while for properties this is not the case.
        """
        self.add(key, value)

    def add(self, key, value):
        """Add a key-value pair to the properties."""
        if isinstance(key, str):
            key = key.encode('utf-8')
        if isinstance(value, str):
            value = value.encode('utf-8')
        ret = capi.lib.notmuch_message_add_property(self._ptr(), key, value)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)

    def __delitem__(self, key):
        """Remove all properties with this key."""
        if isinstance(key, str):
            key = key.encode('utf-8')
        ret = capi.lib.notmuch_message_remove_all_properties(self._ptr(), key)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)

    def remove(self, key, value):
        """Remove a key-value pair from the properties."""
        if isinstance(key, str):
            key = key.encode('utf-8')
        if isinstance(value, str):
            value = value.encode('utf-8')
        ret = capi.lib.notmuch_message_remove_property(self._ptr(), key, value)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)

    def pop(self, key, default=_marker):
        try:
            value = self[key]
        except KeyError:
            if default is self._marker:
                raise
            else:
                return default
        else:
            self.remove(key, value)
            return value

    def popitem(self):
        try:
            key = next(iter(self))
        except StopIteration:
            raise KeyError
        value = self.pop(key)
        return (key, value)

    def clear(self):
        ret = capi.lib.notmuch_message_remove_all_properties(self._ptr(),
                                                             capi.ffi.NULL)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)

    def getall(self, prefix='', *, exact=False):
        """Return an iterator yielding all properties for a given key prefix.

        The returned iterator yields all peroperties which start with
        a given key prefix as ``(key, value)`` namedtuples.  If called
        with ``exact=True`` then only properties which exactly match
        the prefix are returned, those a key longer than the prefix
        will not be included.

        :param prefix: The prefix of the key.
        """
        if isinstance(prefix, str):
            prefix = prefix.encode('utf-8')
        props_p = capi.lib.notmuch_message_get_properties(self._ptr(),
                                                          prefix, exact)
        return PropertiesIter(self, props_p)


class PropertiesKeyIter(base.NotmuchIter):

    def __init__(self, parent, iter_p):
        super().__init__(
            parent,
            iter_p,
            fn_destroy=capi.lib.notmuch_message_properties_destroy,
            fn_valid=capi.lib.notmuch_message_properties_valid,
            fn_get=capi.lib.notmuch_message_properties_key,
            fn_next=capi.lib.notmuch_message_properties_move_to_next)

    def __next__(self):
        item = super().__next__()
        return base.BinString.from_cffi(item)


class PropertiesIter(base.NotmuchIter):

    def __init__(self, parent, iter_p):
        super().__init__(
            parent,
            iter_p,
            fn_destroy=capi.lib.notmuch_message_properties_destroy,
            fn_valid=capi.lib.notmuch_message_properties_valid,
            fn_get=capi.lib.notmuch_message_properties_key,
            fn_next=capi.lib.notmuch_message_properties_move_to_next,
        )

    def __next__(self):
        if not self._fn_valid(self._iter_p):
            self._destroy()
            raise StopIteration
        key = capi.lib.notmuch_message_properties_key(self._iter_p)
        value = capi.lib.notmuch_message_properties_value(self._iter_p)
        capi.lib.notmuch_message_properties_move_to_next(self._iter_p)
        return PropertiesMap.Property(base.BinString.from_cffi(key),
                                      base.BinString.from_cffi(value))


class PropertiesItemsView(collections.abc.Set):

    __slots__ = ('_items',)

    def __init__(self, items):
        self._items = items

    @classmethod
    def _from_iterable(self, it):
        return set(it)

    def __len__(self):
        return len(self._items)

    def __contains__(self, item):
        return item in self._items

    def __iter__(self):
        yield from self._items


collections.abc.ItemsView.register(PropertiesItemsView)


class PropertiesValuesView(collections.abc.Set):

    __slots__ = ('_values',)

    def __init__(self, values):
        self._values = values

    def __len__(self):
        return len(self._values)

    def __contains__(self, value):
        return value in self._values

    def __iter__(self):
        yield from self._values


collections.abc.ValuesView.register(PropertiesValuesView)


class MessageIter(base.NotmuchIter):

    def __init__(self, parent, msgs_p, *, db, msg_cls=Message):
        self._db = db
        self._msg_cls = msg_cls
        super().__init__(parent, msgs_p,
                         fn_destroy=capi.lib.notmuch_messages_destroy,
                         fn_valid=capi.lib.notmuch_messages_valid,
                         fn_get=capi.lib.notmuch_messages_get,
                         fn_next=capi.lib.notmuch_messages_move_to_next)

    def __next__(self):
        msg_p = super().__next__()
        return self._msg_cls(self, msg_p, db=self._db)
