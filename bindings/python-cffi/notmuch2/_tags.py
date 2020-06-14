import collections.abc

import notmuch2._base as base
import notmuch2._capi as capi
import notmuch2._errors as errors


__all__ = ['ImmutableTagSet', 'MutableTagSet', 'TagsIter']


class ImmutableTagSet(base.NotmuchObject, collections.abc.Set):
    """The tags associated with a message thread or whole database.

    Both a thread as well as the database expose the union of all tags
    in messages associated with them.  This exposes these as a
    :class:`collections.abc.Set` object.

    Note that due to the underlying notmuch API the performance of the
    implementation is not the same as you would expect from normal
    sets.  E.g. the :meth:`__contains__` and :meth:`__len__` are O(n)
    rather then O(1).

    Tags are internally stored as bytestrings but normally exposed as
    unicode strings using the UTF-8 encoding and the *ignore* decoder
    error handler.  However the :meth:`iter` method can be used to
    return tags as bytestrings or using a different error handler.

    Note that when doing arithmetic operations on tags, this class
    will return a plain normal set as it is no longer associated with
    the message.

    :param parent: the parent object
    :param ptr_name: the name of the attribute on the parent which will
       return the memory pointer.  This allows this object to
       access the pointer via the parent's descriptor and thus
       trigger :class:`MemoryPointer`'s memory safety.
    :param cffi_fn: the callable CFFI wrapper to retrieve the tags
       iter.  This can be one of notmuch_database_get_all_tags,
       notmuch_thread_get_tags or notmuch_message_get_tags.
    """

    def __init__(self, parent, ptr_name, cffi_fn):
        self._parent = parent
        self._ptr = lambda: getattr(parent, ptr_name)
        self._cffi_fn = cffi_fn

    def __del__(self):
        self._destroy()

    @property
    def alive(self):
        return self._parent.alive

    def _destroy(self):
        pass

    @classmethod
    def _from_iterable(cls, it):
        return set(it)

    def __iter__(self):
        """Return an iterator over the tags.

        Tags are yielded as unicode strings, decoded using the
        "ignore" error handler.

        :raises NullPointerError: If the iterator can not be created.
        """
        return self.iter(encoding='utf-8', errors='ignore')

    def iter(self, *, encoding=None, errors='strict'):
        """Aternate iterator constructor controlling string decoding.

        Tags are stored as bytes in the notmuch database, in Python
        it's easier to work with unicode strings and thus is what the
        normal iterator returns.  However this method allows you to
        specify how you would like to get the tags, defaulting to the
        bytestring representation instead of unicode strings.

        :param encoding: Which codec to use.  The default *None* does not
           decode at all and will return the unmodified bytes.
           Otherwise this is passed on to :func:`str.decode`.
        :param errors: If using a codec, this is the error handler.
           See :func:`str.decode` to which this is passed on.

        :raises NullPointerError: When things do not go as planned.
        """
        # self._cffi_fn should point either to
        # notmuch_database_get_all_tags, notmuch_thread_get_tags or
        # notmuch_message_get_tags.  nothmuch.h suggests these never
        # fail, let's handle NULL anyway.
        tags_p = self._cffi_fn(self._ptr())
        if tags_p == capi.ffi.NULL:
            raise errors.NullPointerError()
        tags = TagsIter(self, tags_p, encoding=encoding, errors=errors)
        return tags

    def __len__(self):
        return sum(1 for t in self)

    def __contains__(self, tag):
        if isinstance(tag, str):
            tag = tag.encode()
        for msg_tag in self.iter():
            if tag == msg_tag:
                return True
        else:
            return False

    def __eq__(self, other):
        return tuple(sorted(self.iter())) == tuple(sorted(other.iter()))

    def issubset(self, other):
        return self <= other

    def issuperset(self, other):
        return self >= other

    def union(self, other):
        return self | other

    def intersection(self, other):
        return self & other

    def difference(self, other):
        return self - other

    def symmetric_difference(self, other):
        return self ^ other

    def copy(self):
        return set(self)

    def __hash__(self):
        return hash(tuple(self.iter()))

    def __repr__(self):
        return '<{name} object at 0x{addr:x} tags={{{tags}}}>'.format(
            name=self.__class__.__name__,
            addr=id(self),
            tags=', '.join(repr(t) for t in self))


class MutableTagSet(ImmutableTagSet, collections.abc.MutableSet):
    """The tags associated with a message.

    This is a :class:`collections.abc.MutableSet` object which can be
    used to manipulate the tags of a message.

    Note that due to the underlying notmuch API the performance of the
    implementation is not the same as you would expect from normal
    sets.  E.g. the ``in`` operator and variants are O(n) rather then
    O(1).

    Tags are bytestrings and calling ``iter()`` will return an
    iterator yielding bytestrings.  However the :meth:`iter` method
    can be used to return tags as unicode strings, while all other
    operations accept either byestrings or unicode strings.  In case
    unicode strings are used they will be encoded using utf-8 before
    being passed to notmuch.
    """

    # Since we subclass ImmutableTagSet we inherit a __hash__.  But we
    # are mutable, setting it to None will make the Python machinary
    # recognise us as unhashable.
    __hash__ = None

    def add(self, tag):
        """Add a tag to the message.

        :param tag: The tag to add.
        :type tag: str or bytes.  A str will be encoded using UTF-8.

        :param sync_flags: Whether to sync the maildir flags with the
           new set of tags.  Leaving this as *None* respects the
           configuration set in the database, while *True* will always
           sync and *False* will never sync.
        :param sync_flags: NoneType or bool

        :raises TypeError: If the tag is not a valid type.
        :raises TagTooLongError: If the added tag exceeds the maximum
           lenght, see ``notmuch_cffi.NOTMUCH_TAG_MAX``.
        :raises ReadOnlyDatabaseError: If the database is opened in
           read-only mode.
        """
        if isinstance(tag, str):
            tag = tag.encode()
        if not isinstance(tag, bytes):
            raise TypeError('Not a valid type for a tag: {}'.format(type(tag)))
        ret = capi.lib.notmuch_message_add_tag(self._ptr(), tag)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)

    def discard(self, tag):
        """Remove a tag from the message.

        :param tag: The tag to remove.
        :type tag: str of bytes.  A str will be encoded using UTF-8.
        :param sync_flags: Whether to sync the maildir flags with the
           new set of tags.  Leaving this as *None* respects the
           configuration set in the database, while *True* will always
           sync and *False* will never sync.
        :param sync_flags: NoneType or bool

        :raises TypeError: If the tag is not a valid type.
        :raises TagTooLongError: If the tag exceeds the maximum
           lenght, see ``notmuch_cffi.NOTMUCH_TAG_MAX``.
        :raises ReadOnlyDatabaseError: If the database is opened in
           read-only mode.
        """
        if isinstance(tag, str):
            tag = tag.encode()
        if not isinstance(tag, bytes):
            raise TypeError('Not a valid type for a tag: {}'.format(type(tag)))
        ret = capi.lib.notmuch_message_remove_tag(self._ptr(), tag)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)

    def clear(self):
        """Remove all tags from the message.

        :raises ReadOnlyDatabaseError: If the database is opened in
           read-only mode.
        """
        ret = capi.lib.notmuch_message_remove_all_tags(self._ptr())
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)

    def from_maildir_flags(self):
        """Update the tags based on the state in the message's maildir flags.

        This function examines the filenames of 'message' for maildir
        flags, and adds or removes tags on 'message' as follows when
        these flags are present:

        Flag    Action if present
        ----    -----------------
        'D'     Adds the "draft" tag to the message
        'F'     Adds the "flagged" tag to the message
        'P'     Adds the "passed" tag to the message
        'R'     Adds the "replied" tag to the message
        'S'     Removes the "unread" tag from the message

        For each flag that is not present, the opposite action
        (add/remove) is performed for the corresponding tags.

        Flags are identified as trailing components of the filename
        after a sequence of ":2,".

        If there are multiple filenames associated with this message,
        the flag is considered present if it appears in one or more
        filenames. (That is, the flags from the multiple filenames are
        combined with the logical OR operator.)
        """
        ret = capi.lib.notmuch_message_maildir_flags_to_tags(self._ptr())
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)

    def to_maildir_flags(self):
        """Update the message's maildir flags based on the notmuch tags.

        If the message's filename is in a maildir directory, that is a
        directory named ``new`` or ``cur``, and has a valid maildir
        filename then the flags will be added as such:

        'D' if the message has the "draft" tag
        'F' if the message has the "flagged" tag
        'P' if the message has the "passed" tag
        'R' if the message has the "replied" tag
        'S' if the message does not have the "unread" tag

        Any existing flags unmentioned in the list above will be
        preserved in the renaming.

        Also, if this filename is in a directory named "new", rename it to
        be within the neighboring directory named "cur".

        In case there are multiple files associated with the message
        all filenames will get the same logic applied.
        """
        ret = capi.lib.notmuch_message_tags_to_maildir_flags(self._ptr())
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)


class TagsIter(base.NotmuchObject, collections.abc.Iterator):
    """Iterator over tags.

    This is only an interator, not a container so calling
    :meth:`__iter__` does not return a new, replenished iterator but
    only itself.

    :param parent: The parent object to keep alive.
    :param tags_p: The CFFI pointer to the C-level tags iterator.
    :param encoding: Which codec to use.  The default *None* does not
       decode at all and will return the unmodified bytes.
       Otherwise this is passed on to :func:`str.decode`.
    :param errors: If using a codec, this is the error handler.
       See :func:`str.decode` to which this is passed on.

    :raises ObjectDestroyedError: if used after destroyed.
    """
    _tags_p = base.MemoryPointer()

    def __init__(self, parent, tags_p, *, encoding=None, errors='strict'):
        self._parent = parent
        self._tags_p = tags_p
        self._encoding = encoding
        self._errors = errors

    def __del__(self):
        self._destroy()

    @property
    def alive(self):
        if not self._parent.alive:
            return False
        try:
            self._tags_p
        except errors.ObjectDestroyedError:
            return False
        else:
            return True

    def _destroy(self):
        if self.alive:
            try:
                capi.lib.notmuch_tags_destroy(self._tags_p)
            except errors.ObjectDestroyedError:
                pass
        self._tags_p = None

    def __iter__(self):
        """Return the iterator itself.

        Note that as this is an iterator and not a container this will
        not return a new iterator.  Thus any elements already consumed
        will not be yielded by the :meth:`__next__` method anymore.
        """
        return self

    def __next__(self):
        if not capi.lib.notmuch_tags_valid(self._tags_p):
            self._destroy()
            raise StopIteration()
        tag_p = capi.lib.notmuch_tags_get(self._tags_p)
        tag = capi.ffi.string(tag_p)
        if self._encoding:
            tag = tag.decode(encoding=self._encoding, errors=self._errors)
        capi.lib.notmuch_tags_move_to_next(self._tags_p)
        return tag

    def __repr__(self):
        try:
            self._tags_p
        except errors.ObjectDestroyedError:
            return '<TagsIter (exhausted)>'
        else:
            return '<TagsIter>'
