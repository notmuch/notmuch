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
               Jesse Rosenthal <jrosenthal@jhu.edu>
"""


from ctypes import c_char_p, c_long, c_uint, c_int
from datetime import date
from .globals import (
    nmlib,
    Enum,
    _str,
    Python3StringMixIn,
    NotmuchTagsP,
    NotmuchMessageP,
    NotmuchMessagesP,
    NotmuchFilenamesP,
)
from .errors import (
    STATUS,
    NotmuchError,
    NullPointerError,
    NotInitializedError,
)
from .tag import Tags
from .filenames import Filenames

import email


class Message(Python3StringMixIn):
    """Represents a single Email message

    Technically, this wraps the underlying *notmuch_message_t*
    structure. A user will usually not create these objects themselves
    but get them as search results.

    As it implements :meth:`__cmp__`, it is possible to compare two
    :class:`Message`\s using `if msg1 == msg2: ...`.
    """

    """notmuch_message_get_filename (notmuch_message_t *message)"""
    _get_filename = nmlib.notmuch_message_get_filename
    _get_filename.argtypes = [NotmuchMessageP]
    _get_filename.restype = c_char_p

    """return all filenames for a message"""
    _get_filenames = nmlib.notmuch_message_get_filenames
    _get_filenames.argtypes = [NotmuchMessageP]
    _get_filenames.restype = NotmuchFilenamesP

    """notmuch_message_get_flag"""
    _get_flag = nmlib.notmuch_message_get_flag
    _get_flag.argtypes = [NotmuchMessageP, c_uint]
    _get_flag.restype = bool

    """notmuch_message_set_flag"""
    _set_flag = nmlib.notmuch_message_set_flag
    _set_flag.argtypes = [NotmuchMessageP, c_uint, c_int]
    _set_flag.restype = None

    """notmuch_message_get_message_id (notmuch_message_t *message)"""
    _get_message_id = nmlib.notmuch_message_get_message_id
    _get_message_id.argtypes = [NotmuchMessageP]
    _get_message_id.restype = c_char_p

    """notmuch_message_get_thread_id"""
    _get_thread_id = nmlib.notmuch_message_get_thread_id
    _get_thread_id.argtypes = [NotmuchMessageP]
    _get_thread_id.restype = c_char_p

    """notmuch_message_get_replies"""
    _get_replies = nmlib.notmuch_message_get_replies
    _get_replies.argtypes = [NotmuchMessageP]
    _get_replies.restype = NotmuchMessagesP

    """notmuch_message_get_tags (notmuch_message_t *message)"""
    _get_tags = nmlib.notmuch_message_get_tags
    _get_tags.argtypes = [NotmuchMessageP]
    _get_tags.restype = NotmuchTagsP

    _get_date = nmlib.notmuch_message_get_date
    _get_date.argtypes = [NotmuchMessageP]
    _get_date.restype = c_long

    _get_header = nmlib.notmuch_message_get_header
    _get_header.argtypes = [NotmuchMessageP, c_char_p]
    _get_header.restype = c_char_p

    """notmuch_status_t ..._maildir_flags_to_tags (notmuch_message_t *)"""
    _tags_to_maildir_flags = nmlib.notmuch_message_tags_to_maildir_flags
    _tags_to_maildir_flags.argtypes = [NotmuchMessageP]
    _tags_to_maildir_flags.restype = c_int

    """notmuch_status_t ..._tags_to_maildir_flags (notmuch_message_t *)"""
    _maildir_flags_to_tags = nmlib.notmuch_message_maildir_flags_to_tags
    _maildir_flags_to_tags.argtypes = [NotmuchMessageP]
    _maildir_flags_to_tags.restype = c_int

    #Constants: Flags that can be set/get with set_flag
    FLAG = Enum(['MATCH'])

    def __init__(self, msg_p, parent=None):
        """
        :param msg_p: A pointer to an internal notmuch_message_t
            Structure.  If it is `None`, we will raise an
            :exc:`NullPointerError`.

        :param parent: A 'parent' object is passed which this message is
              derived from. We save a reference to it, so we can
              automatically delete the parent object once all derived
              objects are dead.
        """
        if not msg_p:
            raise NullPointerError()
        self._msg = msg_p
        #keep reference to parent, so we keep it alive
        self._parent = parent

    def get_message_id(self):
        """Returns the message ID

        :returns: String with a message ID
        :raises: :exc:`NotInitializedError` if the message
                    is not initialized.
        """
        if not self._msg:
            raise NotInitializedError()
        return Message._get_message_id(self._msg).decode('utf-8', 'ignore')

    def get_thread_id(self):
        """Returns the thread ID

        The returned string belongs to 'message' will only be valid for as
        long as the message is valid.

        This function will not return `None` since Notmuch ensures that every
        message belongs to a single thread.

        :returns: String with a thread ID
        :raises: :exc:`NotInitializedError` if the message
                    is not initialized.
        """
        if not self._msg:
            raise NotInitializedError()

        return Message._get_thread_id(self._msg).decode('utf-8', 'ignore')

    def get_replies(self):
        """Gets all direct replies to this message as :class:`Messages`
        iterator

        .. note::

            This call only makes sense if 'message' was ultimately obtained from
            a :class:`Thread` object, (such as by coming directly from the
            result of calling :meth:`Thread.get_toplevel_messages` or by any
            number of subsequent calls to :meth:`get_replies`). If this message
            was obtained through some non-thread means, (such as by a call to
            :meth:`Query.search_messages`), then this function will return
            an empty Messages iterator.

        :returns: :class:`Messages`.
        :raises: :exc:`NotInitializedError` if the message
                    is not initialized.
        """
        if not self._msg:
            raise NotInitializedError()

        msgs_p = Message._get_replies(self._msg)

        from .messages import Messages, EmptyMessagesResult

        if not msgs_p:
            return EmptyMessagesResult(self)

        return Messages(msgs_p, self)

    def get_date(self):
        """Returns time_t of the message date

        For the original textual representation of the Date header from the
        message call notmuch_message_get_header() with a header value of
        "date".

        :returns: A time_t timestamp.
        :rtype: c_unit64
        :raises: :exc:`NotInitializedError` if the message
                    is not initialized.
        """
        if not self._msg:
            raise NotInitializedError()
        return Message._get_date(self._msg)

    def get_header(self, header):
        """Get the value of the specified header.

        The value will be read from the actual message file, not from
        the notmuch database. The header name is case insensitive.

        Returns an empty string ("") if the message does not contain a
        header line matching 'header'.

        :param header: The name of the header to be retrieved.
                       It is not case-sensitive.
        :type header: str
        :returns: The header value as string
        :raises: :exc:`NotInitializedError` if the message is not
                 initialized
        :raises: :exc:`NullPointerError` if any error occured
        """
        if not self._msg:
            raise NotInitializedError()

        #Returns NULL if any error occurs.
        header = Message._get_header(self._msg, _str(header))
        if header == None:
            raise NullPointerError()
        return header.decode('UTF-8', 'ignore')

    def get_filename(self):
        """Returns the file path of the message file

        :returns: Absolute file path & name of the message file
        :raises: :exc:`NotInitializedError` if the message
              is not initialized.
        """
        if not self._msg:
            raise NotInitializedError()
        return Message._get_filename(self._msg).decode('utf-8', 'ignore')

    def get_filenames(self):
        """Get all filenames for the email corresponding to 'message'

        Returns a Filenames() generator with all absolute filepaths for
        messages recorded to have the same Message-ID. These files must
        not necessarily have identical content."""
        if not self._msg:
            raise NotInitializedError()

        files_p = Message._get_filenames(self._msg)

        return Filenames(files_p, self)

    def get_flag(self, flag):
        """Checks whether a specific flag is set for this message

        The method :meth:`Query.search_threads` sets
        *Message.FLAG.MATCH* for those messages that match the
        query. This method allows us to get the value of this flag.

        :param flag: One of the :attr:`Message.FLAG` values (currently only
                     *Message.FLAG.MATCH*
        :returns: An unsigned int (0/1), indicating whether the flag is set.
        :raises: :exc:`NotInitializedError` if the message
              is not initialized.
        """
        if not self._msg:
            raise NotInitializedError()
        return Message._get_flag(self._msg, flag)

    def set_flag(self, flag, value):
        """Sets/Unsets a specific flag for this message

        :param flag: One of the :attr:`Message.FLAG` values (currently only
                     *Message.FLAG.MATCH*
        :param value: A bool indicating whether to set or unset the flag.

        :raises: :exc:`NotInitializedError` if the message
              is not initialized.
        """
        if not self._msg:
            raise NotInitializedError()
        self._set_flag(self._msg, flag, value)

    def get_tags(self):
        """Returns the message tags

        :returns: A :class:`Tags` iterator.
        :raises: :exc:`NotInitializedError` if the message is not
                 initialized
        :raises: :exc:`NullPointerError` if any error occured
        """
        if not self._msg:
            raise NotInitializedError()

        tags_p = Message._get_tags(self._msg)
        if not tags_p:
            raise NullPointerError()
        return Tags(tags_p, self)

    _add_tag = nmlib.notmuch_message_add_tag
    _add_tag.argtypes = [NotmuchMessageP, c_char_p]
    _add_tag.restype = c_uint

    def add_tag(self, tag, sync_maildir_flags=False):
        """Adds a tag to the given message

        Adds a tag to the current message. The maximal tag length is defined in
        the notmuch library and is currently 200 bytes.

        :param tag: String with a 'tag' to be added.

        :param sync_maildir_flags: If notmuch configuration is set to do
            this, add maildir flags corresponding to notmuch tags. See
            underlying method :meth:`tags_to_maildir_flags`. Use False
            if you want to add/remove many tags on a message without
            having to physically rename the file every time. Do note,
            that this will do nothing when a message is frozen, as tag
            changes will not be committed to the database yet.

        :returns: STATUS.SUCCESS if the tag was successfully added.
                  Raises an exception otherwise.
        :raises: :exc:`NullPointerError` if the `tag` argument is NULL
        :raises: :exc:`TagTooLongError` if the length of `tag` exceeds
                 Message.NOTMUCH_TAG_MAX)
        :raises: :exc:`ReadOnlyDatabaseError` if the database was opened
                 in read-only mode so message cannot be modified
        :raises: :exc:`NotInitializedError` if message has not been
                 initialized
        """
        if not self._msg:
            raise NotInitializedError()

        status = self._add_tag(self._msg, _str(tag))

        # bail out on failure
        if status != STATUS.SUCCESS:
            raise NotmuchError(status)

        if sync_maildir_flags:
            self.tags_to_maildir_flags()
        return STATUS.SUCCESS

    _remove_tag = nmlib.notmuch_message_remove_tag
    _remove_tag.argtypes = [NotmuchMessageP, c_char_p]
    _remove_tag.restype = c_uint

    def remove_tag(self, tag, sync_maildir_flags=False):
        """Removes a tag from the given message

        If the message has no such tag, this is a non-operation and
        will report success anyway.

        :param tag: String with a 'tag' to be removed.
        :param sync_maildir_flags: If notmuch configuration is set to do
            this, add maildir flags corresponding to notmuch tags. See
            underlying method :meth:`tags_to_maildir_flags`. Use False
            if you want to add/remove many tags on a message without
            having to physically rename the file every time. Do note,
            that this will do nothing when a message is frozen, as tag
            changes will not be committed to the database yet.

        :returns: STATUS.SUCCESS if the tag was successfully removed or if
                  the message had no such tag.
                  Raises an exception otherwise.
        :raises: :exc:`NullPointerError` if the `tag` argument is NULL
        :raises: :exc:`TagTooLongError` if the length of `tag` exceeds
                 Message.NOTMUCH_TAG_MAX)
        :raises: :exc:`ReadOnlyDatabaseError` if the database was opened
                 in read-only mode so message cannot be modified
        :raises: :exc:`NotInitializedError` if message has not been
                 initialized
        """
        if not self._msg:
            raise NotInitializedError()

        status = self._remove_tag(self._msg, _str(tag))
        # bail out on error
        if status != STATUS.SUCCESS:
            raise NotmuchError(status)

        if sync_maildir_flags:
            self.tags_to_maildir_flags()
        return STATUS.SUCCESS

    _remove_all_tags = nmlib.notmuch_message_remove_all_tags
    _remove_all_tags.argtypes = [NotmuchMessageP]
    _remove_all_tags.restype = c_uint

    def remove_all_tags(self, sync_maildir_flags=False):
        """Removes all tags from the given message.

        See :meth:`freeze` for an example showing how to safely
        replace tag values.


        :param sync_maildir_flags: If notmuch configuration is set to do
            this, add maildir flags corresponding to notmuch tags. See
            :meth:`tags_to_maildir_flags`. Use False if you want to
            add/remove many tags on a message without having to
            physically rename the file every time. Do note, that this
            will do nothing when a message is frozen, as tag changes
            will not be committed to the database yet.

        :returns: STATUS.SUCCESS if the tags were successfully removed.
                  Raises an exception otherwise.
        :raises: :exc:`ReadOnlyDatabaseError` if the database was opened
                 in read-only mode so message cannot be modified
        :raises: :exc:`NotInitializedError` if message has not been
                 initialized
        """
        if not self._msg:
            raise NotInitializedError()

        status = self._remove_all_tags(self._msg)

        # bail out on error
        if status != STATUS.SUCCESS:
            raise NotmuchError(status)

        if sync_maildir_flags:
            self.tags_to_maildir_flags()
        return STATUS.SUCCESS

    _freeze = nmlib.notmuch_message_freeze
    _freeze.argtypes = [NotmuchMessageP]
    _freeze.restype = c_uint

    def freeze(self):
        """Freezes the current state of 'message' within the database

        This means that changes to the message state, (via :meth:`add_tag`,
        :meth:`remove_tag`, and :meth:`remove_all_tags`), will not be
        committed to the database until the message is :meth:`thaw` ed.

        Multiple calls to freeze/thaw are valid and these calls will
        "stack". That is there must be as many calls to thaw as to freeze
        before a message is actually thawed.

        The ability to do freeze/thaw allows for safe transactions to
        change tag values. For example, explicitly setting a message to
        have a given set of tags might look like this::

          msg.freeze()
          msg.remove_all_tags(False)
          for tag in new_tags:
              msg.add_tag(tag, False)
          msg.thaw()
          msg.tags_to_maildir_flags()

        With freeze/thaw used like this, the message in the database is
        guaranteed to have either the full set of original tag values, or
        the full set of new tag values, but nothing in between.

        Imagine the example above without freeze/thaw and the operation
        somehow getting interrupted. This could result in the message being
        left with no tags if the interruption happened after
        :meth:`remove_all_tags` but before :meth:`add_tag`.

        :returns: STATUS.SUCCESS if the message was successfully frozen.
                  Raises an exception otherwise.
        :raises: :exc:`ReadOnlyDatabaseError` if the database was opened
                 in read-only mode so message cannot be modified
        :raises: :exc:`NotInitializedError` if message has not been
                 initialized
        """
        if not self._msg:
            raise NotInitializedError()

        status = self._freeze(self._msg)

        if STATUS.SUCCESS == status:
            # return on success
            return status

        raise NotmuchError(status)

    _thaw = nmlib.notmuch_message_thaw
    _thaw.argtypes = [NotmuchMessageP]
    _thaw.restype = c_uint

    def thaw(self):
        """Thaws the current 'message'

        Thaw the current 'message', synchronizing any changes that may have
        occurred while 'message' was frozen into the notmuch database.

        See :meth:`freeze` for an example of how to use this
        function to safely provide tag changes.

        Multiple calls to freeze/thaw are valid and these calls with
        "stack". That is there must be as many calls to thaw as to freeze
        before a message is actually thawed.

        :returns: STATUS.SUCCESS if the message was successfully frozen.
                  Raises an exception otherwise.
        :raises: :exc:`UnbalancedFreezeThawError` if an attempt was made
                 to thaw an unfrozen message. That is, there have been
                 an unbalanced number of calls to :meth:`freeze` and
                 :meth:`thaw`.
        :raises: :exc:`NotInitializedError` if message has not been
                 initialized
        """
        if not self._msg:
            raise NotInitializedError()

        status = self._thaw(self._msg)

        if STATUS.SUCCESS == status:
            # return on success
            return status

        raise NotmuchError(status)

    def is_match(self):
        """(Not implemented)"""
        return self.get_flag(Message.FLAG.MATCH)

    def tags_to_maildir_flags(self):
        """Synchronize notmuch tags to file Maildir flags

              'D' if the message has the "draft" tag
              'F' if the message has the "flagged" tag
              'P' if the message has the "passed" tag
              'R' if the message has the "replied" tag
              'S' if the message does not have the "unread" tag

        Any existing flags unmentioned in the list above will be
        preserved in the renaming.

        Also, if this filename is in a directory named "new", rename it
        to be within the neighboring directory named "cur".

        Do note that calling this method while a message is frozen might
        not work yet, as the modified tags have not been committed yet
        to the database.

        :returns: a :class:`STATUS` value. In short, you want to see
            notmuch.STATUS.SUCCESS here. See there for details."""
        if not self._msg:
            raise NotInitializedError()
        return Message._tags_to_maildir_flags(self._msg)

    def maildir_flags_to_tags(self):
        """Synchronize file Maildir flags to notmuch tags

            Flag    Action if present
            ----    -----------------
            'D'     Adds the "draft" tag to the message
            'F'     Adds the "flagged" tag to the message
            'P'     Adds the "passed" tag to the message
            'R'     Adds the "replied" tag to the message
            'S'     Removes the "unread" tag from the message

        For each flag that is not present, the opposite action
        (add/remove) is performed for the corresponding tags.  If there
        are multiple filenames associated with this message, the flag is
        considered present if it appears in one or more filenames. (That
        is, the flags from the multiple filenames are combined with the
        logical OR operator.)

        As a convenience, you can set the sync_maildir_flags parameter in
        :meth:`Database.add_message` to implicitly call this.

        :returns: a :class:`STATUS`. In short, you want to see
            notmuch.STATUS.SUCCESS here. See there for details."""
        if not self._msg:
            raise NotInitializedError()
        return Message._maildir_flags_to_tags(self._msg)

    def __repr__(self):
        """Represent a Message() object by str()"""
        return self.__str__()

    def __unicode__(self):
        format = "%s (%s) (%s)"
        return format % (self.get_header('from'),
                         self.get_tags(),
                         date.fromtimestamp(self.get_date()),
                        )

    def get_message_parts(self):
        """Output like notmuch show"""
        fp = open(self.get_filename())
        email_msg = email.message_from_file(fp)
        fp.close()

        out = []
        for msg in email_msg.walk():
            if not msg.is_multipart():
                out.append(msg)
        return out

    def get_part(self, num):
        """Returns the nth message body part"""
        parts = self.get_message_parts()
        if (num <= 0 or num > len(parts)):
            return ""
        else:
            out_part = parts[(num - 1)]
            return out_part.get_payload(decode=True)

    def __hash__(self):
        """Implement hash(), so we can use Message() sets"""
        file = self.get_filename()
        if not file:
            return None
        return hash(file)

    def __cmp__(self, other):
        """Implement cmp(), so we can compare Message()s

        2 messages are considered equal if they point to the same
        Message-Id and if they point to the same file names. If 2
        Messages derive from different queries where some files have
        been added or removed, the same messages would not be considered
        equal (as they do not point to the same set of files
        any more)."""
        res = cmp(self.get_message_id(), other.get_message_id())
        if res:
            res = cmp(list(self.get_filenames()), list(other.get_filenames()))
        return res

    _destroy = nmlib.notmuch_message_destroy
    _destroy.argtypes = [NotmuchMessageP]
    _destroy.restype = None

    def __del__(self):
        """Close and free the notmuch Message"""
        if self._msg:
            self._destroy(self._msg)
