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
               Jesse Rosenthal <jrosenthal@jhu.edu>
"""


from ctypes import c_char_p, c_void_p, c_long, c_uint, c_int
from datetime import date
from notmuch.globals import nmlib, STATUS, NotmuchError, Enum, _str
from notmuch.tag import Tags
from notmuch.filename import Filenames
import sys
import email
import types
try:
    import simplejson as json
except ImportError:
    import json


class Messages(object):
    """Represents a list of notmuch messages

    This object provides an iterator over a list of notmuch messages
    (Technically, it provides a wrapper for the underlying
    *notmuch_messages_t* structure). Do note that the underlying library
    only provides a one-time iterator (it cannot reset the iterator to
    the start). Thus iterating over the function will "exhaust" the list
    of messages, and a subsequent iteration attempt will raise a
    :exc:`NotmuchError` STATUS.NOT_INITIALIZED. If you need to
    re-iterate over a list of messages you will need to retrieve a new
    :class:`Messages` object or cache your :class:`Message`\s in a list
    via::

       msglist = list(msgs)

    You can store and reuse the single :class:`Message` objects as often
    as you want as long as you keep the parent :class:`Messages` object
    around. (Due to hierarchical memory allocation, all derived
    :class:`Message` objects will be invalid when we delete the parent
    :class:`Messages` object, even if it was already exhausted.) So
    this works::

      db   = Database()
      msgs = Query(db,'').search_messages() #get a Messages() object
      msglist = list(msgs)

      # msgs is "exhausted" now and msgs.next() will raise an exception.
      # However it will be kept alive until all retrieved Message()
      # objects are also deleted. If you do e.g. an explicit del(msgs)
      # here, the following lines would fail.

      # You can reiterate over *msglist* however as often as you want.
      # It is simply a list with :class:`Message`s.

      print (msglist[0].get_filename())
      print (msglist[1].get_filename())
      print (msglist[0].get_message_id())


    As :class:`Message` implements both __hash__() and __cmp__(), it is
    possible to make sets out of :class:`Messages` and use set
    arithmetic (this happens in python and will of course be *much*
    slower than redoing a proper query with the appropriate filters::

        s1, s2 = set(msgs1), set(msgs2)
        s.union(s2)
        s1 -= s2
        ...

    Be careful when using set arithmetic between message sets derived
    from different Databases (ie the same database reopened after
    messages have changed). If messages have added or removed associated
    files in the meantime, it is possible that the same message would be
    considered as a different object (as it points to a different file).
    """

    #notmuch_messages_get
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
            raise NotmuchError(STATUS.NULL_POINTER)

        self._msgs = msgs_p
        #store parent, so we keep them alive as long as self  is alive
        self._parent = parent

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
        tags_p = Messages._collect_tags(self._msgs)
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

        msg = Message(Messages._get(self._msgs), self)
        nmlib.notmuch_messages_move_to_next(self._msgs)
        return msg

    def __nonzero__(self):
        """
        :return: True if there is at least one more thread in the
            Iterator, False if not."""
        return self._msgs is not None and \
            nmlib.notmuch_messages_valid(self._msgs) > 0

    def __del__(self):
        """Close and free the notmuch Messages"""
        if self._msgs is not None:
            nmlib.notmuch_messages_destroy(self._msgs)

    def print_messages(self, format, indent=0, entire_thread=False):
        """Outputs messages as needed for 'notmuch show' to sys.stdout

        :param format: A string of either 'text' or 'json'.
        :param indent: A number indicating the reply depth of these messages.
        :param entire_thread: A bool, indicating whether we want to output
                       whole threads or only the matching messages.
        """
        if format.lower() == "text":
            set_start = ""
            set_end = ""
            set_sep = ""
        elif format.lower() == "json":
            set_start = "["
            set_end = "]"
            set_sep = ", "
        else:
            raise TypeError("format must be either 'text' or 'json'")

        first_set = True

        sys.stdout.write(set_start)

        # iterate through all toplevel messages in this thread
        for msg in self:
            # if not msg:
            #     break
            if not first_set:
                sys.stdout.write(set_sep)
            first_set = False

            sys.stdout.write(set_start)
            match = msg.is_match()
            next_indent = indent

            if (match or entire_thread):
                if format.lower() == "text":
                    sys.stdout.write(msg.format_message_as_text(indent))
                else:
                    sys.stdout.write(msg.format_message_as_json(indent))
                next_indent = indent + 1

            # get replies and print them also out (if there are any)
            replies = msg.get_replies()
            if not replies is None:
                sys.stdout.write(set_sep)
                replies.print_messages(format, next_indent, entire_thread)

            sys.stdout.write(set_end)
        sys.stdout.write(set_end)


class Message(object):
    """Represents a single Email message

    Technically, this wraps the underlying *notmuch_message_t*
    structure. A user will usually not create these objects themselves
    but get them as search results.

    As it implements :meth:`__cmp__`, it is possible to compare two
    :class:`Message`\s using `if msg1 == msg2: ...`.
    """

    """notmuch_message_get_filename (notmuch_message_t *message)"""
    _get_filename = nmlib.notmuch_message_get_filename
    _get_filename.restype = c_char_p

    """return all filenames for a message"""
    _get_filenames = nmlib.notmuch_message_get_filenames
    _get_filenames.restype = c_void_p

    """notmuch_message_get_flag"""
    _get_flag = nmlib.notmuch_message_get_flag
    _get_flag.restype = c_uint

    """notmuch_message_get_message_id (notmuch_message_t *message)"""
    _get_message_id = nmlib.notmuch_message_get_message_id
    _get_message_id.restype = c_char_p

    """notmuch_message_get_thread_id"""
    _get_thread_id = nmlib.notmuch_message_get_thread_id
    _get_thread_id.restype = c_char_p

    """notmuch_message_get_replies"""
    _get_replies = nmlib.notmuch_message_get_replies
    _get_replies.restype = c_void_p

    """notmuch_message_get_tags (notmuch_message_t *message)"""
    _get_tags = nmlib.notmuch_message_get_tags
    _get_tags.restype = c_void_p

    _get_date = nmlib.notmuch_message_get_date
    _get_date.restype = c_long

    _get_header = nmlib.notmuch_message_get_header
    _get_header.restype = c_char_p

    """notmuch_status_t ..._maildir_flags_to_tags (notmuch_message_t *)"""
    _tags_to_maildir_flags = nmlib.notmuch_message_tags_to_maildir_flags
    _tags_to_maildir_flags.restype = c_int

    """notmuch_status_t ..._tags_to_maildir_flags (notmuch_message_t *)"""
    _maildir_flags_to_tags = nmlib.notmuch_message_maildir_flags_to_tags
    _maildir_flags_to_tags.restype = c_int

    #Constants: Flags that can be set/get with set_flag
    FLAG = Enum(['MATCH'])

    def __init__(self, msg_p, parent=None):
        """
        :param msg_p: A pointer to an internal notmuch_message_t
            Structure.  If it is `None`, we will raise an :exc:`NotmuchError`
            STATUS.NULL_POINTER.

        :param parent: A 'parent' object is passed which this message is
              derived from. We save a reference to it, so we can
              automatically delete the parent object once all derived
              objects are dead.
        """
        if msg_p is None:
            raise NotmuchError(STATUS.NULL_POINTER)
        self._msg = msg_p
        #keep reference to parent, so we keep it alive
        self._parent = parent

    def get_message_id(self):
        """Returns the message ID

        :returns: String with a message ID
        :exception: :exc:`NotmuchError` STATUS.NOT_INITIALIZED if the message
                    is not initialized.
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        return Message._get_message_id(self._msg)

    def get_thread_id(self):
        """Returns the thread ID

        The returned string belongs to 'message' will only be valid for as
        long as the message is valid.

        This function will not return `None` since Notmuch ensures that every
        message belongs to a single thread.

        :returns: String with a thread ID
        :exception: :exc:`NotmuchError` STATUS.NOT_INITIALIZED if the message
                    is not initialized.
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        return Message._get_thread_id(self._msg)

    def get_replies(self):
        """Gets all direct replies to this message as :class:`Messages`
        iterator

        .. note:: This call only makes sense if 'message' was
          ultimately obtained from a :class:`Thread` object, (such as
          by coming directly from the result of calling
          :meth:`Thread.get_toplevel_messages` or by any number of
          subsequent calls to :meth:`get_replies`). If this message was
          obtained through some non-thread means, (such as by a call
          to :meth:`Query.search_messages`), then this function will
          return `None`.

        :returns: :class:`Messages` or `None` if there are no replies to
            this message.
        :exception: :exc:`NotmuchError` STATUS.NOT_INITIALIZED if the message
                    is not initialized.
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        msgs_p = Message._get_replies(self._msg)

        if msgs_p is None:
            return None

        return Messages(msgs_p, self)

    def get_date(self):
        """Returns time_t of the message date

        For the original textual representation of the Date header from the
        message call notmuch_message_get_header() with a header value of
        "date".

        :returns: A time_t timestamp.
        :rtype: c_unit64
        :exception: :exc:`NotmuchError` STATUS.NOT_INITIALIZED if the message
                    is not initialized.
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        return Message._get_date(self._msg)

    def get_header(self, header):
        """Returns a message header

        This returns any message header that is stored in the notmuch database.
        This is only a selected subset of headers, which is currently:

          TODO: add stored headers

        :param header: The name of the header to be retrieved.
                       It is not case-sensitive (TODO: confirm).
        :type header: str
        :returns: The header value as string
        :exception: :exc:`NotmuchError`

                    * STATUS.NOT_INITIALIZED if the message
                      is not initialized.
                    * STATUS.NULL_POINTER, if no header was found
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        #Returns NULL if any error occurs.
        header = Message._get_header(self._msg, header)
        if header == None:
            raise NotmuchError(STATUS.NULL_POINTER)
        return header.decode('UTF-8')

    def get_filename(self):
        """Returns the file path of the message file

        :returns: Absolute file path & name of the message file
        :exception: :exc:`NotmuchError` STATUS.NOT_INITIALIZED if the message
              is not initialized.
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        return Message._get_filename(self._msg)

    def get_filenames(self):
        """Get all filenames for the email corresponding to 'message'

        Returns a Filenames() generator with all absolute filepaths for
        messages recorded to have the same Message-ID. These files must
        not necessarily have identical content."""
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        files_p = Message._get_filenames(self._msg)

        return Filenames(files_p, self).as_generator()

    def get_flag(self, flag):
        """Checks whether a specific flag is set for this message

        The method :meth:`Query.search_threads` sets
        *Message.FLAG.MATCH* for those messages that match the
        query. This method allows us to get the value of this flag.

        :param flag: One of the :attr:`Message.FLAG` values (currently only
                     *Message.FLAG.MATCH*
        :returns: An unsigned int (0/1), indicating whether the flag is set.
        :exception: :exc:`NotmuchError` STATUS.NOT_INITIALIZED if the message
              is not initialized.
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        return Message._get_flag(self._msg, flag)

    def set_flag(self, flag, value):
        """Sets/Unsets a specific flag for this message

        :param flag: One of the :attr:`Message.FLAG` values (currently only
                     *Message.FLAG.MATCH*
        :param value: A bool indicating whether to set or unset the flag.

        :returns: Nothing
        :exception: :exc:`NotmuchError` STATUS.NOT_INITIALIZED if the message
              is not initialized.
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        nmlib.notmuch_message_set_flag(self._msg, flag, value)

    def get_tags(self):
        """Returns the message tags

        :returns: A :class:`Tags` iterator.
        :exception: :exc:`NotmuchError`

                      * STATUS.NOT_INITIALIZED if the message
                        is not initialized.
                      * STATUS.NULL_POINTER, on error
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        tags_p = Message._get_tags(self._msg)
        if tags_p == None:
            raise NotmuchError(STATUS.NULL_POINTER)
        return Tags(tags_p, self)

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
        :exception: :exc:`NotmuchError`. They have the following meaning:

                  STATUS.NULL_POINTER
                    The 'tag' argument is NULL
                  STATUS.TAG_TOO_LONG
                    The length of 'tag' is too long
                    (exceeds Message.NOTMUCH_TAG_MAX)
                  STATUS.READ_ONLY_DATABASE
                    Database was opened in read-only mode so message cannot be
                    modified.
                  STATUS.NOT_INITIALIZED
                     The message has not been initialized.
       """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        status = nmlib.notmuch_message_add_tag(self._msg, _str(tag))

        # bail out on failure
        if status != STATUS.SUCCESS:
            raise NotmuchError(status)

        if sync_maildir_flags:
            self.tags_to_maildir_flags()
        return STATUS.SUCCESS

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
        :exception: :exc:`NotmuchError`. They have the following meaning:

                   STATUS.NULL_POINTER
                     The 'tag' argument is NULL
                   STATUS.TAG_TOO_LONG
                     The length of 'tag' is too long
                     (exceeds NOTMUCH_TAG_MAX)
                   STATUS.READ_ONLY_DATABASE
                     Database was opened in read-only mode so message cannot
                     be modified.
                   STATUS.NOT_INITIALIZED
                     The message has not been initialized.
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        status = nmlib.notmuch_message_remove_tag(self._msg, _str(tag))
        # bail out on error
        if status != STATUS.SUCCESS:
            raise NotmuchError(status)

        if sync_maildir_flags:
            self.tags_to_maildir_flags()
        return STATUS.SUCCESS

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
        :exception: :exc:`NotmuchError`. They have the following meaning:

                   STATUS.READ_ONLY_DATABASE
                     Database was opened in read-only mode so message cannot
                     be modified.
                   STATUS.NOT_INITIALIZED
                     The message has not been initialized.
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        status = nmlib.notmuch_message_remove_all_tags(self._msg)

        # bail out on error
        if status != STATUS.SUCCESS:
            raise NotmuchError(status)

        if sync_maildir_flags:
            self.tags_to_maildir_flags()
        return STATUS.SUCCESS

    def freeze(self):
        """Freezes the current state of 'message' within the database

        This means that changes to the message state, (via :meth:`add_tag`,
        :meth:`remove_tag`, and :meth:`remove_all_tags`), will not be
        committed to the database until the message is :meth:`thaw`ed.

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
        :exception: :exc:`NotmuchError`. They have the following meaning:

                   STATUS.READ_ONLY_DATABASE
                     Database was opened in read-only mode so message cannot
                     be modified.
                   STATUS.NOT_INITIALIZED
                     The message has not been initialized.
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        status = nmlib.notmuch_message_freeze(self._msg)

        if STATUS.SUCCESS == status:
            # return on success
            return status

        raise NotmuchError(status)

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
        :exception: :exc:`NotmuchError`. They have the following meaning:

                   STATUS.UNBALANCED_FREEZE_THAW
                     An attempt was made to thaw an unfrozen message.
                     That is, there have been an unbalanced number of calls
                     to :meth:`freeze` and :meth:`thaw`.
                   STATUS.NOT_INITIALIZED
                     The message has not been initialized.
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        status = nmlib.notmuch_message_thaw(self._msg)

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

        :returns: a :class:`STATUS`. In short, you want to see
            notmuch.STATUS.SUCCESS here. See there for details."""
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        status = Message._tags_to_maildir_flags(self._msg)

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
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        status = Message._tags_to_maildir_flags(self._msg)

    def __repr__(self):
        """Represent a Message() object by str()"""
        return self.__str__()

    def __str__(self):
        """A message() is represented by a 1-line summary"""
        msg = {}
        msg['from'] = self.get_header('from')
        msg['tags'] = self.get_tags()
        msg['date'] = date.fromtimestamp(self.get_date())
        return "%(from)s (%(date)s) (%(tags)s)" % (msg)

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

    def format_message_internal(self):
        """Create an internal representation of the message parts,
        which can easily be output to json, text, or another output
        format. The argument match tells whether this matched a
        query."""
        output = {}
        output["id"] = self.get_message_id()
        output["match"] = self.is_match()
        output["filename"] = self.get_filename()
        output["tags"] = list(self.get_tags())

        headers = {}
        for h in ["Subject", "From", "To", "Cc", "Bcc", "Date"]:
            headers[h] = self.get_header(h)
        output["headers"] = headers

        body = []
        parts = self.get_message_parts()
        for i in xrange(len(parts)):
            msg = parts[i]
            part_dict = {}
            part_dict["id"] = i + 1
            # We'll be using this is a lot, so let's just get it once.
            cont_type = msg.get_content_type()
            part_dict["content-type"] = cont_type
            # NOTE:
            # Now we emulate the current behaviour, where it ignores
            # the html if there's a text representation.
            #
            # This is being worked on, but it will be easier to fix
            # here in the future than to end up with another
            # incompatible solution.
            disposition = msg["Content-Disposition"]
            if disposition and disposition.lower().startswith("attachment"):
                part_dict["filename"] = msg.get_filename()
            else:
                if cont_type.lower() == "text/plain":
                    part_dict["content"] = msg.get_payload()
                elif (cont_type.lower() == "text/html" and
                      i == 0):
                    part_dict["content"] = msg.get_payload()
            body.append(part_dict)

        output["body"] = body

        return output

    def format_message_as_json(self, indent=0):
        """Outputs the message as json. This is essentially the same
        as python's dict format, but we run it through, just so we
        don't have to worry about the details."""
        return json.dumps(self.format_message_internal())

    def format_message_as_text(self, indent=0):
        """Outputs it in the old-fashioned notmuch text form. Will be
        easy to change to a new format when the format changes."""

        format = self.format_message_internal()
        output = "\fmessage{ id:%s depth:%d match:%d filename:%s" \
                 % (format['id'], indent, format['match'], format['filename'])
        output += "\n\fheader{"

        #Todo: this date is supposed to be prettified, as in the index.
        output += "\n%s (%s) (" % (format["headers"]["From"],
                                   format["headers"]["Date"])
        output += ", ".join(format["tags"])
        output += ")"

        output += "\nSubject: %s" % format["headers"]["Subject"]
        output += "\nFrom: %s" % format["headers"]["From"]
        output += "\nTo: %s" % format["headers"]["To"]
        if format["headers"]["Cc"]:
            output += "\nCc: %s" % format["headers"]["Cc"]
        if format["headers"]["Bcc"]:
            output += "\nBcc: %s" % format["headers"]["Bcc"]
        output += "\nDate: %s" % format["headers"]["Date"]
        output += "\n\fheader}"

        output += "\n\fbody{"

        parts = format["body"]
        parts.sort(key=lambda x: x['id'])
        for p in parts:
            if not "filename" in p:
                output += "\n\fpart{ "
                output += "ID: %d, Content-type: %s\n" % (p["id"],
                                                          p["content-type"])
                if "content" in p:
                    output += "\n%s\n" % p["content"]
                else:
                    output += "Non-text part: %s\n" % p["content-type"]
                    output += "\n\fpart}"
            else:
                output += "\n\fattachment{ "
                output += "ID: %d, Content-type:%s\n" % (p["id"],
                                                         p["content-type"])
                output += "Attachment: %s\n" % p["filename"]
                output += "\n\fattachment}\n"

        output += "\n\fbody}\n"
        output += "\n\fmessage}"

        return output

    def __hash__(self):
        """Implement hash(), so we can use Message() sets"""
        file = self.get_filename()
        if file is None:
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

    def __del__(self):
        """Close and free the notmuch Message"""
        if self._msg is not None:
            nmlib.notmuch_message_destroy(self._msg)
