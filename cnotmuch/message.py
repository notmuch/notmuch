#    This file is part of cnotmuch.
#
#    cnotmuch is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    cnotmuch is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with cnotmuch.  If not, see <http://www.gnu.org/licenses/>.
#
#    (C) Copyright 2010 Sebastian Spaeth <Sebastian@SSpaeth.de>
#                       Jesse Rosenthal <jrosenthal@jhu.edu>
        
from ctypes import c_char_p, c_void_p, c_long, c_uint
from datetime import date
from cnotmuch.globals import nmlib, STATUS, NotmuchError, Enum
from cnotmuch.tag import Tags
import sys
import email
import types
try:
    import simplejson as json
except ImportError:
    import json
#------------------------------------------------------------------------------
class Messages(object):
    """Represents a list of notmuch messages

    This object provides an iterator over a list of notmuch messages
    (Technically, it provides a wrapper for the underlying
    *notmuch_messages_t* structure). Do note that the underlying
    library only provides a one-time iterator (it cannot reset the
    iterator to the start). Thus iterating over the function will
    "exhaust" the list of messages, and a subsequent iteration attempt
    will raise a :exc:`NotmuchError` STATUS.NOT_INITIALIZED. Also
    note, that any function that uses iteration will also
    exhaust the messages. So both::

      for msg in msgs: print msg 

    as well as::

       number_of_msgs = len(msgs)

    will "exhaust" the Messages. If you need to re-iterate over a list of
    messages you will need to retrieve a new :class:`Messages` object.

    Things are not as bad as it seems though, you can store and reuse
    the single Message objects as often as you want as long as you
    keep the parent Messages object around. (Recall that due to
    hierarchical memory allocation, all derived Message objects will
    be invalid when we delete the parent Messages() object, even if it
    was already "exhausted".) So this works::

      db   = Database()
      msgs = Query(db,'').search_messages() #get a Messages() object
      msglist = []
      for m in msgs:
         msglist.append(m)

      # msgs is "exhausted" now and even len(msgs) will raise an exception.
      # However it will be kept around until all retrieved Message() objects are
      # also deleted. If you did e.g. an explicit del(msgs) here, the 
      # following lines would fail.
      
      # You can reiterate over *msglist* however as often as you want. 
      # It is simply a list with Message objects.

      print (msglist[0].get_filename())
      print (msglist[1].get_filename())
      print (msglist[0].get_message_id())
    """

    #notmuch_tags_get
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
            NotmuchError(STATUS.NULL_POINTER)

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
        """len(:class:`Messages`) returns the number of contained messages

        .. note:: As this iterates over the messages, we will not be able to 
               iterate over them again! So this will fail::

                 #THIS FAILS
                 msgs = Database().create_query('').search_message()
                 if len(msgs) > 0:              #this 'exhausts' msgs
                     # next line raises NotmuchError(STATUS.NOT_INITIALIZED)!!!
                     for msg in msgs: print msg

               Most of the time, using the
               :meth:`Query.count_messages` is therefore more
               appropriate (and much faster). While not guaranteeing
               that it will return the exact same number than len(),
               in my tests it effectively always did so.
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
            nmlib.notmuch_messages_destroy (self._msgs)

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
            raise Exception

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
                elif format.lower() == "json":
                    sys.stdout.write(msg.format_message_as_json(indent))
                else:
                    raise NotmuchError
                next_indent = indent + 1

            # get replies and print them also out (if there are any)
            replies = msg.get_replies()
            if not replies is None:
                sys.stdout.write(set_sep)
                replies.print_messages(format, next_indent, entire_thread)

            sys.stdout.write(set_end)
        sys.stdout.write(set_end)

#------------------------------------------------------------------------------
class Message(object):
    """Represents a single Email message

    Technically, this wraps the underlying *notmuch_message_t* structure.
    """

    """notmuch_message_get_filename (notmuch_message_t *message)"""
    _get_filename = nmlib.notmuch_message_get_filename
    _get_filename.restype = c_char_p 

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
            NotmuchError(STATUS.NULL_POINTER)
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

        This function will not return None since Notmuch ensures that every
        message belongs to a single thread.

        :returns: String with a thread ID
        :exception: :exc:`NotmuchError` STATUS.NOT_INITIALIZED if the message 
                    is not initialized.
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        return Message._get_thread_id (self._msg);

    def get_replies(self):
        """Gets all direct replies to this message as :class:`Messages` iterator

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

        msgs_p = Message._get_replies(self._msg);

        if msgs_p is None:
            return None

        return Messages(msgs_p,self)

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
        header = Message._get_header (self._msg, header)
        if header == None:
            raise NotmuchError(STATUS.NULL_POINTER)
        return header

    def get_filename(self):
        """Returns the file path of the message file

        :returns: Absolute file path & name of the message file
        :exception: :exc:`NotmuchError` STATUS.NOT_INITIALIZED if the message 
              is not initialized.
        """
        if self._msg is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)
        return Message._get_filename(self._msg)

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

    def add_tag(self, tag):
        """Adds a tag to the given message

        Adds a tag to the current message. The maximal tag length is defined in
        the notmuch library and is currently 200 bytes.

        :param tag: String with a 'tag' to be added.
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

        status = nmlib.notmuch_message_add_tag (self._msg, tag)

        if STATUS.SUCCESS == status:
            # return on success
            return status

        raise NotmuchError(status)

    def remove_tag(self, tag):
        """Removes a tag from the given message

        If the message has no such tag, this is a non-operation and
        will report success anyway.

        :param tag: String with a 'tag' to be removed.
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

        status = nmlib.notmuch_message_remove_tag(self._msg, tag)

        if STATUS.SUCCESS == status:
            # return on success
            return status

        raise NotmuchError(status)

    def remove_all_tags(self):
        """Removes all tags from the given message.

        See :meth:`freeze` for an example showing how to safely
        replace tag values.

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

        if STATUS.SUCCESS == status:
            # return on success
            return status

        raise NotmuchError(status)

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
          msg.remove_all_tags()
          for tag in new_tags:
              msg.add_tag(tag)
          msg.thaw()

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

    def __str__(self):
        """A message() is represented by a 1-line summary"""
        msg = {}
        msg['from'] = self.get_header('from')
        msg['tags'] = str(self.get_tags())
        msg['date'] = date.fromtimestamp(self.get_date())
        replies = self.get_replies()
        msg['replies'] = len(replies) if replies is not None else -1
        return "%(from)s (%(date)s) (%(tags)s) (%(replies)d) replies" % (msg)


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
        parts.sort(key=lambda(p): p["id"])
        for p in parts:
            if not p.has_key("filename"):
                output += "\n\fpart{ "
                output += "ID: %d, Content-type: %s\n" % (p["id"], 
                                                         p["content-type"])
                if p.has_key("content"):
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

    def __del__(self):
        """Close and free the notmuch Message"""
        if self._msg is not None:
            nmlib.notmuch_message_destroy (self._msg)
