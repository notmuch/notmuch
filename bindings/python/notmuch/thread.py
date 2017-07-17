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

from ctypes import c_char_p, c_long, c_int
from .globals import (
    nmlib,
    NotmuchThreadP,
    NotmuchMessagesP,
    NotmuchTagsP,
)
from .errors import (
    NullPointerError,
    NotInitializedError,
)
from .messages import Messages
from .tag import Tags
from datetime import date

class Thread(object):
    """Represents a single message thread."""

    """notmuch_thread_get_thread_id"""
    _get_thread_id = nmlib.notmuch_thread_get_thread_id
    _get_thread_id.argtypes = [NotmuchThreadP]
    _get_thread_id.restype = c_char_p

    """notmuch_thread_get_authors"""
    _get_authors = nmlib.notmuch_thread_get_authors
    _get_authors.argtypes = [NotmuchThreadP]
    _get_authors.restype = c_char_p

    """notmuch_thread_get_subject"""
    _get_subject = nmlib.notmuch_thread_get_subject
    _get_subject.argtypes = [NotmuchThreadP]
    _get_subject.restype = c_char_p

    """notmuch_thread_get_toplevel_messages"""
    _get_toplevel_messages = nmlib.notmuch_thread_get_toplevel_messages
    _get_toplevel_messages.argtypes = [NotmuchThreadP]
    _get_toplevel_messages.restype = NotmuchMessagesP

    _get_newest_date = nmlib.notmuch_thread_get_newest_date
    _get_newest_date.argtypes = [NotmuchThreadP]
    _get_newest_date.restype = c_long

    _get_oldest_date = nmlib.notmuch_thread_get_oldest_date
    _get_oldest_date.argtypes = [NotmuchThreadP]
    _get_oldest_date.restype = c_long

    """notmuch_thread_get_tags"""
    _get_tags = nmlib.notmuch_thread_get_tags
    _get_tags.argtypes = [NotmuchThreadP]
    _get_tags.restype = NotmuchTagsP

    def __init__(self, thread_p, parent=None):
        """
        :param thread_p: A pointer to an internal notmuch_thread_t
            Structure.  These are not publically exposed, so a user
            will almost never instantiate a :class:`Thread` object
            herself. They are usually handed back as a result,
            e.g. when iterating through :class:`Threads`. *thread_p*
            must be valid, we will raise an :exc:`NullPointerError`
            if it is `None`.

        :param parent: A 'parent' object is passed which this message is
              derived from. We save a reference to it, so we can
              automatically delete the parent object once all derived
              objects are dead.
        """
        if not thread_p:
            raise NullPointerError()
        self._thread = thread_p
        #keep reference to parent, so we keep it alive
        self._parent = parent

    def get_thread_id(self):
        """Get the thread ID of 'thread'

        The returned string belongs to 'thread' and will only be valid
        for as long as the thread is valid.

        :returns: String with a message ID
        :raises: :exc:`NotInitializedError` if the thread
                    is not initialized.
        """
        if not self._thread:
            raise NotInitializedError()
        return Thread._get_thread_id(self._thread).decode('utf-8', 'ignore')

    _get_total_messages = nmlib.notmuch_thread_get_total_messages
    _get_total_messages.argtypes = [NotmuchThreadP]
    _get_total_messages.restype = c_int

    def get_total_messages(self):
        """Get the total number of messages in 'thread'

        :returns: The number of all messages in the database
                  belonging to this thread. Contrast with
                  :meth:`get_matched_messages`.
        :raises: :exc:`NotInitializedError` if the thread
                    is not initialized.
        """
        if not self._thread:
            raise NotInitializedError()
        return self._get_total_messages(self._thread)

    def get_toplevel_messages(self):
        """Returns a :class:`Messages` iterator for the top-level messages in
           'thread'

           This iterator will not necessarily iterate over all of the messages
           in the thread. It will only iterate over the messages in the thread
           which are not replies to other messages in the thread.

        :returns: :class:`Messages`
        :raises: :exc:`NotInitializedError` if query is not initialized
        :raises: :exc:`NullPointerError` if search_messages failed
        """
        if not self._thread:
            raise NotInitializedError()

        msgs_p = Thread._get_toplevel_messages(self._thread)

        if not msgs_p:
            raise NullPointerError()

        return Messages(msgs_p, self)

    """notmuch_thread_get_messages"""
    _get_messages = nmlib.notmuch_thread_get_messages
    _get_messages.argtypes = [NotmuchThreadP]
    _get_messages.restype = NotmuchMessagesP

    def get_messages(self):
        """Returns a :class:`Messages` iterator for all messages in 'thread'

        :returns: :class:`Messages`
        :raises: :exc:`NotInitializedError` if query is not initialized
        :raises: :exc:`NullPointerError` if get_messages failed
        """
        if not self._thread:
            raise NotInitializedError()

        msgs_p = Thread._get_messages(self._thread)

        if not msgs_p:
            raise NullPointerError()

        return Messages(msgs_p, self)

    _get_matched_messages = nmlib.notmuch_thread_get_matched_messages
    _get_matched_messages.argtypes = [NotmuchThreadP]
    _get_matched_messages.restype = c_int

    def get_matched_messages(self):
        """Returns the number of messages in 'thread' that matched the query

        :returns: The number of all messages belonging to this thread that
                  matched the :class:`Query`from which this thread was created.
                  Contrast with :meth:`get_total_messages`.
        :raises: :exc:`NotInitializedError` if the thread
                    is not initialized.
        """
        if not self._thread:
            raise NotInitializedError()
        return self._get_matched_messages(self._thread)

    def get_authors(self):
        """Returns the authors of 'thread'

        The returned string is a comma-separated list of the names of the
        authors of mail messages in the query results that belong to this
        thread.

        The returned string belongs to 'thread' and will only be valid for
        as long as this Thread() is not deleted.
        """
        if not self._thread:
            raise NotInitializedError()
        authors = Thread._get_authors(self._thread)
        if not authors:
            return None
        return authors.decode('UTF-8', 'ignore')

    def get_subject(self):
        """Returns the Subject of 'thread'

        The returned string belongs to 'thread' and will only be valid for
        as long as this Thread() is not deleted.
        """
        if not self._thread:
            raise NotInitializedError()
        subject = Thread._get_subject(self._thread)
        if not subject:
            return None
        return subject.decode('UTF-8', 'ignore')

    def get_newest_date(self):
        """Returns time_t of the newest message date

        :returns: A time_t timestamp.
        :rtype: c_unit64
        :raises: :exc:`NotInitializedError` if the message
                    is not initialized.
        """
        if not self._thread:
            raise NotInitializedError()
        return Thread._get_newest_date(self._thread)

    def get_oldest_date(self):
        """Returns time_t of the oldest message date

        :returns: A time_t timestamp.
        :rtype: c_unit64
        :raises: :exc:`NotInitializedError` if the message
                    is not initialized.
        """
        if not self._thread:
            raise NotInitializedError()
        return Thread._get_oldest_date(self._thread)

    def get_tags(self):
        """ Returns the message tags

        In the Notmuch database, tags are stored on individual
        messages, not on threads. So the tags returned here will be all
        tags of the messages which matched the search and which belong to
        this thread.

        The :class:`Tags` object is owned by the thread and as such, will only
        be valid for as long as this :class:`Thread` is valid (e.g. until the
        query from which it derived is explicitly deleted).

        :returns: A :class:`Tags` iterator.
        :raises: :exc:`NotInitializedError` if query is not initialized
        :raises: :exc:`NullPointerError` if search_messages failed
        """
        if not self._thread:
            raise NotInitializedError()

        tags_p = Thread._get_tags(self._thread)
        if not tags_p:
            raise NullPointerError()
        return Tags(tags_p, self)

    def __unicode__(self):
        frm = "thread:%s %12s [%d/%d] %s; %s (%s)"

        return frm % (self.get_thread_id(),
                      date.fromtimestamp(self.get_newest_date()),
                      self.get_matched_messages(),
                      self.get_total_messages(),
                      self.get_authors(),
                      self.get_subject(),
                      self.get_tags(),
                     )

    _destroy = nmlib.notmuch_thread_destroy
    _destroy.argtypes = [NotmuchThreadP]
    _destroy.restype = None

    def __del__(self):
        """Close and free the notmuch Thread"""
        if self._thread:
            self._destroy(self._thread)
