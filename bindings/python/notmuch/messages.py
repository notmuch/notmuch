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

from .globals import (
    nmlib,
    NotmuchTagsP,
    NotmuchMessageP,
    NotmuchMessagesP,
)
from .errors import (
    NullPointerError,
    NotInitializedError,
)
from .tag import Tags
from .message import Message

class Messages(object):
    """Represents a list of notmuch messages

    This object provides an iterator over a list of notmuch messages
    (Technically, it provides a wrapper for the underlying
    *notmuch_messages_t* structure). Do note that the underlying library
    only provides a one-time iterator (it cannot reset the iterator to
    the start). Thus iterating over the function will "exhaust" the list
    of messages, and a subsequent iteration attempt will raise a
    :exc:`NotInitializedError`. If you need to
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
    _get.argtypes = [NotmuchMessagesP]
    _get.restype = NotmuchMessageP

    _collect_tags = nmlib.notmuch_messages_collect_tags
    _collect_tags.argtypes = [NotmuchMessagesP]
    _collect_tags.restype = NotmuchTagsP

    def __init__(self, msgs_p, parent=None):
        """
        :param msgs_p:  A pointer to an underlying *notmuch_messages_t*
             structure. These are not publically exposed, so a user
             will almost never instantiate a :class:`Messages` object
             herself. They are usually handed back as a result,
             e.g. in :meth:`Query.search_messages`.  *msgs_p* must be
             valid, we will raise an :exc:`NullPointerError` if it is
             `None`.
        :type msgs_p: :class:`ctypes.c_void_p`
        :param parent: The parent object
             (ie :class:`Query`) these tags are derived from. It saves
             a reference to it, so we can automatically delete the db
             object once all derived objects are dead.
        :TODO: Make the iterator work more than once and cache the tags in
               the Python object.(?)
        """
        if not msgs_p:
            raise NullPointerError()

        self._msgs = msgs_p
        #store parent, so we keep them alive as long as self  is alive
        self._parent = parent

    def collect_tags(self):
        """Return the unique :class:`Tags` in the contained messages

        :returns: :class:`Tags`
        :exceptions: :exc:`NotInitializedError` if not init'ed

        .. note::

            :meth:`collect_tags` will iterate over the messages and therefore
            will not allow further iterations.
        """
        if not self._msgs:
            raise NotInitializedError()

        # collect all tags (returns NULL on error)
        tags_p = Messages._collect_tags(self._msgs)
        #reset _msgs as we iterated over it and can do so only once
        self._msgs = None

        if not tags_p:
            raise NullPointerError()
        return Tags(tags_p, self)

    def __iter__(self):
        """ Make Messages an iterator """
        return self

    _valid = nmlib.notmuch_messages_valid
    _valid.argtypes = [NotmuchMessagesP]
    _valid.restype = bool

    _move_to_next = nmlib.notmuch_messages_move_to_next
    _move_to_next.argtypes = [NotmuchMessagesP]
    _move_to_next.restype = None

    def __next__(self):
        if not self._msgs:
            raise NotInitializedError()

        if not self._valid(self._msgs):
            self._msgs = None
            raise StopIteration

        msg = Message(Messages._get(self._msgs), self)
        self._move_to_next(self._msgs)
        return msg
    next = __next__ # python2.x iterator protocol compatibility

    def __nonzero__(self):
        '''
        Implement truth value testing. If __nonzero__ is not
        implemented, the python runtime would fall back to `len(..) >
        0` thus exhausting the iterator.

        :returns: True if the wrapped iterator has at least one more object
                  left.
        '''
        return self._msgs and self._valid(self._msgs)

    _destroy = nmlib.notmuch_messages_destroy
    _destroy.argtypes = [NotmuchMessagesP]
    _destroy.restype = None

    def __del__(self):
        """Close and free the notmuch Messages"""
        if self._msgs:
            self._destroy(self._msgs)

class EmptyMessagesResult(Messages):
    def __init__(self, parent):
        self._msgs = None
        self._parent = parent

    def __next__(self):
        raise StopIteration()
    next = __next__
