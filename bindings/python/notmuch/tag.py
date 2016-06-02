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
from ctypes import c_char_p
from .globals import (
    nmlib,
    Python3StringMixIn,
    NotmuchTagsP,
)
from .errors import (
    NullPointerError,
    NotInitializedError,
)


class Tags(Python3StringMixIn):
    """Represents a list of notmuch tags

    This object provides an iterator over a list of notmuch tags (which
    are unicode instances).

    Do note that the underlying library only provides a one-time
    iterator (it cannot reset the iterator to the start). Thus iterating
    over the function will "exhaust" the list of tags, and a subsequent
    iteration attempt will raise a :exc:`NotInitializedError`.
    Also note, that any function that uses iteration (nearly all) will
    also exhaust the tags. So both::

      for tag in tags: print tag

    as well as::

       number_of_tags = len(tags)

    and even a simple::

       #str() iterates over all tags to construct a space separated list
       print(str(tags))

    will "exhaust" the Tags. If you need to re-iterate over a list of
    tags you will need to retrieve a new :class:`Tags` object.
    """

    #notmuch_tags_get
    _get = nmlib.notmuch_tags_get
    _get.argtypes = [NotmuchTagsP]
    _get.restype = c_char_p

    def __init__(self, tags_p, parent=None):
        """
        :param tags_p: A pointer to an underlying *notmuch_tags_t*
             structure. These are not publically exposed, so a user
             will almost never instantiate a :class:`Tags` object
             herself. They are usually handed back as a result,
             e.g. in :meth:`Database.get_all_tags`.  *tags_p* must be
             valid, we will raise an :exc:`NullPointerError` if it is
             `None`.
        :type tags_p: :class:`ctypes.c_void_p`
        :param parent: The parent object (ie :class:`Database` or
             :class:`Message` these tags are derived from, and saves a
             reference to it, so we can automatically delete the db object
             once all derived objects are dead.
        :TODO: Make the iterator optionally work more than once by
               cache the tags in the Python object(?)
        """
        if not tags_p:
            raise NullPointerError()

        self._tags = tags_p
        #save reference to parent object so we keep it alive
        self._parent = parent

    def __iter__(self):
        """ Make Tags an iterator """
        return self

    _valid = nmlib.notmuch_tags_valid
    _valid.argtypes = [NotmuchTagsP]
    _valid.restype = bool

    _move_to_next = nmlib.notmuch_tags_move_to_next
    _move_to_next.argtypes = [NotmuchTagsP]
    _move_to_next.restype = None

    def __next__(self):
        if not self._tags:
            raise NotInitializedError()
        if not self._valid(self._tags):
            self._tags = None
            raise StopIteration
        tag = Tags._get(self._tags).decode('UTF-8')
        self._move_to_next(self._tags)
        return tag
    next = __next__ # python2.x iterator protocol compatibility

    def __nonzero__(self):
        '''
        Implement truth value testing. If __nonzero__ is not
        implemented, the python runtime would fall back to `len(..) >
        0` thus exhausting the iterator.

        :returns: True if the wrapped iterator has at least one more object
                  left.
        '''
        return self._tags and self._valid(self._tags)

    def __unicode__(self):
        """string representation of :class:`Tags`: a space separated list of tags

        .. note::

            As this iterates over the tags, we will not be able to iterate over
            them again (as in retrieve them)! If the tags have been exhausted
            already, this will raise a :exc:`NotInitializedError`on subsequent
            attempts.
        """
        return " ".join(self)

    _destroy = nmlib.notmuch_tags_destroy
    _destroy.argtypes = [NotmuchTagsP]
    _destroy.restype = None

    def __del__(self):
        """Close and free the notmuch tags"""
        if self._tags:
            self._destroy(self._tags)
