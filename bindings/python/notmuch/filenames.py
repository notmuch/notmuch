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
    NotmuchFilenamesP,
    Python3StringMixIn,
)
from .errors import (
    NullPointerError,
    NotInitializedError,
)


class Filenames(Python3StringMixIn):
    """Represents a list of filenames as returned by notmuch

    Objects of this class implement the iterator protocol.

    .. note::

        The underlying library only provides a one-time iterator (it
        cannot reset the iterator to the start). Thus iterating over
        the function will "exhaust" the list of tags, and a subsequent
        iteration attempt will raise a
        :exc:`NotInitializedError`. Also note, that any function that
        uses iteration (nearly all) will also exhaust the tags. So
        both::

           for name in filenames: print name

        as well as::

           list_of_names = list(names)

        and even a simple::

           #str() iterates over all tags to construct a space separated list
           print(str(filenames))

        will "exhaust" the Filenames. However, you can use
        :meth:`Message.get_filenames` repeatedly to get fresh
        Filenames objects to perform various actions on filenames.
    """

    #notmuch_filenames_get
    _get = nmlib.notmuch_filenames_get
    _get.argtypes = [NotmuchFilenamesP]
    _get.restype = c_char_p

    def __init__(self, files_p, parent):
        """
        :param files_p: A pointer to an underlying *notmuch_tags_t*
             structure. These are not publically exposed, so a user
             will almost never instantiate a :class:`Tags` object
             herself. They are usually handed back as a result,
             e.g. in :meth:`Database.get_all_tags`.  *tags_p* must be
             valid, we will raise an :exc:`NullPointerError`
             if it is `None`.
        :type files_p: :class:`ctypes.c_void_p`
        :param parent: The parent object (ie :class:`Message` these
             filenames are derived from, and saves a
             reference to it, so we can automatically delete the db object
             once all derived objects are dead.
        """
        if not files_p:
            raise NullPointerError()

        self._files_p = files_p
        #save reference to parent object so we keep it alive
        self._parent = parent

    def __iter__(self):
        """ Make Filenames an iterator """
        return self

    _valid = nmlib.notmuch_filenames_valid
    _valid.argtypes = [NotmuchFilenamesP]
    _valid.restype = bool

    _move_to_next = nmlib.notmuch_filenames_move_to_next
    _move_to_next.argtypes = [NotmuchFilenamesP]
    _move_to_next.restype = None

    def __next__(self):
        if not self._files_p:
            raise NotInitializedError()

        if not self._valid(self._files_p):
            self._files_p = None
            raise StopIteration

        file_ = Filenames._get(self._files_p)
        self._move_to_next(self._files_p)
        return file_.decode('utf-8', 'ignore')
    next = __next__ # python2.x iterator protocol compatibility

    def __unicode__(self):
        """Represent Filenames() as newline-separated list of full paths

        .. note::

            This method exhausts the iterator object, so you will not be able to
            iterate over them again.
        """
        return "\n".join(self)

    _destroy = nmlib.notmuch_filenames_destroy
    _destroy.argtypes = [NotmuchFilenamesP]
    _destroy.restype = None

    def __del__(self):
        """Close and free the notmuch filenames"""
        if self._files_p:
            self._destroy(self._files_p)
