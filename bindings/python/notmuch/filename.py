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
"""
from ctypes import c_char_p
from notmuch.globals import nmlib, STATUS, NotmuchError


class Filenames(object):
    """Represents a list of filenames as returned by notmuch

    This object contains the Filenames iterator. The main function is
    as_generator() which will return a generator so we can do a Filenamesth an
    iterator over a list of notmuch filenames. Do note that the underlying
    library only provides a one-time iterator (it cannot reset the iterator to
    the start). Thus iterating over the function will "exhaust" the list of
    tags, and a subsequent iteration attempt will raise a :exc:`NotmuchError`
    STATUS.NOT_INITIALIZED. Also note, that any function that uses iteration
    (nearly all) will also exhaust the tags. So both::

      for name in filenames: print name

    as well as::

       number_of_names = len(names)

    and even a simple::

       #str() iterates over all tags to construct a space separated list
       print(str(filenames))

    will "exhaust" the Filenames. However, you can use
    :meth:`Message.get_filenames` repeatedly to get fresh Filenames
    objects to perform various actions on filenames.
    """

    #notmuch_filenames_get
    _get = nmlib.notmuch_filenames_get
    _get.restype = c_char_p

    def __init__(self, files_p, parent):
        """
        :param files_p: A pointer to an underlying *notmuch_tags_t*
             structure. These are not publically exposed, so a user
             will almost never instantiate a :class:`Tags` object
             herself. They are usually handed back as a result,
             e.g. in :meth:`Database.get_all_tags`.  *tags_p* must be
             valid, we will raise an :exc:`NotmuchError`
             (STATUS.NULL_POINTER) if it is `None`.
        :type files_p: :class:`ctypes.c_void_p`
        :param parent: The parent object (ie :class:`Message` these
             filenames are derived from, and saves a
             reference to it, so we can automatically delete the db object
             once all derived objects are dead.
        """
        if files_p is None:
            raise NotmuchError(STATUS.NULL_POINTER)

        self._files = files_p
        #save reference to parent object so we keep it alive
        self._parent = parent

    def as_generator(self):
        """Return generator of Filenames

        This is the main function that will usually be used by the
        user."""
        if self._files is None:
            raise NotmuchError(STATUS.NOT_INITIALIZED)

        while nmlib.notmuch_filenames_valid(self._files):
            yield Filenames._get(self._files)
            nmlib.notmuch_filenames_move_to_next(self._files)

        self._files = None

    def __str__(self):
        """Represent Filenames() as newline-separated list of full paths

        .. note:: As this iterates over the filenames, we will not be
               able to iterate over them again (as in retrieve them)! If
               the tags have been exhausted already, this will raise a
               :exc:`NotmuchError` STATUS.NOT_INITIALIZED on subsequent
               attempts. However, you can use
               :meth:`Message.get_filenames` repeatedly to perform
               various actions on filenames.
        """
        return "\n".join(self)

    def __del__(self):
        """Close and free the notmuch filenames"""
        if self._files is not None:
            nmlib.notmuch_filenames_destroy(self._files)
