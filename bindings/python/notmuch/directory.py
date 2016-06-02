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

from ctypes import c_uint, c_long
from .globals import (
    nmlib,
    NotmuchDirectoryP,
    NotmuchFilenamesP
)
from .errors import (
    STATUS,
    NotmuchError,
    NotInitializedError,
)
from .filenames import Filenames

class Directory(object):
    """Represents a directory entry in the notmuch directory

    Modifying attributes of this object will modify the
    database, not the real directory attributes.

    The Directory object is usually derived from another object
    e.g. via :meth:`Database.get_directory`, and will automatically be
    become invalid whenever that parent is deleted. You should
    therefore initialized this object handing it a reference to the
    parent, preventing the parent from automatically being garbage
    collected.
    """

    """notmuch_directory_get_mtime"""
    _get_mtime = nmlib.notmuch_directory_get_mtime
    _get_mtime.argtypes = [NotmuchDirectoryP]
    _get_mtime.restype = c_long

    """notmuch_directory_set_mtime"""
    _set_mtime = nmlib.notmuch_directory_set_mtime
    _set_mtime.argtypes = [NotmuchDirectoryP, c_long]
    _set_mtime.restype = c_uint

    """notmuch_directory_get_child_files"""
    _get_child_files = nmlib.notmuch_directory_get_child_files
    _get_child_files.argtypes = [NotmuchDirectoryP]
    _get_child_files.restype = NotmuchFilenamesP

    """notmuch_directory_get_child_directories"""
    _get_child_directories = nmlib.notmuch_directory_get_child_directories
    _get_child_directories.argtypes = [NotmuchDirectoryP]
    _get_child_directories.restype = NotmuchFilenamesP

    def _assert_dir_is_initialized(self):
        """Raises a NotmuchError(:attr:`STATUS`.NOT_INITIALIZED)
        if dir_p is None"""
        if not self._dir_p:
            raise NotInitializedError()

    def __init__(self, path, dir_p, parent):
        """
        :param path:   The absolute path of the directory object.
        :param dir_p:  The pointer to an internal notmuch_directory_t object.
        :param parent: The object this Directory is derived from
                       (usually a :class:`Database`). We do not directly use
                       this, but store a reference to it as long as
                       this Directory object lives. This keeps the
                       parent object alive.
        """
        self._path = path
        self._dir_p = dir_p
        self._parent = parent

    def set_mtime(self, mtime):
        """Sets the mtime value of this directory in the database

        The intention is for the caller to use the mtime to allow efficient
        identification of new messages to be added to the database. The
        recommended usage is as follows:

        * Read the mtime of a directory from the filesystem

        * Call :meth:`Database.add_message` for all mail files in
          the directory

        * Call notmuch_directory_set_mtime with the mtime read from the
          filesystem.  Then, when wanting to check for updates to the
          directory in the future, the client can call :meth:`get_mtime`
          and know that it only needs to add files if the mtime of the
          directory and files are newer than the stored timestamp.

          .. note::

                :meth:`get_mtime` function does not allow the caller to
                distinguish a timestamp of 0 from a non-existent timestamp. So
                don't store a timestamp of 0 unless you are comfortable with
                that.

        :param mtime: A (time_t) timestamp
        :raises: :exc:`XapianError` a Xapian exception occurred, mtime
                 not stored
        :raises: :exc:`ReadOnlyDatabaseError` the database was opened
                 in read-only mode so directory mtime cannot be modified
        :raises: :exc:`NotInitializedError` the directory object has not
                 been initialized
        """
        self._assert_dir_is_initialized()
        status = Directory._set_mtime(self._dir_p, mtime)

        if status != STATUS.SUCCESS:
            raise NotmuchError(status)

    def get_mtime(self):
        """Gets the mtime value of this directory in the database

        Retrieves a previously stored mtime for this directory.

        :param mtime: A (time_t) timestamp
        :raises: :exc:`NotmuchError`:

                        :attr:`STATUS`.NOT_INITIALIZED
                          The directory has not been initialized
        """
        self._assert_dir_is_initialized()
        return Directory._get_mtime(self._dir_p)

    # Make mtime attribute a property of Directory()
    mtime = property(get_mtime, set_mtime, doc="""Property that allows getting
                     and setting of the Directory *mtime* (read-write)

                     See :meth:`get_mtime` and :meth:`set_mtime` for usage and
                     possible exceptions.""")

    def get_child_files(self):
        """Gets a Filenames iterator listing all the filenames of
        messages in the database within the given directory.

        The returned filenames will be the basename-entries only (not
        complete paths.
        """
        self._assert_dir_is_initialized()
        files_p = Directory._get_child_files(self._dir_p)
        return Filenames(files_p, self)

    def get_child_directories(self):
        """Gets a :class:`Filenames` iterator listing all the filenames of
        sub-directories in the database within the given directory

        The returned filenames will be the basename-entries only (not
        complete paths.
        """
        self._assert_dir_is_initialized()
        files_p = Directory._get_child_directories(self._dir_p)
        return Filenames(files_p, self)

    @property
    def path(self):
        """Returns the absolute path of this Directory (read-only)"""
        return self._path

    def __repr__(self):
        """Object representation"""
        return "<notmuch Directory object '%s'>" % self._path

    _destroy = nmlib.notmuch_directory_destroy
    _destroy.argtypes = [NotmuchDirectoryP]
    _destroy.restype = None

    def __del__(self):
        """Close and free the Directory"""
        if self._dir_p:
            self._destroy(self._dir_p)
