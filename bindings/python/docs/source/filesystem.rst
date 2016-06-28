Files and directories
=====================

.. currentmodule:: notmuch

:class:`Filenames` -- An iterator over filenames
------------------------------------------------

.. autoclass:: Filenames

   .. method:: Filenames.__len__
   .. warning::
      :meth:`__len__` was removed in version 0.22 as it exhausted the
      iterator and broke list(Filenames()). Use `len(list(names))`
      instead.

:class:`Directoy` -- A directory entry in the database
------------------------------------------------------

.. autoclass:: Directory

   .. automethod:: Directory.get_child_files

   .. automethod:: Directory.get_child_directories

   .. automethod:: Directory.get_mtime

   .. automethod:: Directory.set_mtime

   .. autoattribute:: Directory.mtime

   .. autoattribute:: Directory.path
