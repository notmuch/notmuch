Files and directories
=====================

.. currentmodule:: notmuch

:class:`Filenames` -- An iterator over filenames
------------------------------------------------

.. autoclass:: Filenames

   .. automethod:: Filenames.__len__

:class:`Directoy` -- A directory entry in the database
------------------------------------------------------

.. autoclass:: Directory

   .. automethod:: Directory.get_child_files

   .. automethod:: Directory.get_child_directories

   .. automethod:: Directory.get_mtime

   .. automethod:: Directory.set_mtime

   .. autoattribute:: Directory.mtime

   .. autoattribute:: Directory.path
