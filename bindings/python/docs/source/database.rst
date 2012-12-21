:class:`Database` -- The underlying notmuch database
====================================================

.. currentmodule:: notmuch

.. autoclass:: Database([path=None[, create=False[, mode=MODE.READ_ONLY]]])

   .. automethod:: create

   .. automethod:: open(path, status=MODE.READ_ONLY)

   .. automethod:: close

   .. automethod:: get_path

   .. automethod:: get_version

   .. automethod:: needs_upgrade

   .. automethod:: upgrade

   .. automethod:: begin_atomic

   .. automethod:: end_atomic

   .. automethod:: get_directory

   .. automethod:: add_message

   .. automethod:: remove_message

   .. automethod:: find_message

   .. automethod:: find_message_by_filename

   .. automethod:: get_all_tags

   .. automethod:: create_query

   .. attribute:: Database.MODE

     Defines constants that are used as the mode in which to open a database.

     MODE.READ_ONLY
       Open the database in read-only mode

     MODE.READ_WRITE
       Open the database in read-write mode
