.. notmuch documentation master file, created by
   sphinx-quickstart on Tue Feb  2 10:00:47 2010.

.. currentmodule:: notmuch

Welcome to :mod:`notmuch`'s documentation
===========================================

The :mod:`notmuch` module provides an interface to the `notmuch <http://notmuchmail.org>`_ functionality, directly interfacing to a shared notmuch library.
Within :mod:`notmuch`, the classes :class:`Database`, :class:`Query` provide most of the core functionality, returning :class:`Threads`, :class:`Messages` and :class:`Tags`.

.. moduleauthor:: Sebastian Spaeth <Sebastian@SSpaeth.de>

:License: This module is covered under the GNU GPL v3 (or later).

This page contains the main API overview of notmuch |release|. 

Notmuch can be imported as::

 import notmuch

or::

 from notmuch import Query, Database

 db = Database('path',create=True)
 msgs = Query(db,'from:myself').search_messages()

 for msg in msgs:
    print (msg)

More information on specific topics can be found on the following pages:

.. toctree::
   :maxdepth: 1

   status_and_errors
   notmuch   

:mod:`notmuch` -- The Notmuch interface
=================================================

.. automodule:: notmuch

:class:`notmuch.Database` -- The underlying notmuch database
---------------------------------------------------------------------

.. autoclass:: notmuch.Database([path=None[, create=False[, mode=MODE.READ_ONLY]]])

   .. automethod:: create

   .. automethod:: open(path, status=MODE.READ_ONLY)

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

   .. autoattribute:: db_p


:class:`notmuch.Query` -- A search query
-------------------------------------------------

.. autoclass:: notmuch.Query

   .. automethod:: create

   .. attribute:: Query.SORT

     Defines constants that are used as the mode in which to open a database.

     SORT.OLDEST_FIRST
       Sort by message date, oldest first.

     SORT.NEWEST_FIRST
       Sort by message date, newest first.

     SORT.MESSAGE_ID
       Sort by email message ID.

     SORT.UNSORTED
       Do not apply a special sort order (returns results in document id     
       order). 

   .. automethod:: set_sort

   .. attribute::  sort

      Instance attribute :attr:`sort` contains the sort order (see
      :attr:`Query.SORT`) if explicitely specified via
      :meth:`set_sort`. By default it is set to `None`.

   .. automethod:: search_threads

   .. automethod:: search_messages

   .. automethod:: count_messages


:class:`Messages` -- A bunch of messages
----------------------------------------

.. autoclass:: Messages

   .. automethod:: collect_tags

   .. method:: __len__()

   .. warning:: :meth:`__len__` was removed in version 0.6 as it exhausted
       the iterator and broke list(Messages()). Use the
       :meth:`Query.count_messages` function or use
       `len(list(msgs))`.

:class:`Message` -- A single message
----------------------------------------

.. autoclass:: Message

   .. automethod:: get_message_id

   .. automethod:: get_thread_id

   .. automethod:: get_replies

   .. automethod:: get_filename

   .. automethod:: get_filenames

   .. attribute:: FLAG

        FLAG.MATCH 
          This flag is automatically set by a
	  Query.search_threads on those messages that match the
	  query. This allows us to distinguish matches from the rest
	  of the messages in that thread.

   .. automethod:: get_flag

   .. automethod:: set_flag
   
   .. automethod:: get_date

   .. automethod:: get_header

   .. automethod:: get_tags

   .. automethod:: maildir_flags_to_tags

   .. automethod:: tags_to_maildir_flags

   .. automethod:: remove_tag

   .. automethod:: add_tag

   .. automethod:: remove_all_tags

   .. automethod:: freeze

   .. automethod:: thaw

   .. automethod:: format_message_as_json

   .. automethod:: format_message_as_text

   .. automethod:: __str__


:class:`Tags` -- Notmuch tags
-----------------------------

.. autoclass:: Tags
   :members:

   .. method:: __len__

       .. warning:: :meth:`__len__` was removed in version 0.6 as it
           exhausted the iterator and broke list(Tags()). Use
           :meth:`len(list(msgs))` instead if you need to know the
           number of tags.

   .. automethod:: __str__


:class:`notmuch.Threads` -- Threads iterator
-----------------------------------------------------

.. autoclass:: notmuch.Threads

   .. automethod:: __len__

   .. automethod:: __str__

:class:`Thread` -- A single thread
------------------------------------

.. autoclass:: Thread

  .. automethod:: get_thread_id

  .. automethod:: get_total_messages

  .. automethod:: get_toplevel_messages

  .. automethod:: get_matched_messages

  .. automethod:: get_authors

  .. automethod:: get_subject

  .. automethod:: get_oldest_date

  .. automethod:: get_newest_date

  .. automethod:: get_tags

  .. automethod:: __str__


:class:`Filenames` -- An iterator over filenames
------------------------------------------------

.. autoclass:: notmuch.database.Filenames

   .. automethod:: notmuch.database.Filenames.__len__

:class:`notmuch.database.Directoy` -- A directory entry in the database
------------------------------------------------------------------------

.. autoclass:: notmuch.database.Directory

   .. automethod:: notmuch.database.Directory.get_child_files

   .. automethod:: notmuch.database.Directory.get_child_directories

   .. automethod:: notmuch.database.Directory.get_mtime

   .. automethod:: notmuch.database.Directory.set_mtime

   .. autoattribute:: notmuch.database.Directory.mtime

   .. autoattribute:: notmuch.database.Directory.path


The `next page <status_and_errors.html>`_ contains information on possible Status and Error values.

Indices and tables
==================

* :ref:`genindex`
* :ref:`search`

