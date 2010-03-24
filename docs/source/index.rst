.. cnotmuch documentation master file, created by
   sphinx-quickstart on Tue Feb  2 10:00:47 2010.

.. currentmodule:: cnotmuch

Welcome to :mod:`cnotmuch`'s documentation
===========================================

The :mod:`cnotmuch` module provides an interface to the `notmuch <http://notmuchmail.org>`_ functionality, directly interfacing to a shared notmuch library.
The classes :class:`notmuch.Database`, :class:`notmuch.Query` provide most of the core functionality, returning :class:`notmuch.Threads`, :class:`notmuch.Messages` and :class:`notmuch.Tags`.

.. moduleauthor:: Sebastian Spaeth <Sebastian@SSpaeth.de>

:License: This module is covered under the GNU GPL v3 (or later).

This page contains the main API overview of cnotmuch |release|. 

Notmuch can be imported as::

 from cnotmuch import notmuch

or::

 from cnotmuch.notmuch import Query,Database

More information on specific topics can be found on the following pages:

.. toctree::
   :maxdepth: 1

   notmuch   

:mod:`notmuch` -- The Notmuch interface
=============================================

.. automodule:: cnotmuch.notmuch

:todo: Document nmlib,STATUS

:class:`Database` -- The underlying notmuch database
-----------------------------------------------------

.. autoclass:: Database([path=None[, create=False[, mode=MODE.READ_ONLY]]])

   .. automethod:: create

   .. automethod:: open(path, status=MODE.READ_ONLY)

   .. automethod:: get_path

   .. automethod:: get_version

   .. automethod:: needs_upgrade

   .. automethod:: upgrade

   .. automethod:: get_directory

   .. automethod:: add_message

   .. automethod:: remove_message

   .. automethod:: find_message

   .. automethod:: get_all_tags

   .. automethod:: create_query

   .. note:: :meth:`create_query` was broken in release
             0.1 and is fixed since 0.1.1.

   .. attribute:: Database.MODE

     Defines constants that are used as the mode in which to open a database.

     MODE.READ_ONLY
       Open the database in read-only mode

     MODE.READ_WRITE
       Open the database in read-write mode

   .. autoattribute:: db_p

:class:`Query` -- A search query
-----------------------------------------------

.. autoclass:: Query

   .. automethod:: create

   .. attribute:: Query.SORT

     Defines constants that are used as the mode in which to open a database.

     SORT.OLDEST_FIRST
       Sort by message date, oldest first.

     SORT.NEWEST_FIRST
       Sort by message date, newest first.

     SORT.MESSAGE_ID
       Sort by email message ID

   .. automethod:: set_sort

   .. attribute::  sort

      Instance attribute :attr:`sort` contains the sort order (see
      :attr:`Query.SORT`) if explicitely specified via
      :meth:`set_sort`. By default it is set to `None`.

   .. automethod:: search_threads

   .. automethod:: search_messages

   .. automethod:: count_messages

.. #############################################
.. currentmodule:: cnotmuch.message

:class:`Messages` -- A bunch of messages
----------------------------------------

.. autoclass:: Messages

   .. automethod:: collect_tags

   .. automethod:: __len__

:class:`Message` -- A single message
----------------------------------------

.. autoclass:: Message

   .. automethod:: get_message_id

   .. automethod:: get_thread_id

   .. automethod:: get_replies

   .. automethod:: get_filename

   .. automethod:: get_flag

   .. automethod:: set_flag
   
   .. automethod:: get_date

   .. automethod:: get_header

   .. automethod:: get_tags

   .. automethod:: remove_tag

   .. automethod:: add_tag

   .. automethod:: remove_all_tags

   .. automethod:: freeze

   .. automethod:: thaw

   .. automethod:: format_as_text

   .. automethod:: __str__

.. #############################################
.. currentmodule:: cnotmuch.tag

:class:`Tags` -- Notmuch tags
-----------------------------

.. autoclass:: Tags
   :members:

   .. automethod:: __len__

   .. automethod:: __str__


.. #############################################
.. currentmodule:: cnotmuch.thread

:class:`Threads` -- Threads iterator
------------------------------------

.. autoclass:: Threads

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

.. #############################################
.. currentmodule:: cnotmuch.notmuch

:class:`Filenames` -- An iterator over filenames
------------------------------------------------

To be implemented

:class:`Directoy` -- A directory entry in the database
------------------------------------------------------

To be implemented

:exc:`NotmuchError` -- A Notmuch execution error
------------------------------------------------
.. autoexception:: NotmuchError
   :members:

   This execption inherits directly from :exc:`Exception` and is raised on errors during the notmuch execution.

:class:`STATUS` -- Notmuch operation return status
--------------------------------------------------
.. autoclass:: STATUS

   To be documented

Indices and tables
==================

* :ref:`genindex`
* :ref:`search`

