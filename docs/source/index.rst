.. cnotmuch documentation master file, created by
   sphinx-quickstart on Tue Feb  2 10:00:47 2010.

.. currentmodule:: cnotmuch

Welcome to :mod:`cnotmuch`'s documentation
===========================================

The :mod:`cnotmuch` module provides an interface to the `notmuch <http://notmuchmail.org>`_ functionality, directly interfacing to a shared notmuch library.
The classes :class:`notmuch.Database`, :class:`notmuch.Query` provide most of the core functionality, returning :class:`notmuch.Messages` and :class:`notmuch.Tags`.

.. moduleauthor:: Sebastian Spaeth <Sebastian@SSpaeth.de>

:License: This module is covered under the GNU GPL v3 (or later).

This page contains the main API overview. More information on specific topics can be found on the following pages: (none here yet)

Notmuch can be imported as::

 from cnotmuch import notmuch

or::

 from cnotmuch.notmuch import Query,Database

.. toctree::
   :maxdepth: 1


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

   .. automethod:: find_message

   .. automethod:: get_all_tags


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

   .. automethod:: search_messages

:class:`Messages` -- A bunch of messages
----------------------------------------

.. autoclass:: Messages

   .. automethod:: collect_tags

   .. automethod:: __len__

:class:`Message` -- A single message
----------------------------------------

.. autoclass:: Message
   :members:

:class:`Tags` -- Notmuch tags
-----------------------------

.. autoclass:: Tags
   :members:

   .. automethod:: __len__

   .. automethod:: __str__

:exc:`NotmuchError` -- A Notmuch execution error
------------------------------------------------
.. autoexception:: NotmuchError
   :members:

   This execption inherits directly from :exc:`Exception` and is raised on errors during the notmuch execution.

:class:`STATUS` -- Notmuch operation return status
--------------------------------------------------
.. autoclass:: STATUS
   :members:

Indices and tables
==================

* :ref:`genindex`
* :ref:`search`

