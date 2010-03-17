.. cnotmuch documentation master file, created by
   sphinx-quickstart on Tue Feb  2 10:00:47 2010.

.. currentmodule:: cnotmuch.notmuch

Welcome to notmuch's documentation!
===================================

The :mod:`cnotmuch` module provides an interface to the `notmuch <http://notmuchmail.org>`_ functionality, directly interfacing to a shared notmuch library.
The classes :class:`Database`, :class:`Query` provide most of the core functionality, returning :class:`Messages` and :class:`Tags`.

.. moduleauthor:: Sebastian Spaeth <Sebastian@SSpaeth.de>

:License: This module is covered under the GNU GPL v3 (or later).

This page contains the main API overview. More information on specific topics can be found on the following pages: (none here yet)

Notmuch can be imported as:

 from cnotmuch import notmuch

or:

 from cnotmuch.notmuch import Query,Database

.. toctree::
   :maxdepth: 1



:mod:`notmuch` -- The Notmuch interface
=============================================

Document from cnotmuch.globals import nmlib,STATUS

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

     READ_ONLY
       Open the database in read-only mode

     READ_WRITE
       Open the database in read-write mode

   .. autoattribute:: db_p

:class:`Query` -- Represents a notmuch Query
-----------------------------------------------

.. autoclass:: Query
   :members:

:class:`Messages` -- A bunch of messages
----------------------------------------

.. autoclass:: Messages
   :members:

:class:`Message` -- A single message
----------------------------------------

.. autoclass:: Message
   :members:

:class:`Tags` -- A bunch of notmuch tags
----------------------------------------

.. autoclass:: Tags
   :members:

   .. data: '__notmuchcmd__'

      This is the actual binary that will be executed in order to run a notmuch command. This is set to *notmuch* and should usually not be changed.

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

