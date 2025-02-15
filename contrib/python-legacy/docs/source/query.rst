:class:`Query` -- A search query
================================

.. currentmodule:: notmuch

.. autoclass:: Query

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
      :attr:`Query.SORT`) if explicitly specified via
      :meth:`set_sort`. By default it is set to `None`.

   .. automethod:: exclude_tag

   .. automethod:: search_threads

   .. automethod:: search_messages

   .. automethod:: count_messages

   .. automethod:: count_threads
