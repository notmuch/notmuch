:class:`Message` -- A single message
====================================

.. currentmodule:: notmuch

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

   .. automethod:: __str__
