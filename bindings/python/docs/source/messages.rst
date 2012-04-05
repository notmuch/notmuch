:class:`Messages` -- A bunch of messages
========================================

.. currentmodule:: notmuch

.. autoclass:: Messages

   .. automethod:: collect_tags

   .. method:: __len__()

   .. warning::

      :meth:`__len__` was removed in version 0.6 as it exhausted the iterator and broke
      list(Messages()). Use the :meth:`Query.count_messages` function or use `len(list(msgs))`.
