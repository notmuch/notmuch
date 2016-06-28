:class:`Threads` -- Threads iterator
====================================

.. currentmodule:: notmuch

.. autoclass:: Threads

   .. method:: __len__
   .. warning::
      :meth:`__len__` was removed in version 0.22 as it exhausted the
      iterator and broke list(Threads()). Use `len(list(msgs))`
      instead.

.. automethod:: __str__
