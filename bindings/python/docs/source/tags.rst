:class:`Tags` -- Notmuch tags
-----------------------------

.. currentmodule:: notmuch

.. autoclass:: Tags
   :members:

   .. method:: __len__

       .. warning::

            :meth:`__len__` was removed in version 0.6 as it exhausted the iterator and broke
            list(Tags()). Use :meth:`len(list(msgs))` instead if you need to know the number of
            tags.

   .. automethod:: __str__
