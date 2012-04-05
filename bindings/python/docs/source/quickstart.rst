Quickstart and examples
=======================

.. todo:: write a nice introduction
.. todo:: improve the examples

Notmuch can be imported as::

    import notmuch

or::

    from notmuch import Query, Database

    db = Database('path', create=True)
    msgs = Query(db, 'from:myself').search_messages()

    for msg in msgs:
        print(msg)
