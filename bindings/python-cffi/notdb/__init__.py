"""Pythonic API to the notmuch database.

Creating Objects
================

Only the :class:`Database` object is meant to be created by the user.
All other objects should be created from this initial object.  Users
should consider their signatures implementation details.

Errors
======

All errors occuring due to errors from the underlying notmuch database
are subclasses of the :exc:`NotmuchError`.  Due to memory management
it is possible to try and use an object after it has been freed.  In
this case a :exc:`ObjectDestoryedError` will be raised.

Memory Management
=================

Libnotmuch uses a hierarchical memory allocator, this means all
objects have a strict parent-child relationship and when the parent is
freed all the children are freed as well.  This has some implications
for these Python bindings as parent objects need to be kept alive.
This is normally schielded entirely from the user however and the
Python objects automatically make sure the right references are kept
alive.  It is however the reason the :class:`BaseObject` exists as it
defines the API all Python objects need to implement to work
correctly.

Collections and Containers
==========================

Libnotmuch exposes nearly all collections of things as iterators only.
In these python bindings they have sometimes been exposed as
:class:`collections.abc.Container` instances or subclasses of this
like :class:`collections.abc.Set` or :class:`collections.abc.Mapping`
etc.  This gives a more natural API to work with, e.g. being able to
treat tags as sets.  However it does mean that the
:meth:`__contains__`, :meth:`__len__` and frieds methods on these are
usually more and essentially O(n) rather than O(1) as you might
usually expect from Python containers.
"""

from notdb import _capi
from notdb._base import *
from notdb._database import *
from notdb._errors import *
from notdb._message import *
from notdb._tags import *
from notdb._thread import *


NOTMUCH_TAG_MAX = _capi.lib.NOTMUCH_TAG_MAX
del _capi


# Re-home all the objects to the package.  This leaves __qualname__ intact.
for x in locals().copy().values():
    if hasattr(x, '__module__'):
        x.__module__ = __name__
del x
