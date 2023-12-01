import abc
import collections.abc

from notmuch2 import _capi as capi
from notmuch2 import _errors as errors


__all__ = ['NotmuchObject', 'BinString']


class NotmuchObject(metaclass=abc.ABCMeta):
    """Base notmuch object syntax.

    This base class exists to define the memory management handling
    required to use the notmuch library.  It is meant as an interface
    definition rather than a base class, though you can use it as a
    base class to ensure you don't forget part of the interface.  It
    only concerns you if you are implementing this package itself
    rather then using it.

    libnotmuch uses a hierarchical memory allocator, where freeing the
    memory of a parent object also frees the memory of all child
    objects.  To make this work seamlessly in Python this package
    keeps references to parent objects which makes them stay alive
    correctly under normal circumstances.  When an object finally gets
    deleted the :meth:`__del__` method will be called to free the
    memory.

    However during some peculiar situations, e.g. interpreter
    shutdown, it is possible for the :meth:`__del__` method to have
    been called, whele there are still references to an object.  This
    could result in child objects asking their memory to be freed
    after the parent has already freed the memory, making things
    rather unhappy as double frees are not taken lightly in C.  To
    handle this case all objects need to follow the same protocol to
    destroy themselves, see :meth:`destroy`.

    Once an object has been destroyed trying to use it should raise
    the :exc:`ObjectDestroyedError` exception.  For this see also the
    convenience :class:`MemoryPointer` descriptor in this module which
    can be used as a pointer to libnotmuch memory.
    """

    @abc.abstractmethod
    def __init__(self, parent, *args, **kwargs):
        """Create a new object.

        Other then for the toplevel :class:`Database` object
        constructors are only ever called by internal code and not by
        the user.  Per convention their signature always takes the
        parent object as first argument.  Feel free to make the rest
        of the signature match the object's requirement.  The object
        needs to keep a reference to the parent, so it can check the
        parent is still alive.
        """

    @property
    @abc.abstractmethod
    def alive(self):
        """Whether the object is still alive.

        This indicates whether the object is still alive.  The first
        thing this needs to check is whether the parent object is
        still alive, if it is not then this object can not be alive
        either.  If the parent is alive then it depends on whether the
        memory for this object has been freed yet or not.
        """

    def __del__(self):
        self._destroy()

    @abc.abstractmethod
    def _destroy(self):
        """Destroy the object, freeing all memory.

        This method needs to destroy the object on the
        libnotmuch-level.  It must ensure it's not been destroyed by
        it's parent object yet before doing so.  It also must be
        idempotent.
        """


class MemoryPointer:
    """Data Descriptor to handle accessing libnotmuch pointers.

    Most :class:`NotmuchObject` instances will have one or more CFFI
    pointers to C-objects.  Once an object is destroyed this pointer
    should no longer be used and a :exc:`ObjectDestroyedError`
    exception should be raised on trying to access it.  This
    descriptor simplifies implementing this, allowing the creation of
    an attribute which can be assigned to, but when accessed when the
    stored value is *None* it will raise the
    :exc:`ObjectDestroyedError` exception::

       class SomeOjb:
           _ptr = MemoryPointer()

           def __init__(self, ptr):
               self._ptr = ptr

           def destroy(self):
               somehow_free(self._ptr)
               self._ptr = None

           def do_something(self):
               return some_libnotmuch_call(self._ptr)
    """

    def __get__(self, instance, owner):
        try:
            val = getattr(instance, self.attr_name, None)
        except AttributeError:
            # We're not on 3.6+ and self.attr_name does not exist
            self.__set_name__(instance, 'dummy')
            val = getattr(instance, self.attr_name, None)
        if val is None:
            raise errors.ObjectDestroyedError()
        return val

    def __set__(self, instance, value):
        try:
            setattr(instance, self.attr_name, value)
        except AttributeError:
            # We're not on 3.6+ and self.attr_name does not exist
            self.__set_name__(instance, 'dummy')
            setattr(instance, self.attr_name, value)

    def __set_name__(self, instance, name):
        self.attr_name = '_memptr_{}_{:x}'.format(name, id(instance))


class BinString(str):
    """A str subclass with binary data.

    Most data in libnotmuch should be valid ASCII or valid UTF-8.
    However since it is a C library these are represented as
    bytestrings instead which means on an API level we can not
    guarantee that decoding this to UTF-8 will both succeed and be
    lossless.  This string type converts bytes to unicode in a lossy
    way, but also makes the raw bytes available.

    This object is a normal unicode string for most intents and
    purposes, but you can get the original bytestring back by calling
    ``bytes()`` on it.
    """

    def __new__(cls, data, encoding='utf-8', errors='ignore'):
        if not isinstance(data, bytes):
            data = bytes(data, encoding=encoding)
        strdata = str(data, encoding=encoding, errors=errors)
        inst = super().__new__(cls, strdata)
        inst._bindata = data
        return inst

    @classmethod
    def from_cffi(cls, cdata):
        """Create a new string from a CFFI cdata pointer."""
        return cls(capi.ffi.string(cdata))

    def __bytes__(self):
        return self._bindata


class NotmuchIter(NotmuchObject, collections.abc.Iterator):
    """An iterator for libnotmuch iterators.

    It is tempting to use a generator function instead, but this would
    not correctly respect the :class:`NotmuchObject` memory handling
    protocol and in some unsuspecting cornercases cause memory
    trouble.  You probably want to sublcass this in order to wrap the
    value returned by :meth:`__next__`.

    :param parent: The parent object.
    :type parent: NotmuchObject
    :param iter_p: The CFFI pointer to the C iterator.
    :type iter_p: cffi.cdata
    :param fn_destory: The CFFI notmuch_*_destroy function.
    :param fn_valid: The CFFI notmuch_*_valid function.
    :param fn_get: The CFFI notmuch_*_get function.
    :param fn_next: The CFFI notmuch_*_move_to_next function.
    """
    _iter_p = MemoryPointer()

    def __init__(self, parent, iter_p,
                 *, fn_destroy, fn_valid, fn_get, fn_next):
        self._parent = parent
        self._iter_p = iter_p
        self._fn_destroy = fn_destroy
        self._fn_valid = fn_valid
        self._fn_get = fn_get
        self._fn_next = fn_next

    def __del__(self):
        self._destroy()

    @property
    def alive(self):
        if not self._parent.alive:
            return False
        try:
            self._iter_p
        except errors.ObjectDestroyedError:
            return False
        else:
            return True

    def _destroy(self):
        if self.alive:
            try:
                self._fn_destroy(self._iter_p)
            except errors.ObjectDestroyedError:
                pass
        self._iter_p = None

    def __iter__(self):
        """Return the iterator itself.

        Note that as this is an iterator and not a container this will
        not return a new iterator.  Thus any elements already consumed
        will not be yielded by the :meth:`__next__` method anymore.
        """
        return self

    def __next__(self):
        if not self._fn_valid(self._iter_p):
            self._destroy()
            raise StopIteration()
        obj_p = self._fn_get(self._iter_p)
        self._fn_next(self._iter_p)
        return obj_p

    def __repr__(self):
        try:
            self._iter_p
        except errors.ObjectDestroyedError:
            return '<NotmuchIter (exhausted)>'
        else:
            return '<NotmuchIter>'
