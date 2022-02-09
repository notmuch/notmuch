import collections.abc

import notmuch2._base as base
import notmuch2._capi as capi
import notmuch2._errors as errors


__all__ = ['ConfigMapping']


class ConfigIter(base.NotmuchIter):

    def __init__(self, parent, iter_p):
        super().__init__(
            parent, iter_p,
            fn_destroy=capi.lib.notmuch_config_pairs_destroy,
            fn_valid=capi.lib.notmuch_config_pairs_valid,
            fn_get=capi.lib.notmuch_config_pairs_key,
            fn_next=capi.lib.notmuch_config_pairs_move_to_next)

    def __next__(self):
        # skip pairs whose value is NULL
        while capi.lib.notmuch_config_pairs_valid(super()._iter_p):
            val_p = capi.lib.notmuch_config_pairs_value(super()._iter_p)
            key_p = capi.lib.notmuch_config_pairs_key(super()._iter_p)
            if key_p == capi.ffi.NULL:
                # this should never happen
                raise errors.NullPointerError
            key = base.BinString.from_cffi(key_p)
            capi.lib.notmuch_config_pairs_move_to_next(super()._iter_p)
            if val_p != capi.ffi.NULL and base.BinString.from_cffi(val_p) != "":
                return key
        self._destroy()
        raise StopIteration

class ConfigMapping(base.NotmuchObject, collections.abc.MutableMapping):
    """The config key/value pairs loaded from the database, config file,
    and and/or defaults.

    The entries are exposed as a :class:`collections.abc.MutableMapping` object.
    Note that setting a value to an empty string is the same as deleting it.

    Mutating (deleting or updating values) in the map persists only in
    the database, which can be shadowed by config files.

    :param parent: the parent object
    :param ptr_name: the name of the attribute on the parent which will
       return the memory pointer.  This allows this object to
       access the pointer via the parent's descriptor and thus
       trigger :class:`MemoryPointer`'s memory safety.

    """

    def __init__(self, parent, ptr_name):
        self._parent = parent
        self._ptr = lambda: getattr(parent, ptr_name)

    @property
    def alive(self):
        return self._parent.alive

    def _destroy(self):
        pass

    def __getitem__(self, key):
        if isinstance(key, str):
            key = key.encode('utf-8')
        val_pp = capi.ffi.new('char**')
        ret = capi.lib.notmuch_database_get_config(self._ptr(), key, val_pp)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)
        val = base.BinString.from_cffi(val_pp[0])
        capi.lib.free(val_pp[0])
        if val == '':
            raise KeyError
        return val

    def __setitem__(self, key, val):
        if isinstance(key, str):
            key = key.encode('utf-8')
        if isinstance(val, str):
            val = val.encode('utf-8')
        ret = capi.lib.notmuch_database_set_config(self._ptr(), key, val)
        if ret != capi.lib.NOTMUCH_STATUS_SUCCESS:
            raise errors.NotmuchError(ret)

    def __delitem__(self, key):
        self[key] = ""

    def __iter__(self):
        """Return an iterator over the config items.

        :raises NullPointerError: If the iterator can not be created.
        """
        config_pairs_p = capi.lib.notmuch_config_get_pairs(self._ptr(), b'')
        if config_pairs_p == capi.ffi.NULL:
            raise KeyError
        return ConfigIter(self._parent, config_pairs_p)

    def __len__(self):
        return sum(1 for t in self)
