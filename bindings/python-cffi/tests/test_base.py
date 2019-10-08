import pytest

from notdb import _base as base
from notdb import _errors as errors


class TestNotmuchObject:

    def test_no_impl_methods(self):
        class Object(base.NotmuchObject):
            pass
        with pytest.raises(TypeError):
            Object()

    def test_impl_methods(self):

        class Object(base.NotmuchObject):

            def __init__(self):
                pass

            @property
            def alive(self):
                pass

            def _destroy(self, parent=False):
                pass

        Object()

    def test_del(self):
        destroyed = False

        class Object(base.NotmuchObject):

            def __init__(self):
                pass

            @property
            def alive(self):
                pass

            def _destroy(self, parent=False):
                nonlocal destroyed
                destroyed = True

        o = Object()
        o.__del__()
        assert destroyed


class TestMemoryPointer:

    @pytest.fixture
    def obj(self):
        class Cls:
            ptr = base.MemoryPointer()
        return Cls()

    def test_unset(self, obj):
        with pytest.raises(errors.ObjectDestroyedError):
            obj.ptr

    def test_set(self, obj):
        obj.ptr = 'some'
        assert obj.ptr == 'some'

    def test_cleared(self, obj):
        obj.ptr = 'some'
        obj.ptr
        obj.ptr = None
        with pytest.raises(errors.ObjectDestroyedError):
            obj.ptr

    def test_two_instances(self, obj):
        obj2 = obj.__class__()
        obj.ptr = 'foo'
        obj2.ptr = 'bar'
        assert obj.ptr != obj2.ptr


class TestBinString:

    def test_type(self):
        s = base.BinString(b'foo')
        assert isinstance(s, str)

    def test_init_bytes(self):
        s = base.BinString(b'foo')
        assert s == 'foo'

    def test_init_str(self):
        s = base.BinString('foo')
        assert s == 'foo'

    def test_bytes(self):
        s = base.BinString(b'foo')
        assert bytes(s) == b'foo'

    def test_invalid_utf8(self):
        s = base.BinString(b'\x80foo')
        assert s == 'foo'
        assert bytes(s) == b'\x80foo'

    def test_errors(self):
        s = base.BinString(b'\x80foo', errors='replace')
        assert s == '�foo'
        assert bytes(s) == b'\x80foo'

    def test_encoding(self):
        # pound sign: '£' == '\u00a3' latin-1: b'\xa3', utf-8: b'\xc2\xa3'
        with pytest.raises(UnicodeDecodeError):
            base.BinString(b'\xa3', errors='strict')
        s = base.BinString(b'\xa3', encoding='latin-1', errors='strict')
        assert s == '£'
        assert bytes(s) == b'\xa3'
