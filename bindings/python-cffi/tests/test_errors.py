from notmuch2 import _capi as capi
from notmuch2 import _errors as errors

def test_status_no_message():
    exc = errors.NotmuchError(capi.lib.NOTMUCH_STATUS_PATH_ERROR)
    assert exc.status == capi.lib.NOTMUCH_STATUS_PATH_ERROR
    assert exc.message is None
    assert str(exc) == 'Path supplied is illegal for this function'
