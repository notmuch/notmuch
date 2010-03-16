from ctypes import CDLL, c_char_p, c_int
#package-global instance of the notmuch library
#TODO: lazy load this on first access?
nmlib = CDLL('/usr/local/lib/libnotmuch.so')

class STATUS(object):
  SUCCESS = 0
  OUT_OF_MEMORY = 1
  READ_ONLY_DATABASE = 2
  XAPIAN_EXCEPTION = 3
  FILE_ERROR = 4
  FILE_NOT_EMAIL = 5
  DUPLICATE_MESSAGE_ID = 6
  NULL_POINTER = 7
  TAG_TOO_LONG = 8
  UNBALANCED_FREEZE_THAW = 9
  NOT_INITIALIZED = 10

  """Get a string representation of a notmuch_status_t value."""
  status2str = nmlib.notmuch_status_to_string
  status2str.restype = c_char_p
  status2str.argtypes = [c_int]

  def __init__(self, status):
      self._status = status

  def __str__(self):
      """Get a string representation of a notmuch_status_t value."""   
      # define strings for custom error messages
      if self._status == STATUS.NOT_INITIALIZED:
        return "Operation on uninitialized DB/MSG/THREAD impossible."
      return str(STATUS.status2str(self._status))

class NotmuchError(Exception):
    def __init__(self, status=None, message=None):
        """Is initiated with a (notmuch.STATUS[,message=None])"""
        super(NotmuchError, self).__init__(message, status)

    def __str__(self):
        if self.args[0] is not None: return self.args[0]
        else: return str(STATUS(self.args[1]))
