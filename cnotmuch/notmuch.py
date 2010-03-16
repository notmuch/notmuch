import ctypes
from ctypes import c_int, c_char_p
from database import Database,Tags,Query
from cnotmuch.globals import nmlib,STATUS,NotmuchError

# 114 typedef struct _notmuch_query notmuch_query_t;
# 115 typedef struct _notmuch_threads notmuch_threads_t;
# 116 typedef struct _notmuch_thread notmuch_thread_t;
# 117 typedef struct _notmuch_messages notmuch_messages_t;
# 118 typedef struct _notmuch_message notmuch_message_t;
# 120 typedef struct _notmuch_directory notmuch_directory_t;
# 121 typedef struct _notmuch_filenames notmuch_filenames_t;
