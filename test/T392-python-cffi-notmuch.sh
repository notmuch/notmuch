#!/usr/bin/env bash
test_description="python bindings (notmuch test suite)"
. $(dirname "$0")/test-lib.sh || exit 1

if [ $NOTMUCH_HAVE_PYTHON3_CFFI -eq 0 -o $NOTMUCH_HAVE_PYTHON3_PYTEST -eq 0 ]; then
    test_done
fi

add_email_corpus

cat <<EOF > recurse.py
from notmuch2 import Database
def show_msgs(msgs, level):
    print('{:s} {:s}'.format(' ' * level*4, type(msgs).__name__))
    for msg in msgs:
        print('{:s} {:s}'.format(' ' * (level*4+2), type(msg).__name__))
        replies=msg.replies()
        show_msgs(replies, level+1)
db = Database(config=Database.CONFIG.SEARCH)
msg=db.find("87ocn0qh6d.fsf@yoom.home.cworth.org")
threads = db.threads(query="thread:"+msg.threadid)
thread = next (threads)
show_msgs(thread, 0)
EOF

test_begin_subtest "recursive traversal of replies (no crash)"
test_python < recurse.py
error=$?
test_expect_equal "${error}" 0

test_begin_subtest "recursive traversal of replies (output)"
test_python < recurse.py
tail -n 10 < OUTPUT > OUTPUT.sample
cat <<EOF > EXPECTED
   OwnedMessage
     MessageIter
   OwnedMessage
     MessageIter
       OwnedMessage
         MessageIter
   OwnedMessage
     MessageIter
   OwnedMessage
     MessageIter
EOF
test_expect_equal_file EXPECTED OUTPUT.sample

test_done
