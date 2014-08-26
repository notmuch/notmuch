#!/usr/bin/env bash
test_description="python bindings"
. ./test-lib.sh

add_email_corpus

test_begin_subtest "compare thread ids"
test_python <<EOF
import notmuch
db = notmuch.Database(mode=notmuch.Database.MODE.READ_ONLY)
q_new = notmuch.Query(db, 'tag:inbox')
q_new.set_sort(notmuch.Query.SORT.OLDEST_FIRST)
for t in q_new.search_threads():
    print t.get_thread_id()
EOF
notmuch search --sort=oldest-first --output=threads tag:inbox | sed s/^thread:// > EXPECTED
test_expect_equal_file OUTPUT EXPECTED

test_begin_subtest "compare message ids"
test_python <<EOF
import notmuch
db = notmuch.Database(mode=notmuch.Database.MODE.READ_ONLY)
q_new = notmuch.Query(db, 'tag:inbox')
q_new.set_sort(notmuch.Query.SORT.OLDEST_FIRST)
for m in q_new.search_messages():
    print m.get_message_id()
EOF
notmuch search --sort=oldest-first --output=messages tag:inbox | sed s/^id:// > EXPECTED
test_expect_equal_file OUTPUT EXPECTED

test_begin_subtest "get non-existent file"
test_python <<EOF
import notmuch
db = notmuch.Database(mode=notmuch.Database.MODE.READ_ONLY)
print db.find_message_by_filename("i-dont-exist")
EOF
test_expect_equal "$(cat OUTPUT)" "None"

test_done
