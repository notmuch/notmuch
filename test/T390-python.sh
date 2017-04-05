#!/usr/bin/env bash
test_description="python bindings"
. ./test-lib.sh || exit 1

test_require_external_prereq ${NOTMUCH_PYTHON}

add_email_corpus

test_begin_subtest "compare thread ids"
test_python <<EOF
import notmuch
db = notmuch.Database(mode=notmuch.Database.MODE.READ_ONLY)
q_new = notmuch.Query(db, 'tag:inbox')
q_new.set_sort(notmuch.Query.SORT.OLDEST_FIRST)
for t in q_new.search_threads():
    print (t.get_thread_id())
EOF
notmuch search --sort=oldest-first --output=threads tag:inbox | sed s/^thread:// > EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "compare message ids"
test_python <<EOF
import notmuch
db = notmuch.Database(mode=notmuch.Database.MODE.READ_ONLY)
q_new = notmuch.Query(db, 'tag:inbox')
q_new.set_sort(notmuch.Query.SORT.OLDEST_FIRST)
for m in q_new.search_messages():
    print (m.get_message_id())
EOF
notmuch search --sort=oldest-first --output=messages tag:inbox | sed s/^id:// > EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "get non-existent file"
test_python <<EOF
import notmuch
db = notmuch.Database(mode=notmuch.Database.MODE.READ_ONLY)
print (db.find_message_by_filename("i-dont-exist"))
EOF
test_expect_equal "$(cat OUTPUT)" "None"

test_begin_subtest "get revision"
test_python ${MAIL_DIR} <<'EOF'
import notmuch
db = notmuch.Database(mode=notmuch.Database.MODE.READ_ONLY)
(revision, uuid) = db.get_revision()
print ("%s\t%lu" % (uuid, revision))
EOF
notmuch_uuid_sanitize < OUTPUT > CLEAN
cat <<'EOF' >EXPECTED
UUID	53
EOF
test_expect_equal_file EXPECTED CLEAN

grep '^[0-9a-f]' OUTPUT > INITIAL_OUTPUT

test_begin_subtest "output of count matches test code"
notmuch count --lastmod '*' | cut -f2-3 > OUTPUT
test_expect_equal_file INITIAL_OUTPUT OUTPUT

test_done
