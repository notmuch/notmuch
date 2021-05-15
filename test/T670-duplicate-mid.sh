#!/usr/bin/env bash
test_description="duplicate message ids"
. $(dirname "$0")/test-lib.sh || exit 1

test_require_external_prereq xapian-delve

add_message '[id]="duplicate"' '[subject]="message 1" [filename]=copy1'
add_message '[id]="duplicate"' '[subject]="message 2" [filename]=copy2'

add_message '[id]="duplicate"' '[subject]="message 0" [filename]=copy0'

test_begin_subtest 'at most 1 thread-id per xapian document'
db=${MAIL_DIR}/.notmuch/xapian
for doc in $(xapian-delve -1 -t '' "$db" | grep '^[1-9]'); do
    xapian-delve -1 -r "$doc" "$db" | grep -c '^G'
done > OUTPUT.raw
sort -u < OUTPUT.raw > OUTPUT
cat <<EOF > EXPECTED
0
1
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'search: first indexed subject preserved'
cat <<EOF > EXPECTED
thread:XXX   2001-01-05 [1/1(3)] Notmuch Test Suite; message 1 (inbox unread)
EOF
notmuch search id:duplicate | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'First subject preserved in notmuch-show (json)'
test_subtest_known_broken
output=$(notmuch show --body=false --format=json id:duplicate | notmuch_json_show_sanitize)
expected='[[[{
    "id": "XXXXX",
    "match": true,
    "excluded": false,
    "filename": [
        "'"${MAIL_DIR}"/copy0'",
        "'"${MAIL_DIR}"/copy1'",
        "'"${MAIL_DIR}"/copy2'"
    ],
    "timestamp": 42,
    "date_relative": "2001-01-05",
    "tags": ["inbox","unread"],
    "headers": {
        "Subject": "message 1",
        "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
        "To": "Notmuch Test Suite <test_suite@notmuchmail.org>",
        "Date": "GENERATED_DATE"
    }
 },
[]]]]'
test_expect_equal_json "$output" "$expected"

test_begin_subtest 'Search for second subject'
cat <<EOF >EXPECTED
MAIL_DIR/copy0
MAIL_DIR/copy1
MAIL_DIR/copy2
EOF
notmuch search --output=files subject:'"message 2"' | notmuch_dir_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'Regexp search for second subject'
test_subtest_known_broken
cat <<EOF >EXPECTED
MAIL_DIR/copy0
MAIL_DIR/copy1
MAIL_DIR/copy2
EOF
notmuch search --output=files 'subject:"/message 2/"' | notmuch_dir_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

add_message '[id]="duplicate"' '[body]="sekrit" [filename]=copy3'
test_begin_subtest 'search for body in duplicate file'
cat <<EOF >EXPECTED
MAIL_DIR/copy0
MAIL_DIR/copy1
MAIL_DIR/copy2
MAIL_DIR/copy3
EOF
notmuch search --output=files "sekrit" | notmuch_dir_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

rm ${MAIL_DIR}/copy3
test_begin_subtest 'reindex drops terms in duplicate file'
cp /dev/null EXPECTED
notmuch reindex '*'
notmuch search --output=files "sekrit" | notmuch_dir_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'reindex choses subject from first filename'
cat <<EOF > EXPECTED
thread:XXX   2001-01-05 [1/1(3)] Notmuch Test Suite; message 0 (inbox unread)
EOF
notmuch search id:duplicate | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

rm ${MAIL_DIR}/copy0
test_begin_subtest 'Deleted first duplicate file does not stop notmuch show from working'
output=$(notmuch show --body=false --format=json id:duplicate |
	     notmuch_json_show_sanitize | sed 's/message [0-9]/A_SUBJECT/')
expected='[[[{
    "id": "XXXXX",
    "crypto": {},
    "match": true,
    "excluded": false,
    "filename": [
        "'"${MAIL_DIR}"/copy0'",
        "'"${MAIL_DIR}"/copy1'",
        "'"${MAIL_DIR}"/copy2'"
    ],
    "timestamp": 42,
    "date_relative": "2001-01-05",
    "tags": ["inbox","unread"],
    "headers": {
        "Subject": "A_SUBJECT",
        "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
        "To": "Notmuch Test Suite <test_suite@notmuchmail.org>",
        "Date": "GENERATED_DATE"
    }
 },
[]]]]'

test_expect_equal_json "$output" "$expected"

test_done
