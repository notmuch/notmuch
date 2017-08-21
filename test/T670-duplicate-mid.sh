#!/usr/bin/env bash
test_description="duplicate message ids"
. ./test-lib.sh || exit 1

add_message '[id]="duplicate"' '[subject]="message 1" [filename]=copy1'
add_message '[id]="duplicate"' '[subject]="message 2" [filename]=copy2'

test_begin_subtest 'First subject preserved'
cat <<EOF > EXPECTED
thread:XXX   2001-01-05 [1/1(2)] Notmuch Test Suite; message 1 (inbox unread)
EOF
notmuch search id:duplicate | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'Search for second subject'
cat <<EOF >EXPECTED
MAIL_DIR/copy1
MAIL_DIR/copy2
EOF
notmuch search --output=files subject:'"message 2"' | notmuch_dir_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

add_message '[id]="duplicate"' '[body]="sekrit" [filename]=copy3'
test_begin_subtest 'search for body in duplicate file'
cat <<EOF >EXPECTED
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

rm ${MAIL_DIR}/copy1
test_begin_subtest 'Deleted first duplicate file does not stop notmuch show from working'
output=$(notmuch show --body=false --format=json id:duplicate)
expected='[[[{
    "id": "'duplicate'",
    "match": true,
    "excluded": false,
    "filename": [
        "'"${MAIL_DIR}"/copy1'",
        "'"${MAIL_DIR}"/copy2'"
    ],
    "timestamp": 978709435,
    "date_relative": "2001-01-05",
    "tags": ["inbox","unread"],
    "headers": {
        "Subject": "message 2",
        "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
        "To": "Notmuch Test Suite <test_suite@notmuchmail.org>",
        "Date": "Fri, 05 Jan 2001 15:43:55 +0000"
    }
 },
[]]]]'

test_expect_equal_json "$output" "$expected"

test_done
