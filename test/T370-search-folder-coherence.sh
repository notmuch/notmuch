#!/usr/bin/env bash
test_description='folder tags removed and added through file renames remain consistent'
. ./test-lib.sh || exit 1

test_begin_subtest "No new messages"
output=$(NOTMUCH_NEW)
test_expect_equal "$output" "No new mail."


test_begin_subtest "Single new message"
generate_message
file_x=$gen_msg_filename
id_x=$gen_msg_id
output=$(NOTMUCH_NEW)
test_expect_equal "$output" "Added 1 new message to the database."

test_begin_subtest "Add second folder for same message"
dir=$(dirname $file_x)
mkdir $dir/spam
cp $file_x $dir/spam
output=$(NOTMUCH_NEW)
test_expect_equal "$output" "No new mail."


test_begin_subtest "Multiple files for same message"
cat <<EOF >EXPECTED
MAIL_DIR/msg-001
MAIL_DIR/spam/msg-001
EOF
notmuch search --output=files id:$id_x | notmuch_search_files_sanitize >OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Test matches folder:spam"
output=$(notmuch search folder:spam)
test_expect_equal "$output" "thread:0000000000000001   2001-01-05 [1/1] Notmuch Test Suite; Single new message (inbox unread)"

test_begin_subtest "Remove folder:spam copy of email"
rm $dir/spam/$(basename $file_x)
output=$(NOTMUCH_NEW)
test_expect_equal "$output" "No new mail. Detected 1 file rename."

test_begin_subtest "No mails match the folder:spam search"
output=$(notmuch search folder:spam)
test_expect_equal "$output" ""

test_done
