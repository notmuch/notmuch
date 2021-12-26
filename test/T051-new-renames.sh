#!/usr/bin/env bash
test_description='"notmuch new" with directory renames'
. $(dirname "$0")/test-lib.sh || exit 1

for loop in {1..10}; do

rm -rf ${MAIL_DIR}

for i in {1..10}; do
    generate_message '[dir]=foo' '[subject]="Message foo $i"'
done

for i in {1..10}; do
    generate_message '[dir]=bar' '[subject]="Message bar $i"'
done

test_begin_subtest "Index the messages, round $loop"
output=$(NOTMUCH_NEW)
test_expect_equal "$output" "Added 20 new messages to the database."

all_files=$(notmuch search --output=files \*)
count_foo=$(notmuch count folder:foo)

test_begin_subtest "Rename folder"
mv ${MAIL_DIR}/foo ${MAIL_DIR}/baz
output=$(NOTMUCH_NEW)
test_expect_equal "$output" "No new mail. Detected $count_foo file renames."

test_begin_subtest "Rename folder back"
mv ${MAIL_DIR}/baz ${MAIL_DIR}/foo
output=$(NOTMUCH_NEW)
test_expect_equal "$output" "No new mail. Detected $count_foo file renames."

test_begin_subtest "Files remain the same"
output=$(notmuch search --output=files \*)
test_expect_equal "$output" "$all_files"

done

test_done
