#!/usr/bin/env bash
test_description="threading when messages received out of order"
. ./test-lib.sh

test_begin_subtest "Adding initial child message"
generate_message [body]=foo "[in-reply-to]=\<parent-id\>" [subject]=brokenthreadtest '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
output=$(NOTMUCH_NEW)
test_expect_equal "$output" "Added 1 new message to the database."

test_begin_subtest "Searching returns the message"
output=$(notmuch search foo | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; brokenthreadtest (inbox unread)"

test_begin_subtest "Adding second child message"
generate_message [body]=foo "[in-reply-to]=\<parent-id\>" [subject]=brokenthreadtest '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
output=$(NOTMUCH_NEW)
test_expect_equal "$output" "Added 1 new message to the database."

test_begin_subtest "Searching returns both messages in one thread"
output=$(notmuch search foo | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [2/2] Notmuch Test Suite; brokenthreadtest (inbox unread)"

test_begin_subtest "Adding parent message"
generate_message [body]=foo [id]=parent-id [subject]=brokenthreadtest '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
output=$(NOTMUCH_NEW)
test_expect_equal "$output" "Added 1 new message to the database."

test_begin_subtest "Searching returns all three messages in one thread"
output=$(notmuch search foo | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [3/3] Notmuch Test Suite; brokenthreadtest (inbox unread)"

test_done
