#!/bin/bash
test_description="threading when messages received out of order"
. ./test-lib.sh
test_expect_success "Adding initial child message" '
generate_message [body]=foo "[in-reply-to]=\<parent-id\>" [subject]=brokenthreadtest "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" &&
output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "Added 1 new message to the database."
'
test_expect_success "Searching returns the message" '
output=$($NOTMUCH search foo | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; brokenthreadtest (inbox unread)"
'
test_expect_success "Adding second child message" '
generate_message [body]=foo "[in-reply-to]=\<parent-id\>" [subject]=brokenthreadtest "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" &&
output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "Added 1 new message to the database."
'
test_expect_success "Searching returns both messages in one thread" '
output=$($NOTMUCH search foo | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [2/2] Notmuch Test Suite; brokenthreadtest (inbox unread)"
'
test_expect_success "Adding parent message" '
generate_message [body]=foo [id]=parent-id [subject]=brokenthreadtest "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" &&
output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "Added 1 new message to the database."
'
test_expect_success "Searching returns all three messages in one thread" '
output=$($NOTMUCH search foo | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [3/3] Notmuch Test Suite; brokenthreadtest (inbox unread)"
'
test_done
