#!/bin/bash
test_description="author reordering;"
. ./test-lib.sh
test_expect_success "Adding parent message" '
generate_message [body]=findme [id]=new-parent-id [subject]=author-reorder-threadtest "[from]=\"User <user@example.com>\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" &&
output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "Added 1 new message to the database."
'
test_expect_success "Adding initial child message" '
generate_message [body]=findme "[in-reply-to]=\<new-parent-id\>" [subject]=author-reorder-threadtest "[from]=\"User1 <user1@example.com>\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" &&
output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "Added 1 new message to the database."
'
test_expect_success "Adding second child message" '
generate_message [body]=findme "[in-reply-to]=\<new-parent-id\>" [subject]=author-reorder-threadtest "[from]=\"User2 <user2@example.com>\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" &&
output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "Added 1 new message to the database."
'
test_expect_success "Searching when all three messages match" '
output=$($NOTMUCH search findme | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [3/3] User, User1, User2; author-reorder-threadtest (inbox unread)"
'
test_expect_success "Searching when two messages match" '
output=$($NOTMUCH search User1 or User2 | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [2/3] User1, User2| User; author-reorder-threadtest (inbox unread)"
'
test_expect_success "Searching when only one message matches" '
output=$($NOTMUCH search User2 | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/3] User2| User, User1; author-reorder-threadtest (inbox unread)"
'
test_expect_success "Searching when only first message matches" '
output=$($NOTMUCH search User | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/3] User| User1, User2; author-reorder-threadtest (inbox unread)"
'
test_done
