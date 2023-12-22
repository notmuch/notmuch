#!/usr/bin/env bash
test_description="custom separator for authors"
. $(dirname "$0")/test-lib.sh || exit 1

test_begin_subtest "Adding parent message"
generate_message [body]=findme [id]=new-parent-id [subject]=author-reorder-threadtest '[from]="User <user@example.com>"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
output=$(NOTMUCH_NEW)
test_expect_equal "$output" "Added 1 new message to the database."

test_begin_subtest "Adding initial child message"
generate_message [body]=findme "[in-reply-to]=\<new-parent-id\>" [subject]=author-reorder-threadtest '[from]="last name, first name <user1@example.com>"' '[date]="Sat, 01 Jan 2000 12:01:00 -0000"'
output=$(NOTMUCH_NEW)
test_expect_equal "$output" "Added 1 new message to the database."

test_begin_subtest "Adding second child message"
generate_message [body]=findme "[in-reply-to]=\<new-parent-id\>" [subject]=author-reorder-threadtest '[from]="first last <user2@example.com>"' '[date]="Sat, 01 Jan 2000 12:02:00 -0000"'
output=$(NOTMUCH_NEW)
test_expect_equal "$output" "Added 1 new message to the database."

test_begin_subtest "Custom separarator is used"
notmuch config set search.authors_matched_separator "#"
notmuch config set search.authors_separator ";"
output=$(notmuch search from:user | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/3] User#last name, first name;first last; author-reorder-threadtest (inbox unread)"

test_done
