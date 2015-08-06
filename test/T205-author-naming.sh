#!/usr/bin/env bash
test_description="naming of authors with unusual addresses"
. ./test-lib.sh || exit 1

test_begin_subtest "Add author with empty quoted real name"
add_message '[subject]="author-naming: Initial thread subject"' \
	    '[date]="Fri, 05 Jan 2001 15:43:56 -0000"' \
	    '[from]="\"\" <address@example.com>"'
output=$(notmuch search --sort=oldest-first author-naming and tag:inbox | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] address@example.com; author-naming: Initial thread subject (inbox unread)"

test_done
