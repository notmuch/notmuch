#!/usr/bin/env bash

# Test to demonstrate a position overlap bug.
#
# At one point, notmuch would index terms incorrectly in the case of
# calling index_terms multiple times for a single field. The term
# generator was being reset to position 0 each time. This means that
# with text such as:
#
#	To: a@b.c, x@y.z
#
# one could get a bogus match by searching for:
#
#	To: a@y.c
#
# Thanks to Mark Anderson for reporting the bug, (and providing a nice,
# minimal test case that inspired what is used here), in
# id:3wd4o8wa7fx.fsf@testarossa.amd.com

test_description='that notmuch does not overlap term positions'
. ./test-lib.sh || exit 1

add_message '[to]="a@b.c, x@y.z"'

test_begin_subtest "Search for a@b.c matches"
output=$(notmuch search a@b.c | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Test message #1 (inbox unread)"

test_begin_subtest "Search for x@y.z matches"
output=$(notmuch search x@y.z | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Test message #1 (inbox unread)"

test_begin_subtest "Search for a@y.c must not match"
output=$(notmuch search a@y.c | notmuch_search_sanitize)
test_expect_equal "$output" ""

test_done
