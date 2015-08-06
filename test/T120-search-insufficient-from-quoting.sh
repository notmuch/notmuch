#!/usr/bin/env bash
test_description='messages with unquoted . in name'
. ./test-lib.sh || exit 1

add_message \
  '[from]="Some.Name for Someone <bugs@quoting.com>"' \
  '[subject]="This message needs more quoting on the From line"'

add_message \
  '[from]="\"Some.Name for Someone\" <bugs@quoting.com>"' \
  '[subject]="This message has necessary quoting in place"'

add_message \
  '[from]="No.match Here <filler@mail.com>"' \
  '[subject]="This message needs more quoting on the From line"'

add_message \
  '[from]="\"No.match Here\" <filler@mail.com>"' \
  '[subject]="This message has necessary quoting in place"'


test_begin_subtest "Search by first name"
output=$(notmuch search from:Some.Name | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Some.Name for Someone; This message needs more quoting on the From line (inbox unread)
thread:XXX   2001-01-05 [1/1] Some.Name for Someone; This message has necessary quoting in place (inbox unread)"

test_begin_subtest "Search by last name:"
output=$(notmuch search from:Someone | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Some.Name for Someone; This message needs more quoting on the From line (inbox unread)
thread:XXX   2001-01-05 [1/1] Some.Name for Someone; This message has necessary quoting in place (inbox unread)"

test_begin_subtest "Search by address:"
output=$(notmuch search from:bugs@quoting.com | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Some.Name for Someone; This message needs more quoting on the From line (inbox unread)
thread:XXX   2001-01-05 [1/1] Some.Name for Someone; This message has necessary quoting in place (inbox unread)"

test_begin_subtest "Search for all messages:"
output=$(notmuch search '*' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Some.Name for Someone; This message needs more quoting on the From line (inbox unread)
thread:XXX   2001-01-05 [1/1] Some.Name for Someone; This message has necessary quoting in place (inbox unread)
thread:XXX   2001-01-05 [1/1] No.match Here; This message needs more quoting on the From line (inbox unread)
thread:XXX   2001-01-05 [1/1] No.match Here; This message has necessary quoting in place (inbox unread)"

test_done
