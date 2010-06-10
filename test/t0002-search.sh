#!/bin/bash
test_description="\"notmuch search\" in several variations"
. ./test-lib.sh

test_expect_success "Search body" '
add_message "[subject]=\"body search\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" [body]=bodysearchtest &&
output=$($NOTMUCH search bodysearchtest | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; body search (inbox unread)"

'
test_expect_success "Search by from:" '
add_message "[subject]=\"search by from\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" [from]=searchbyfrom &&
output=$($NOTMUCH search from:searchbyfrom | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/1] searchbyfrom; search by from (inbox unread)"

'
test_expect_success "Search by to:" '
add_message "[subject]=\"search by to\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" [to]=searchbyto &&
output=$($NOTMUCH search to:searchbyto | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (inbox unread)"

'
test_expect_success "Search by subject:" '
add_message [subject]=subjectsearchtest "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" &&
output=$($NOTMUCH search subject:subjectsearchtest | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; subjectsearchtest (inbox unread)"

'
test_expect_success "Search by id:" '
add_message "[subject]=\"search by id\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" &&
output=$($NOTMUCH search id:${gen_msg_id} | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by id (inbox unread)"

'
test_expect_success "Search by tag:" '
add_message "[subject]=\"search by tag\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" &&
$NOTMUCH tag +searchbytag id:${gen_msg_id} &&
output=$($NOTMUCH search tag:searchbytag | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by tag (inbox searchbytag unread)"

'
test_expect_success "Search by thread:" '
add_message "[subject]=\"search by thread\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" &&
thread_id=$($NOTMUCH search id:${gen_msg_id} | sed -e "s/thread:\([a-f0-9]*\).*/\1/") &&
output=$($NOTMUCH search thread:${thread_id} | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by thread (inbox unread)"

'
test_expect_success "Search body (phrase)" '
add_message "[subject]=\"body search (phrase)\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[body]=\"body search (phrase)\"" &&
add_message "[subject]=\"negative result\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[body]=\"This phrase should not match the body search\"" &&
output=$($NOTMUCH search "\"body search (phrase)\"" | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; body search (phrase) (inbox unread)"

'
test_expect_success "Search by from: (address)" '
add_message "[subject]=\"search by from (address)\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" [from]=searchbyfrom@example.com &&
output=$($NOTMUCH search from:searchbyfrom@example.com | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/1] searchbyfrom@example.com; search by from (address) (inbox unread)"

'
test_expect_success "Search by from: (name)" '
add_message "[subject]=\"search by from (name)\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[from]=\"Search By From Name <test@example.com>\"" &&
output=$($NOTMUCH search from:"Search By From Name" | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/1] Search By From Name; search by from (name) (inbox unread)"

'
test_expect_success "Search by to: (address)" '
add_message "[subject]=\"search by to (address)\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" [to]=searchbyto@example.com &&
output=$($NOTMUCH search to:searchbyto@example.com | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (address) (inbox unread)"

'
test_expect_success "Search by to: (name)" '
add_message "[subject]=\"search by to (name)\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[to]=\"Search By To Name <test@example.com>\"" &&
output=$($NOTMUCH search to:"Search By To Name" | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (name) (inbox unread)"

'
test_expect_success "Search by subject: (phrase)" '
add_message "[subject]=\"subject search test (phrase)\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" &&
add_message "[subject]=\"this phrase should not match the subject search test\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" &&
output=$($NOTMUCH search "subject:\"subject search test (phrase)\"" | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; subject search test (phrase) (inbox unread)"

'
test_expect_success "Search for all messages (\"*\"):" '
output=$($NOTMUCH search "*" | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; body search (inbox unread)
thread:XXX   2000-01-01 [1/1] searchbyfrom; search by from (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; subjectsearchtest (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by id (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by tag (inbox searchbytag unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by thread (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; body search (phrase) (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; negative result (inbox unread)
thread:XXX   2000-01-01 [1/1] searchbyfrom@example.com; search by from (address) (inbox unread)
thread:XXX   2000-01-01 [1/1] Search By From Name; search by from (name) (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (address) (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (name) (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; subject search test (phrase) (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; this phrase should not match the subject search test (inbox unread)"

'
test_expect_success "Search body (utf-8):" '
add_message "[subject]=\"utf8-message-body-subject\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[body]=\"message body utf8: bödý\"" &&
output=$($NOTMUCH search "bödý" | notmuch_search_sanitize) &&
pass_if_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-message-body-subject (inbox unread)"
'
test_done
