#!/usr/bin/env bash
test_description='"notmuch search" by folder: (with variations)'
. ./test-lib.sh

add_message '[dir]=bad' '[subject]="To the bone"'
add_message '[dir]=.' '[subject]="Top level"'
add_message '[dir]=bad/news' '[subject]="Bears"'
mkdir -p "${MAIL_DIR}/duplicate/bad/news"
cp "$gen_msg_filename" "${MAIL_DIR}/duplicate/bad/news"

add_message '[dir]=things' '[subject]="These are a few"'
add_message '[dir]=things/favorite' '[subject]="Raindrops, whiskers, kettles"'
add_message '[dir]=things/bad' '[subject]="Bites, stings, sad feelings"'

test_begin_subtest "Single-world folder: specification (multiple results)"
output=$(notmuch search folder:bad folder:bad/news folder:things/bad | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; To the bone (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Bears (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Bites, stings, sad feelings (inbox unread)"

test_begin_subtest "Top level folder"
output=$(notmuch search folder:'""' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Top level (inbox unread)"

test_begin_subtest "Two-word path to narrow results to one"
output=$(notmuch search folder:bad/news | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Bears (inbox unread)"

test_begin_subtest "Folder search with --output=files"
output=$(notmuch search --output=files folder:bad/news | notmuch_search_files_sanitize)
test_expect_equal "$output" "MAIL_DIR/bad/news/msg-003
MAIL_DIR/duplicate/bad/news/msg-003"

test_begin_subtest "After removing duplicate instance of matching path"
rm -r "${MAIL_DIR}/bad/news"
notmuch new
output=$(notmuch search folder:bad/news | notmuch_search_sanitize)
test_expect_equal "$output" ""

test_begin_subtest "Folder search with --output=files part #2"
output=$(notmuch search --output=files folder:duplicate/bad/news | notmuch_search_files_sanitize)
test_expect_equal "$output" "MAIL_DIR/duplicate/bad/news/msg-003"

test_begin_subtest "After removing duplicate instance of matching path part #2"
output=$(notmuch search folder:duplicate/bad/news | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Bears (inbox unread)"

test_begin_subtest "After rename, old path returns nothing"
mv "${MAIL_DIR}/duplicate/bad/news" "${MAIL_DIR}/duplicate/bad/olds"
notmuch new
output=$(notmuch search folder:duplicate/bad/news | notmuch_search_sanitize)
test_expect_equal "$output" ""

test_begin_subtest "After rename, new path returns result"
output=$(notmuch search folder:duplicate/bad/olds | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Bears (inbox unread)"

test_done
