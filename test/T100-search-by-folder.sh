#!/usr/bin/env bash
test_description='"notmuch search" by folder: and path: (with variations)'
. ./test-lib.sh || exit 1

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

# folder: and path: searches with full corpus
rm -rf $MAIL_DIR
add_email_corpus

# add some more dupes
cp $MAIL_DIR/foo/new/03:2, $MAIL_DIR/new
cp $MAIL_DIR/bar/baz/05:2, $MAIL_DIR/foo
notmuch new >/dev/null

test_begin_subtest "folder: search"
output=$(notmuch search --output=files folder:foo | notmuch_search_files_sanitize | sort)
# bar/baz/05:2, is a duplicate of foo/05:2,
# new/03:2, is a duplicate of foo/new/03:2,
test_expect_equal "$output" "MAIL_DIR/bar/baz/05:2,
MAIL_DIR/foo/05:2,
MAIL_DIR/foo/06:2,
MAIL_DIR/foo/cur/07:2,
MAIL_DIR/foo/cur/08:2,
MAIL_DIR/foo/new/03:2,
MAIL_DIR/foo/new/09:2,
MAIL_DIR/foo/new/10:2,
MAIL_DIR/new/03:2,"

test_begin_subtest "top level folder: search"
output=$(notmuch search --output=files folder:'""' | notmuch_search_files_sanitize | sort)
# bar/18:2, is a duplicate of cur/51:2,
# foo/new/03:2, is a duplicate of new/03:2,
test_expect_equal "$output" "MAIL_DIR/01:2,
MAIL_DIR/02:2,
MAIL_DIR/bar/18:2,
MAIL_DIR/cur/29:2,
MAIL_DIR/cur/30:2,
MAIL_DIR/cur/31:2,
MAIL_DIR/cur/32:2,
MAIL_DIR/cur/33:2,
MAIL_DIR/cur/34:2,
MAIL_DIR/cur/35:2,
MAIL_DIR/cur/36:2,
MAIL_DIR/cur/37:2,
MAIL_DIR/cur/38:2,
MAIL_DIR/cur/39:2,
MAIL_DIR/cur/40:2,
MAIL_DIR/cur/41:2,
MAIL_DIR/cur/42:2,
MAIL_DIR/cur/43:2,
MAIL_DIR/cur/44:2,
MAIL_DIR/cur/45:2,
MAIL_DIR/cur/46:2,
MAIL_DIR/cur/47:2,
MAIL_DIR/cur/48:2,
MAIL_DIR/cur/49:2,
MAIL_DIR/cur/50:2,
MAIL_DIR/cur/51:2,
MAIL_DIR/cur/52:2,
MAIL_DIR/cur/53:2,
MAIL_DIR/foo/new/03:2,
MAIL_DIR/new/03:2,
MAIL_DIR/new/04:2,"

test_begin_subtest "path: search"
output=$(notmuch search --output=files path:"bar" | notmuch_search_files_sanitize | sort)
# cur/51:2, is a duplicate of bar/18:2,
test_expect_equal "$output" "MAIL_DIR/bar/17:2,
MAIL_DIR/bar/18:2,
MAIL_DIR/cur/51:2,"

test_begin_subtest "top level path: search"
output=$(notmuch search --output=files path:'""' | notmuch_search_files_sanitize | sort)
test_expect_equal "$output" "MAIL_DIR/01:2,
MAIL_DIR/02:2,"

test_begin_subtest "recursive path: search"
output=$(notmuch search --output=files path:"bar/**" | notmuch_search_files_sanitize | sort)
# cur/51:2, is a duplicate of bar/18:2,
# foo/05:2, is a duplicate of bar/baz/05:2,
test_expect_equal "$output" "MAIL_DIR/bar/17:2,
MAIL_DIR/bar/18:2,
MAIL_DIR/bar/baz/05:2,
MAIL_DIR/bar/baz/23:2,
MAIL_DIR/bar/baz/24:2,
MAIL_DIR/bar/baz/cur/25:2,
MAIL_DIR/bar/baz/cur/26:2,
MAIL_DIR/bar/baz/new/27:2,
MAIL_DIR/bar/baz/new/28:2,
MAIL_DIR/bar/cur/19:2,
MAIL_DIR/bar/cur/20:2,
MAIL_DIR/bar/new/21:2,
MAIL_DIR/bar/new/22:2,
MAIL_DIR/cur/51:2,
MAIL_DIR/foo/05:2,"

test_done
