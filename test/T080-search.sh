#!/usr/bin/env bash
test_description='"notmuch search" in several variations'
. ./test-lib.sh || exit 1

add_email_corpus

test_begin_subtest "Search body"
add_message '[subject]="body search"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' [body]=bodysearchtest
output=$(notmuch search bodysearchtest | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; body search (inbox unread)"

test_begin_subtest "Search by from:"
add_message '[subject]="search by from"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' [from]=searchbyfrom
output=$(notmuch search from:searchbyfrom | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] searchbyfrom; search by from (inbox unread)"

test_begin_subtest "Search by to:"
add_message '[subject]="search by to"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' [to]=searchbyto
output=$(notmuch search to:searchbyto | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (inbox unread)"

test_begin_subtest "Search by subject:"
add_message [subject]=subjectsearchtest '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
output=$(notmuch search subject:subjectsearchtest | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; subjectsearchtest (inbox unread)"

test_begin_subtest "Search by subject (utf-8):"
add_message [subject]=utf8-sübjéct '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
output=$(notmuch search subject:utf8-sübjéct | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-sübjéct (inbox unread)"

test_begin_subtest "Search by id:"
add_message '[subject]="search by id"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
output=$(notmuch search id:${gen_msg_id} | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by id (inbox unread)"

test_begin_subtest "Search by mid:"
add_message '[subject]="search by mid"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
output=$(notmuch search mid:${gen_msg_id} | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by mid (inbox unread)"

test_begin_subtest "Search by tag:"
add_message '[subject]="search by tag"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
notmuch tag +searchbytag id:${gen_msg_id}
output=$(notmuch search tag:searchbytag | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by tag (inbox searchbytag unread)"

test_begin_subtest "Search by thread:"
add_message '[subject]="search by thread"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
thread_id=$(notmuch search id:${gen_msg_id} | sed -e "s/thread:\([a-f0-9]*\).*/\1/")
output=$(notmuch search thread:${thread_id} | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by thread (inbox unread)"

test_begin_subtest "Search body (phrase)"
add_message '[subject]="body search (phrase)"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' '[body]="body search (phrase)"'
add_message '[subject]="negative result"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' '[body]="This phrase should not match the body search"'
output=$(notmuch search '"body search (phrase)"' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; body search (phrase) (inbox unread)"

test_begin_subtest "Search by from: (address)"
add_message '[subject]="search by from (address)"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' [from]=searchbyfrom@example.com
output=$(notmuch search from:searchbyfrom@example.com | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] searchbyfrom@example.com; search by from (address) (inbox unread)"

test_begin_subtest "Search by from: (name)"
add_message '[subject]="search by from (name)"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' '[from]="Search By From Name <test@example.com>"'
output=$(notmuch search 'from:"Search By From Name"' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Search By From Name; search by from (name) (inbox unread)"

test_begin_subtest "Search by from: (name and address)"
output=$(notmuch search 'from:"Search By From Name <test@example.com>"' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Search By From Name; search by from (name) (inbox unread)"

test_begin_subtest "Search by from: without prefix (name and address)"
output=$(notmuch search '"Search By From Name <test@example.com>"' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Search By From Name; search by from (name) (inbox unread)"

test_begin_subtest "Search by to: (address)"
add_message '[subject]="search by to (address)"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' [to]=searchbyto@example.com
output=$(notmuch search to:searchbyto@example.com | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (address) (inbox unread)"

test_begin_subtest "Search by to: (name)"
add_message '[subject]="search by to (name)"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' '[to]="Search By To Name <test@example.com>"'
output=$(notmuch search 'to:"Search By To Name"' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (name) (inbox unread)"

test_begin_subtest "Search by to: (name and adress)"
output=$(notmuch search 'to:"Search By To Name <test@example.com>"' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (name) (inbox unread)"

test_begin_subtest "Search by to: without prefix (name and adress)"
output=$(notmuch search '"Search By To Name <test@example.com>"' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (name) (inbox unread)"

test_begin_subtest "Search by subject: (phrase)"
add_message '[subject]="subject search test (phrase)"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
add_message '[subject]="this phrase should not match the subject search test"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
output=$(notmuch search 'subject:"subject search test (phrase)"' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; subject search test (phrase) (inbox unread)"

test_begin_subtest 'Search for all messages ("*")'
notmuch search '*' | notmuch_search_sanitize > OUTPUT
cat <<EOF >EXPECTED
thread:XXX   2010-12-29 [1/1] François Boulogne; [aur-general] Guidelines: cp, mkdir vs install (inbox unread)
thread:XXX   2010-12-16 [1/1] Olivier Berger; Essai accentué (inbox unread)
thread:XXX   2009-11-18 [1/1] Chris Wilson; [notmuch] [PATCH 1/2] Makefile: evaluate pkg-config once (inbox unread)
thread:XXX   2009-11-18 [2/2] Alex Botero-Lowry, Carl Worth; [notmuch] [PATCH] Error out if no query is supplied to search instead of going into an infinite loop (attachment inbox unread)
thread:XXX   2009-11-18 [2/2] Ingmar Vanhassel, Carl Worth; [notmuch] [PATCH] Typsos (inbox unread)
thread:XXX   2009-11-18 [3/3] Adrian Perez de Castro, Keith Packard, Carl Worth; [notmuch] Introducing myself (inbox signed unread)
thread:XXX   2009-11-18 [3/3] Israel Herraiz, Keith Packard, Carl Worth; [notmuch] New to the list (inbox unread)
thread:XXX   2009-11-18 [3/3] Jan Janak, Carl Worth; [notmuch] What a great idea! (inbox unread)
thread:XXX   2009-11-18 [2/2] Jan Janak, Carl Worth; [notmuch] [PATCH] Older versions of install do not support -C. (inbox unread)
thread:XXX   2009-11-18 [3/3] Aron Griffis, Keith Packard, Carl Worth; [notmuch] archive (inbox unread)
thread:XXX   2009-11-18 [2/2] Keith Packard, Carl Worth; [notmuch] [PATCH] Make notmuch-show 'X' (and 'x') commands remove inbox (and unread) tags (inbox unread)
thread:XXX   2009-11-18 [7/7] Lars Kellogg-Stedman, Mikhail Gusarov, Keith Packard, Carl Worth; [notmuch] Working with Maildir storage? (inbox signed unread)
thread:XXX   2009-11-18 [5/5] Mikhail Gusarov, Carl Worth, Keith Packard; [notmuch] [PATCH 1/2] Close message file after parsing message headers (inbox unread)
thread:XXX   2009-11-18 [2/2] Keith Packard, Alexander Botero-Lowry; [notmuch] [PATCH] Create a default notmuch-show-hook that highlights URLs and uses word-wrap (inbox unread)
thread:XXX   2009-11-18 [1/1] Alexander Botero-Lowry; [notmuch] request for pull (inbox unread)
thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox unread)
thread:XXX   2009-11-18 [1/1] Rolland Santimano; [notmuch] Link to mailing list archives ? (inbox unread)
thread:XXX   2009-11-18 [1/1] Jan Janak; [notmuch] [PATCH] notmuch new: Support for conversion of spool subdirectories into tags (inbox unread)
thread:XXX   2009-11-18 [1/1] Stewart Smith; [notmuch] [PATCH] count_files: sort directory in inode order before statting (inbox unread)
thread:XXX   2009-11-18 [1/1] Stewart Smith; [notmuch] [PATCH 2/2] Read mail directory in inode number order (inbox unread)
thread:XXX   2009-11-18 [1/1] Stewart Smith; [notmuch] [PATCH] Fix linking with gcc to use g++ to link in C++ libs. (inbox unread)
thread:XXX   2009-11-18 [2/2] Lars Kellogg-Stedman; [notmuch] "notmuch help" outputs to stderr? (attachment inbox signed unread)
thread:XXX   2009-11-17 [1/1] Mikhail Gusarov; [notmuch] [PATCH] Handle rename of message file (inbox unread)
thread:XXX   2009-11-17 [2/2] Alex Botero-Lowry, Carl Worth; [notmuch] preliminary FreeBSD support (attachment inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; body search (inbox unread)
thread:XXX   2000-01-01 [1/1] searchbyfrom; search by from (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; subjectsearchtest (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-sübjéct (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by id (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by mid (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by tag (inbox searchbytag unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by thread (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; body search (phrase) (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; negative result (inbox unread)
thread:XXX   2000-01-01 [1/1] searchbyfrom@example.com; search by from (address) (inbox unread)
thread:XXX   2000-01-01 [1/1] Search By From Name; search by from (name) (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (address) (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (name) (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; subject search test (phrase) (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; this phrase should not match the subject search test (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Search body (utf-8):"
add_message '[subject]="utf8-message-body-subject"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' '[body]="message body utf8: bödý"'
output=$(notmuch search "bödý" | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-message-body-subject (inbox unread)"


cat <<EOF > ${MAIL_DIR}/termpos
From: Source <source@example.com>
To: Dest <dest@example.com>
Subject: part overlap test
Date: Sat, 01 January 2000 00:00:00 +0000
Message-ID: <termpos>
MIME-Version: 1.0
Content-Type: multipart/mixed; boundary="==-=="

--==-==
Content-Type: text/plain

a b c

--==-==
Content-Type: text/plain

x y z

--==-==--
EOF
notmuch new > /dev/null

test_begin_subtest "headers do not have adjacent term positions"
# Regression test for a bug where term positions for non-prefixed
# terms weren't updated
output=$(notmuch search id:termpos and '"com dest"')
test_expect_equal "$output" ""

test_begin_subtest "parts have non-overlapping term positions"
output=$(notmuch search id:termpos and '"a y c"')
test_expect_equal "$output" ""

test_begin_subtest "parts do not have adjacent term positions"
output=$(notmuch search id:termpos and '"c x"')
test_expect_equal "$output" ""

test_done
