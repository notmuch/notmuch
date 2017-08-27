#!/usr/bin/env bash
test_description="naming of threads with changing subject"
. ./test-lib.sh || exit 1

test_begin_subtest "Initial thread name (oldest-first search)"
add_message '[subject]="thread-naming: Initial thread subject"' \
	    '[date]="Fri, 05 Jan 2001 15:43:56 -0000"'
first=${gen_msg_cnt}
parent=${gen_msg_id}
add_message '[subject]="thread-naming: Older changed subject"' \
	    '[date]="Sat, 06 Jan 2001 15:43:56 -0000"' \
	    "[in-reply-to]=\<$parent\>"
add_message '[subject]="thread-naming: Newer changed subject"' \
	    '[date]="Sun, 07 Jan 2001 15:43:56 -0000"' \
	    "[in-reply-to]=\<$parent\>"
add_message '[subject]="thread-naming: Final thread subject"' \
	    '[date]="Mon, 08 Jan 2001 15:43:56 -0000"' \
	    "[in-reply-to]=\<$parent\>"
final=${gen_msg_id}
output=$(notmuch search --sort=oldest-first thread-naming and tag:inbox | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [4/4] Notmuch Test Suite; thread-naming: Initial thread subject (inbox unread)"

test_begin_subtest "Initial thread name (newest-first search)"
output=$(notmuch search --sort=newest-first thread-naming and tag:inbox | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-08 [4/4] Notmuch Test Suite; thread-naming: Final thread subject (inbox unread)"

# Remove oldest and newest messages from search results
notmuch tag -inbox id:$parent or id:$final

test_begin_subtest "Changed thread name (oldest-first search)"
output=$(notmuch search --sort=oldest-first thread-naming and tag:inbox | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-06 [2/4] Notmuch Test Suite; thread-naming: Older changed subject (inbox unread)"

test_begin_subtest "Changed thread name (newest-first search)"
output=$(notmuch search --sort=newest-first thread-naming and tag:inbox | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-07 [2/4] Notmuch Test Suite; thread-naming: Newer changed subject (inbox unread)"

test_begin_subtest "Ignore added reply prefix (Re:)"
add_message '[subject]="Re: thread-naming: Initial thread subject"' \
	    '[date]="Tue, 09 Jan 2001 15:43:45 -0000"' \
	    "[in-reply-to]=\<$parent\>"
output=$(notmuch search --sort=newest-first thread-naming and tag:inbox | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-09 [3/5] Notmuch Test Suite; thread-naming: Initial thread subject (inbox unread)"

test_begin_subtest "Ignore added reply prefix (Aw:)"
add_message '[subject]="Aw: thread-naming: Initial thread subject"' \
	    '[date]="Wed, 10 Jan 2001 15:43:45 -0000"' \
	    "[in-reply-to]=\<$parent\>"
output=$(notmuch search --sort=newest-first thread-naming and tag:inbox | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-10 [4/6] Notmuch Test Suite; thread-naming: Initial thread subject (inbox unread)"

test_begin_subtest "Ignore added reply prefix (Vs:)"
add_message '[subject]="Vs: thread-naming: Initial thread subject"' \
	    '[date]="Thu, 11 Jan 2001 15:43:45 -0000"' \
	    "[in-reply-to]=\<$parent\>"
output=$(notmuch search --sort=newest-first thread-naming and tag:inbox | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-11 [5/7] Notmuch Test Suite; thread-naming: Initial thread subject (inbox unread)"

test_begin_subtest "Ignore added reply prefix (Sv:)"
add_message '[subject]="Sv: thread-naming: Initial thread subject"' \
	    '[date]="Fri, 12 Jan 2001 15:43:45 -0000"' \
	    "[in-reply-to]=\<$parent\>"
output=$(notmuch search --sort=newest-first thread-naming and tag:inbox | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-12 [6/8] Notmuch Test Suite; thread-naming: Initial thread subject (inbox unread)"

test_begin_subtest "Use empty subjects if necessary."
add_message '[subject]="@FORCE_EMPTY"' \
	    '[date]="Sat, 13 Jan 2001 15:43:45 -0000"' \
            '[from]="Empty Sender <empty_test@notmuchmail.org>"'
empty_parent=${gen_msg_id}
add_message '[subject]="@FORCE_EMPTY"' \
	    '[date]="Sun, 14 Jan 2001 15:43:45 -0000"' \
            '[from]="Empty Sender <empty_test@notmuchmail.org>"' \
            "[in-reply-to]=\<$empty_parent\>"
output=$(notmuch search --sort=newest-first from:empty_test@notmuchmail.org | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-14 [2/2] Empty Sender;  (inbox unread)"

test_begin_subtest "Avoid empty subjects if possible (newest-first)."
add_message '[subject]="Non-empty subject (1)"' \
	    '[date]="Mon, 15 Jan 2001 15:43:45 -0000"' \
            '[from]="Empty Sender <empty_test@notmuchmail.org>"' \
            "[in-reply-to]=\<$empty_parent\>"
add_message '[subject]="Non-empty subject (2)"' \
	    '[date]="Mon, 16 Jan 2001 15:43:45 -0000"' \
            '[from]="Empty Sender <empty_test@notmuchmail.org>"' \
            "[in-reply-to]=\<$empty_parent\>"
add_message '[subject]="@FORCE_EMPTY"' \
	    '[date]="Tue, 17 Jan 2001 15:43:45 -0000"' \
            '[from]="Empty Sender <empty_test@notmuchmail.org>"' \
            "[in-reply-to]=\<$empty_parent\>"
output=$(notmuch search --sort=newest-first from:Empty | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-17 [5/5] Empty Sender; Non-empty subject (2) (inbox unread)"

test_begin_subtest "Avoid empty subjects if possible (oldest-first)."
output=$(notmuch search --sort=oldest-first from:Empty | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-13 [5/5] Empty Sender; Non-empty subject (1) (inbox unread)"

test_begin_subtest 'Test order of messages in "notmuch show"'
output=$(notmuch show thread-naming | notmuch_show_sanitize)
test_expect_equal "$output" "message{ id:msg-$(printf "%03d" $first)@notmuch-test-suite depth:0 match:1 excluded:0 filename:/XXX/mail/msg-$(printf "%03d" $first)
header{
Notmuch Test Suite <test_suite@notmuchmail.org> (2001-01-05) (unread)
Subject: thread-naming: Initial thread subject
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: Fri, 05 Jan 2001 15:43:56 +0000
header}
body{
part{ ID: 1, Content-type: text/plain
This is just a test message (#$first)
part}
body}
message}
message{ id:msg-$(printf "%03d" $((first + 1)))@notmuch-test-suite depth:1 match:1 excluded:0 filename:/XXX/mail/msg-$(printf "%03d" $((first + 1)))
header{
Notmuch Test Suite <test_suite@notmuchmail.org> (2001-01-06) (inbox unread)
Subject: thread-naming: Older changed subject
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: Sat, 06 Jan 2001 15:43:56 +0000
header}
body{
part{ ID: 1, Content-type: text/plain
This is just a test message (#$((first + 1)))
part}
body}
message}
message{ id:msg-$(printf "%03d" $((first + 2)))@notmuch-test-suite depth:1 match:1 excluded:0 filename:/XXX/mail/msg-$(printf "%03d" $((first + 2)))
header{
Notmuch Test Suite <test_suite@notmuchmail.org> (2001-01-07) (inbox unread)
Subject: thread-naming: Newer changed subject
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: Sun, 07 Jan 2001 15:43:56 +0000
header}
body{
part{ ID: 1, Content-type: text/plain
This is just a test message (#$((first + 2)))
part}
body}
message}
message{ id:msg-$(printf "%03d" $((first + 3)))@notmuch-test-suite depth:1 match:1 excluded:0 filename:/XXX/mail/msg-$(printf "%03d" $((first + 3)))
header{
Notmuch Test Suite <test_suite@notmuchmail.org> (2001-01-08) (unread)
Subject: thread-naming: Final thread subject
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: Mon, 08 Jan 2001 15:43:56 +0000
header}
body{
part{ ID: 1, Content-type: text/plain
This is just a test message (#$((first + 3)))
part}
body}
message}
message{ id:msg-$(printf "%03d" $((first + 4)))@notmuch-test-suite depth:1 match:1 excluded:0 filename:/XXX/mail/msg-$(printf "%03d" $((first + 4)))
header{
Notmuch Test Suite <test_suite@notmuchmail.org> (2001-01-09) (inbox unread)
Subject: Re: thread-naming: Initial thread subject
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: Tue, 09 Jan 2001 15:43:45 +0000
header}
body{
part{ ID: 1, Content-type: text/plain
This is just a test message (#$((first + 4)))
part}
body}
message}
message{ id:msg-$(printf "%03d" $((first + 5)))@notmuch-test-suite depth:1 match:1 excluded:0 filename:/XXX/mail/msg-$(printf "%03d" $((first + 5)))
header{
Notmuch Test Suite <test_suite@notmuchmail.org> (2001-01-10) (inbox unread)
Subject: Aw: thread-naming: Initial thread subject
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: Wed, 10 Jan 2001 15:43:45 +0000
header}
body{
part{ ID: 1, Content-type: text/plain
This is just a test message (#$((first + 5)))
part}
body}
message}
message{ id:msg-$(printf "%03d" $((first + 6)))@notmuch-test-suite depth:1 match:1 excluded:0 filename:/XXX/mail/msg-$(printf "%03d" $((first + 6)))
header{
Notmuch Test Suite <test_suite@notmuchmail.org> (2001-01-11) (inbox unread)
Subject: Vs: thread-naming: Initial thread subject
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: Thu, 11 Jan 2001 15:43:45 +0000
header}
body{
part{ ID: 1, Content-type: text/plain
This is just a test message (#$((first + 6)))
part}
body}
message}
message{ id:msg-$(printf "%03d" $((first + 7)))@notmuch-test-suite depth:1 match:1 excluded:0 filename:/XXX/mail/msg-$(printf "%03d" $((first + 7)))
header{
Notmuch Test Suite <test_suite@notmuchmail.org> (2001-01-12) (inbox unread)
Subject: Sv: thread-naming: Initial thread subject
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: Fri, 12 Jan 2001 15:43:45 +0000
header}
body{
part{ ID: 1, Content-type: text/plain
This is just a test message (#$((first + 7)))
part}
body}
message}"
test_done
