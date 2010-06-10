#!/bin/bash
test_description="Testing encoding issues"
. ./test-lib.sh

test_expect_success "Message with text of unknown charset" '
add_message "[content-type]=\"text/plain; charset=unknown-8bit\"" \
            "[body]=irrelevant" &&
output=$($NOTMUCH show id:${gen_msg_id} 2>&1 | notmuch_show_sanitize) &&
pass_if_equal "$output" "message{ id:msg-001@notmuch-test-suite depth:0 match:1 filename:/XXX/mail/msg-001
header{
Notmuch Test Suite <test_suite@notmuchmail.org> (2001-01-05) (inbox unread)
Subject: Test message #1
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: Tue, 05 Jan 2001 15:43:57 -0800
header}
body{
part{ ID: 1, Content-type: text/plain
irrelevant
part}
body}
message}"
'

test_done
