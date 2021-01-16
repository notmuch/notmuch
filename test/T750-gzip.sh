#!/usr/bin/env bash
test_description='support for gzipped messages'
. $(dirname "$0")/test-lib.sh || exit 1

#######################################################################
# notmuch new
test_begin_subtest "Single new gzipped message"
generate_message
gzip $gen_msg_filename
output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "Added 1 new message to the database."

test_begin_subtest "Single new gzipped message (full-scan)"
generate_message
gzip $gen_msg_filename
output=$(NOTMUCH_NEW --debug --full-scan 2>&1)
test_expect_equal "$output" "Added 1 new message to the database."

test_begin_subtest "Multiple new messages, one gzipped"
generate_message
gzip $gen_msg_filename
generate_message
output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "Added 2 new messages to the database."

test_begin_subtest "Multiple new messages, one gzipped (full-scan)"
generate_message
gzip $gen_msg_filename
generate_message
output=$(NOTMUCH_NEW --debug --full-scan 2>&1)
test_expect_equal "$output" "Added 2 new messages to the database."

test_begin_subtest "Renamed (gzipped) message"
generate_message
echo $gen_message_filename
notmuch new > /dev/null
gzip $gen_msg_filename
output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "(D) add_files, pass 2: queuing passed file ${gen_msg_filename} for deletion from database
No new mail. Detected 1 file rename."

######################################################################
# notmuch search

test_begin_subtest "notmuch search with partially gzipped mail store"
notmuch search '*' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Single new gzipped message (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Single new gzipped message (full-scan) (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Multiple new messages, one gzipped (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Multiple new messages, one gzipped (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Multiple new messages, one gzipped (full-scan) (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Multiple new messages, one gzipped (full-scan) (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Renamed (gzipped) message (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch search --output=files with partially gzipped mail store"
notmuch search --output=files '*' | notmuch_search_files_sanitize > OUTPUT
cat <<EOF > EXPECTED
MAIL_DIR/msg-001.gz
MAIL_DIR/msg-002.gz
MAIL_DIR/msg-003.gz
MAIL_DIR/msg-004
MAIL_DIR/msg-005.gz
MAIL_DIR/msg-006
MAIL_DIR/msg-007.gz
EOF
test_expect_equal_file EXPECTED OUTPUT

######################################################################
# notmuch show

test_begin_subtest "show un-gzipped message"
notmuch show id:msg-006@notmuch-test-suite | notmuch_show_sanitize > OUTPUT
cat <<EOF > EXPECTED
message{ id:msg-006@notmuch-test-suite depth:0 match:1 excluded:0 filename:/XXX/mail/msg-006
header{
Notmuch Test Suite <test_suite@notmuchmail.org> (2001-01-05) (inbox unread)
Subject: Multiple new messages, one gzipped (full-scan)
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: Fri, 05 Jan 2001 15:43:51 +0000
header}
body{
part{ ID: 1, Content-type: text/plain
This is just a test message (#6)
part}
body}
message}
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "show un-gzipped message (format mbox)"
notmuch show --format=mbox id:msg-006@notmuch-test-suite | notmuch_show_sanitize > OUTPUT
cat <<EOF > EXPECTED
From test_suite@notmuchmail.org Fri Jan  5 15:43:51 2001
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Message-Id: <msg-006@notmuch-test-suite>
Subject: Multiple new messages, one gzipped (full-scan)
Date: Fri, 05 Jan 2001 15:43:51 +0000

This is just a test message (#6)

EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "show un-gzipped message (format raw)"
notmuch show --format=raw id:msg-006@notmuch-test-suite | notmuch_show_sanitize > OUTPUT
cat <<EOF > EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Message-Id: <msg-006@notmuch-test-suite>
Subject: Multiple new messages, one gzipped (full-scan)
Date: Fri, 05 Jan 2001 15:43:51 +0000

This is just a test message (#6)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "show gzipped message"
notmuch show id:msg-007@notmuch-test-suite | notmuch_show_sanitize > OUTPUT
cat <<EOF > EXPECTED
message{ id:msg-007@notmuch-test-suite depth:0 match:1 excluded:0 filename:/XXX/mail/msg-007.gz
header{
Notmuch Test Suite <test_suite@notmuchmail.org> (2001-01-05) (inbox unread)
Subject: Renamed (gzipped) message
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: Fri, 05 Jan 2001 15:43:50 +0000
header}
body{
part{ ID: 1, Content-type: text/plain
This is just a test message (#7)
part}
body}
message}
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "show gzipped message (mbox)"
notmuch show --format=mbox id:msg-007@notmuch-test-suite | notmuch_show_sanitize > OUTPUT
cat <<EOF > EXPECTED
From test_suite@notmuchmail.org Fri Jan  5 15:43:50 2001
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Message-Id: <msg-007@notmuch-test-suite>
Subject: Renamed (gzipped) message
Date: Fri, 05 Jan 2001 15:43:50 +0000

This is just a test message (#7)

EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "show gzipped message (raw)"
notmuch show --format=raw id:msg-007@notmuch-test-suite | notmuch_show_sanitize > OUTPUT
cat <<EOF > EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Message-Id: <msg-007@notmuch-test-suite>
Subject: Renamed (gzipped) message
Date: Fri, 05 Jan 2001 15:43:50 +0000

This is just a test message (#7)
EOF
test_expect_equal_file EXPECTED OUTPUT

# there are more than 200 messages in this corpus
add_email_corpus lkml
test_begin_subtest "new doesn't run out of file descriptors with many gzipped files"
ulimit -n 200
find ${MAIL_DIR} -name .notmuch -prune -o -type f -print0 | xargs -0 gzip --
test_expect_success "notmuch new"

test_done
