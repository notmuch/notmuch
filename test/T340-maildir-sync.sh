#!/usr/bin/env bash

test_description="maildir synchronization"

. ./test-lib.sh || exit 1

# Create the expected maildir structure
mkdir $MAIL_DIR/cur
mkdir $MAIL_DIR/new
mkdir $MAIL_DIR/tmp

test_begin_subtest "Adding 'S' flag to existing filename removes 'unread' tag"
add_message [subject]='"Adding S flag"' [filename]='adding-s-flag:2,' [dir]=cur
output=$(notmuch search subject:"Adding S flag" | notmuch_search_sanitize)
output+="
"
mv "${gen_msg_filename}" "${gen_msg_filename}S"
output+=$(NOTMUCH_NEW)
output+="
"
output+=$(notmuch search subject:"Adding S flag" | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Adding S flag (inbox unread)
No new mail. Detected 1 file rename.
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Adding S flag (inbox)"

test_begin_subtest "Adding message with 'S' flag prevents 'unread' tag"
add_message [subject]='"Adding message with S"' [filename]='adding-with-s-flag:2,S' [dir]=cur
output=$(notmuch search subject:"Adding message with S" | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Adding message with S (inbox)"

test_begin_subtest "Adding 'replied' tag adds 'R' flag to filename"
add_message [subject]='"Adding replied tag"' [filename]='adding-replied-tag:2,S' [dir]=cur
notmuch tag +replied subject:"Adding replied tag"
output=$(cd ${MAIL_DIR}/cur; ls -1 adding-replied*)
test_expect_equal "$output" "adding-replied-tag:2,RS"

test_begin_subtest "notmuch show works with renamed file (without notmuch new)"
output=$(notmuch show --format=json id:${gen_msg_id} | notmuch_json_show_sanitize)
test_expect_equal_json "$output" '[[[{"id": "XXXXX",
"match": true,
"excluded": false,
"filename": ["YYYYY"],
"timestamp": 42,
"date_relative": "2001-01-05",
"tags": ["inbox","replied"],
"headers": {"Subject": "Adding replied tag",
"From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
"To": "Notmuch Test Suite <test_suite@notmuchmail.org>",
"Date": "GENERATED_DATE"},
"body": [{"id": 1,
"content-type": "text/plain",
"content": "This is just a test message (#3)\n"}]},
[]]]]'

test_begin_subtest "notmuch reply works with renamed file (without notmuch new)"
test_expect_success 'notmuch reply id:${gen_msg_id}'

test_begin_subtest "notmuch new detects no file rename after tag->flag synchronization"
output=$(NOTMUCH_NEW)
test_expect_equal "$output" "No new mail."

test_begin_subtest "When read, message moved from new to cur"
add_message [subject]='"Message to move to cur"' [date]='"Sat, 01 Jan 2000 12:00:00 -0000"' [filename]='message-to-move-to-cur' [dir]=new
notmuch tag -unread subject:"Message to move to cur"
output=$(cd "$MAIL_DIR/cur"; ls message-to-move*)
test_expect_equal "$output" "message-to-move-to-cur:2,S"

test_begin_subtest "No rename should be detected by notmuch new"
output=$(NOTMUCH_NEW)
test_expect_equal "$output" "No new mail."
# (*) If notmuch new was not run we've got "Processed 1 file in almost
# no time" here. The reason is that removing unread tag in a previous
# test created directory document in the database but this document
# was not linked as subdirectory of $MAIL_DIR. Therefore notmuch new
# could not reach the cur/ directory and its files in it during
# recursive traversal.
#
# XXX: The above sounds like a bug that should be fixed. If notmuch is
# creating new directories in the mail store, then it should be
# creating all necessary database state for those directories.

test_begin_subtest "Adding non-maildir tags does not move message from new to cur"
add_message [subject]='"Message to stay in new"' \
    [date]='"Sat, 01 Jan 2000 12:00:00 -0000"' \
    [filename]='message-to-stay-in-new' [dir]=new
notmuch tag +donotmove subject:"Message to stay in new"
output=$(cd "$MAIL_DIR"; ls */message-to-stay-in-new*)
test_expect_equal "$output" "new/message-to-stay-in-new"

test_begin_subtest "Message in cur lacking maildir info gets one on any tag change"
add_message [filename]='message-to-get-maildir-info' [dir]=cur
notmuch tag +anytag id:$gen_msg_id
output=$(cd "$MAIL_DIR"; ls */message-to-get-maildir-info*)
test_expect_equal "$output" "cur/message-to-get-maildir-info:2,"

test_begin_subtest "Message in new with maildir info is moved to cur on any tag change"
add_message [filename]='message-with-info-to-be-moved-to-cur:2,' [dir]=new
notmuch tag +anytag id:$gen_msg_id
output=$(cd "$MAIL_DIR"; ls */message-with-info-to-be-moved-to-cur*)
test_expect_equal "$output" "cur/message-with-info-to-be-moved-to-cur:2,"

test_begin_subtest "Removing 'S' flag from existing filename adds 'unread' tag"
add_message [subject]='"Removing S flag"' [filename]='removing-s-flag:2,S' [dir]=cur
output=$(notmuch search subject:"Removing S flag" | notmuch_search_sanitize)
output+="
"
mv "${gen_msg_filename}" "${gen_msg_filename%S}"
output+=$(NOTMUCH_NEW)
output+="
"
output+=$(notmuch search subject:"Removing S flag" | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Removing S flag (inbox)
No new mail. Detected 1 file rename.
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Removing S flag (inbox unread)"

test_begin_subtest "Removing info from filename leaves tags unchanged"
add_message [subject]='"Message to lose maildir info"' [filename]='message-to-lose-maildir-info' [dir]=cur
notmuch tag -unread subject:"Message to lose maildir info"
mv "$MAIL_DIR/cur/message-to-lose-maildir-info:2,S" "$MAIL_DIR/cur/message-without-maildir-info"
output=$(NOTMUCH_NEW)
output+="
"
output+=$(notmuch search subject:"Message to lose maildir info" | notmuch_search_sanitize)
test_expect_equal "$output" "No new mail. Detected 1 file rename.
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Message to lose maildir info (inbox)"

test_begin_subtest "Can remove unread tag from message in non-maildir directory"
add_message [subject]='"Non-maildir message"' [dir]=notmaildir [filename]='non-maildir-message'
expected=$(notmuch search --output=files subject:"Non-maildir message")
test_expect_success 'notmuch tag -unread subject:"Non-maildir message"'

test_begin_subtest "Message in non-maildir directory does not get renamed"
output=$(notmuch search --output=files subject:"Non-maildir message")
test_expect_equal "$output" "$expected"

test_begin_subtest "notmuch dump/restore re-synchronizes maildir tags with flags"
# Capture current filename state
expected=$(ls $MAIL_DIR/cur)
# Add/remove some flags from filenames
mv $MAIL_DIR/cur/adding-replied-tag:2,RS $MAIL_DIR/cur/adding-replied-tag:2,S
mv $MAIL_DIR/cur/adding-s-flag:2,S $MAIL_DIR/cur/adding-s-flag:2,
mv $MAIL_DIR/cur/adding-with-s-flag:2,S $MAIL_DIR/cur/adding-with-s-flag:2,RS
mv $MAIL_DIR/cur/message-to-move-to-cur:2,S $MAIL_DIR/cur/message-to-move-to-cur:2,DS
notmuch dump --output=dump.txt
NOTMUCH_NEW >/dev/null
notmuch restore --input=dump.txt
output=$(ls $MAIL_DIR/cur)
test_expect_equal "$output" "$expected"

test_begin_subtest 'Adding flags to duplicate message tags the mail'
add_message [subject]='"Duplicated message"' [dir]=cur [filename]='duplicated-message:2,'
cp "$MAIL_DIR/cur/duplicated-message:2," "$MAIL_DIR/cur/duplicated-message-copy:2,RS"
NOTMUCH_NEW > output
notmuch search subject:"Duplicated message" | notmuch_search_sanitize >> output
test_expect_equal "$(< output)" "No new mail.
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Duplicated message (inbox replied)"

test_begin_subtest "Adding duplicate message without flags does not remove tags"
cp "$MAIL_DIR/cur/duplicated-message-copy:2,RS" "$MAIL_DIR/cur/duplicated-message-another-copy:2,"
NOTMUCH_NEW > output
notmuch search subject:"Duplicated message" | notmuch_search_sanitize >> output
test_expect_equal "$(< output)" "No new mail.
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Duplicated message (inbox replied)"

test_begin_subtest "Tag changes modify flags of multiple files"
notmuch tag -replied subject:"Duplicated message"
(cd $MAIL_DIR/cur/; ls duplicated*) > actual
test_expect_equal "$(< actual)"  "duplicated-message-another-copy:2,S
duplicated-message-copy:2,S
duplicated-message:2,S"

test_begin_subtest "Synchronizing tag changes preserves unsupported maildir flags"
add_message [subject]='"Unsupported maildir flags"' [dir]=cur [filename]='unsupported-maildir-flags:2,FSZxyz'
notmuch tag +unread +draft -flagged subject:"Unsupported maildir flags"
test_expect_equal "$(cd $MAIL_DIR/cur/; ls unsupported*)" "unsupported-maildir-flags:2,DZxyz"

test_begin_subtest "A file with non-compliant maildir info will not be renamed"
add_message [subject]='"Non-compliant maildir info"' [dir]=cur [filename]='non-compliant-maildir-info:2,These-are-not-flags-in-ASCII-order-donottouch'
notmuch tag +unread +draft -flagged subject:"Non-compliant maildir info"
test_expect_equal "$(cd $MAIL_DIR/cur/; ls non-compliant*)" "non-compliant-maildir-info:2,These-are-not-flags-in-ASCII-order-donottouch"

test_begin_subtest "Files in new/ get default synchronized tags"
OLDCONFIG=$(notmuch config get new.tags)
notmuch config set new.tags test
add_message [subject]='"File in new/"' [dir]=new [filename]='file-in-new'
notmuch config set new.tags $OLDCONFIG
notmuch search 'subject:"File in new"' | notmuch_search_sanitize > output
test_expect_equal "$(< output)" \
"thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; File in new/ (test unread)"

test_done
