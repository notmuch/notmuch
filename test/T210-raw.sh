#!/usr/bin/env bash

test_description='notmuch show --format=raw'
. ./test-lib.sh || exit 1

add_message
add_message

test_begin_subtest "Attempt to show multiple raw messages"
output=$(notmuch show --format=raw "*" 2>&1)
test_expect_equal "$output" "Error: search term did not match precisely one message (matched 2 messages)."

test_begin_subtest "Show a raw message"
output=$(notmuch show --format=raw id:msg-001@notmuch-test-suite | notmuch_date_sanitize)
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Message-Id: <msg-001@notmuch-test-suite>
Subject: Test message #1
Date: GENERATED_DATE

This is just a test message (#1)"

test_begin_subtest "Show another raw message"
output=$(notmuch show --format=raw id:msg-002@notmuch-test-suite | notmuch_date_sanitize)
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Message-Id: <msg-002@notmuch-test-suite>
Subject: Test message #2
Date: GENERATED_DATE

This is just a test message (#2)"

test_done
