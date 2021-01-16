#!/usr/bin/env bash

test_description='notmuch show --format=raw'
. $(dirname "$0")/test-lib.sh || exit 1

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

test_python <<EOF
from email.message import EmailMessage
for pow in range(10,21):
    size = 2 ** pow
    msg = EmailMessage()
    msg['Subject'] = 'message with {:07d} bytes'.format(size)
    msg['From'] = 'Notmuch Test Suite <test_suite@notmuchmail.org>'
    msg['To'] = msg['From']
    msg['Message-Id'] = 'size-{:07d}@notmuch-test-suite'.format(size)
    content = ""
    msg.set_content("\n")
    padding = size - len(bytes(msg))
    lines = []
    while padding > 0:
        line = '.' * min(padding, 72)
        lines.append(line)
        padding = padding - len(line) - 1
    content ='\n'.join(lines)
    msg.set_content(content)
    with open('mail/size-{:07d}'.format(size), 'wb') as f:
        f.write(bytes(msg))
EOF

notmuch new --quiet

for pow in {10..20}; do
    printf -v size "%07d" $((2**$pow))
    test_begin_subtest "content, message of size $size"
    notmuch show --format=raw subject:$size > OUTPUT
    test_expect_equal_file mail/size-$size OUTPUT
    test_begin_subtest "return value, message of size $size"
    test_expect_success "notmuch show --format=raw subject:$size > /dev/null"
done

test_done
