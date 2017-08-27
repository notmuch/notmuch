#!/usr/bin/env bash
test_description="parsing of bad dates"
. ./test-lib.sh || exit 1

add_message [date]='"()"'

test_begin_subtest 'Bad dates translate to a date after the Unix epoch'
cat <<EOF >EXPECTED
thread:0000000000000001   1970-01-01 [1/1] Notmuch Test Suite; Test message #1 (inbox unread)
EOF
notmuch search '*' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_done
