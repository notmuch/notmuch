#!/usr/bin/env bash
test_description='search body'
. $(dirname "$0")/test-lib.sh || exit 1

add_message "[body]=thebody-1" "[subject]=subject-1"
add_message "[body]=nothing-to-see-here-1" "[subject]=thebody-1"

test_begin_subtest 'search with body: prefix'
notmuch search body:thebody | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; subject-1 (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'search without body: prefix'
notmuch search thebody | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; subject-1 (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; thebody-1 (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'negated body: prefix'
notmuch search thebody and not body:thebody | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; thebody-1 (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'search unprefixed for prefixed term'
notmuch search subject | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; subject-1 (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'search with body: prefix for term only in subject'
notmuch search body:subject | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
