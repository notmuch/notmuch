#!/usr/bin/env bash
test_description="indexing of html parts"
. ./test-lib.sh || exit 1

add_email_corpus html

test_begin_subtest 'embedded images should not be indexed'
notmuch search kwpza7svrgjzqwi8fhb2msggwtxtwgqcxp4wbqr4wjddstqmeqa7 > OUTPUT
test_expect_equal_file /dev/null OUTPUT

test_begin_subtest 'ignore > in attribute text'
notmuch search swordfish | notmuch_search_sanitize > OUTPUT
test_expect_equal_file /dev/null OUTPUT

test_begin_subtest 'non tag text should be indexed'
notmuch search hunter2 | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2009-11-17 [1/1] David Bremner; test html attachment (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
