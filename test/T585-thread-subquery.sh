#!/usr/bin/env bash
#
# Copyright (c) 2018 David Bremner
#

test_description='test of searching by using thread subqueries'

. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus

test_begin_subtest "Basic query that matches no messages"
count=$(notmuch count from:keithp and to:keithp)
test_expect_equal 0 "$count"

test_begin_subtest "Same query against threads"
notmuch search thread:{from:keithp} and thread:{to:keithp} | notmuch_search_sanitize > OUTPUT
cat<<EOF > EXPECTED
thread:XXX   2009-11-18 [7/7] Lars Kellogg-Stedman, Mikhail Gusarov, Keith Packard, Carl Worth; [notmuch] Working with Maildir storage? (inbox signed unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Mix thread and non-threads query"
notmuch search thread:{from:keithp} and to:keithp | notmuch_search_sanitize > OUTPUT
cat<<EOF > EXPECTED
thread:XXX   2009-11-18 [1/7] Lars Kellogg-Stedman| Mikhail Gusarov, Keith Packard, Carl Worth; [notmuch] Working with Maildir storage? (inbox signed unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Compound subquery"
notmuch search 'thread:"{from:keithp and date:2009}" and thread:{to:keithp}' | notmuch_search_sanitize > OUTPUT
cat<<EOF > EXPECTED
thread:XXX   2009-11-18 [7/7] Lars Kellogg-Stedman, Mikhail Gusarov, Keith Packard, Carl Worth; [notmuch] Working with Maildir storage? (inbox signed unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Syntax/quoting error in subquery"
notmuch search 'thread:{from:keithp and date:2009} and thread:{to:keithp}' 1>OUTPUT 2>&1
cat<<EOF > EXPECTED
notmuch search: A Xapian exception occurred
A Xapian exception occurred parsing query: missing } in '{from:keithp'
Query string was: thread:{from:keithp and date:2009} and thread:{to:keithp}
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
