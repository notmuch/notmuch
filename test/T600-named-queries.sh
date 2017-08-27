#!/usr/bin/env bash
test_description='named queries'
. ./test-lib.sh || exit 1

QUERYSTR="date:2009-11-18..2009-11-18 and tag:unread"

test_begin_subtest "error adding named query before initializing DB"
test_expect_code 1 "notmuch config set query.test \"$QUERYSTR\""

add_email_corpus

test_begin_subtest "adding named query"
test_expect_success "notmuch config set query.test \"$QUERYSTR\""

test_begin_subtest "adding nested named query"
QUERYSTR2="query:test and subject:Maildir"
test_expect_success "notmuch config set query.test2 \"$QUERYSTR2\""

test_begin_subtest "retrieve named query"
output=$(notmuch config get query.test)
test_expect_equal "$QUERYSTR" "$output"

test_begin_subtest "List all queries"
notmuch config list | grep ^query | notmuch_config_sanitize > OUTPUT
cat <<EOF > EXPECTED
query.test=date:2009-11-18..2009-11-18 and tag:unread
query.test2=query:test and subject:Maildir
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "dump named queries"
notmuch dump | grep '^#@' > OUTPUT
cat<<EOF > QUERIES.BEFORE
#@ query.test date%3a2009-11-18..2009-11-18%20and%20tag%3aunread
#@ query.test2 query%3atest%20and%20subject%3aMaildir
EOF
test_expect_equal_file QUERIES.BEFORE OUTPUT

test_begin_subtest "delete named queries"
notmuch dump > BEFORE
notmuch config set query.test
notmuch dump | grep '^#@' > OUTPUT
cat<<EOF > EXPECTED
#@ query.test2 query%3atest%20and%20subject%3aMaildir
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "restore named queries"
notmuch restore < BEFORE
notmuch dump | grep '^#@' > OUTPUT
test_expect_equal_file QUERIES.BEFORE OUTPUT

if [ $NOTMUCH_HAVE_XAPIAN_FIELD_PROCESSOR -eq 1 ]; then
    test_begin_subtest "search named query"
    notmuch search query:test > OUTPUT
    notmuch search $QUERYSTR > EXPECTED
    test_expect_equal_file EXPECTED OUTPUT

    test_begin_subtest "search named query with other terms"
    notmuch search query:test and subject:Maildir > OUTPUT
    notmuch search $QUERYSTR and subject:Maildir > EXPECTED
    test_expect_equal_file EXPECTED OUTPUT

    test_begin_subtest "search nested named query"
    notmuch search query:test2 > OUTPUT
    notmuch search $QUERYSTR2 > EXPECTED
    test_expect_equal_file EXPECTED OUTPUT
fi

test_done
