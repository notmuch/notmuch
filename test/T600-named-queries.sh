#!/usr/bin/env bash
test_description='named queries'
. ./test-lib.sh || exit 1

QUERYSTR="date:2009-11-18..2009-11-18 and tag:unread"

test_expect_code 1 "error adding named query before initializing DB" \
		 "notmuch config set query.test \"$QUERYSTR\""

add_email_corpus

test_expect_success "adding named query" \
		    "notmuch config set query.test \"$QUERYSTR\""

QUERYSTR2="query:test and subject:Maildir"
test_expect_success "adding nested named query" \
		    "notmuch config set query.test2 \"$QUERYSTR2\""

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

test_done
