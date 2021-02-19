#!/usr/bin/env bash
test_description='named queries'
. $(dirname "$0")/test-lib.sh || exit 1

QUERYSTR="date:2009-11-18..2009-11-18 and tag:unread"

test_begin_subtest "error adding named query to DB before initialization"
test_expect_code 1 "notmuch config set --database query.test \"$QUERYSTR\""

add_email_corpus

test_begin_subtest "adding named query (database)"
test_expect_success "notmuch config set --database query.test \"$QUERYSTR\""

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
EOF
test_expect_equal_file QUERIES.BEFORE OUTPUT

test_begin_subtest 'dumping large queries'
# This value is just large enough to trigger a limitation of gzprintf
# to 8191 bytes in total (by default).
repeat=1329
notmuch config set --database query.big "$(seq -s' ' $repeat)"
notmuch dump --include=config > OUTPUT
notmuch config set --database query.big
printf "#notmuch-dump batch-tag:3 config\n#@ query.big " > EXPECTED
seq -s'%20' $repeat >> EXPECTED
cat <<EOF >> EXPECTED
#@ query.test date%3a2009-11-18..2009-11-18%20and%20tag%3aunread
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "delete named queries"
notmuch dump > BEFORE
notmuch config set --database query.test
notmuch dump | grep '^#@' > OUTPUT
cat<<EOF > EXPECTED
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "restore named queries"
notmuch restore < BEFORE
notmuch dump | grep '^#@' > OUTPUT
test_expect_equal_file QUERIES.BEFORE OUTPUT

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

test_done
