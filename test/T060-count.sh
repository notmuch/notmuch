#!/usr/bin/env bash
test_description='"notmuch count" for messages and threads'
. ./test-lib.sh || exit 1

add_email_corpus

# Note: The 'wc -l' results below are wrapped in arithmetic evaluation
# $((...)) to strip whitespace. This is for portability, as 'wc -l'
# emits whitespace on some BSD variants.

test_begin_subtest "message count is the default for notmuch count"
test_expect_equal \
    "$((`notmuch search --output=messages '*' | wc -l`))" \
    "`notmuch count '*'`"

test_begin_subtest "message count with --output=messages"
test_expect_equal \
    "$((`notmuch search --output=messages '*' | wc -l`))" \
    "`notmuch count --output=messages '*'`"

test_begin_subtest "thread count with --output=threads"
test_expect_equal \
    "$((`notmuch search --output=threads '*' | wc -l`))" \
    "`notmuch count --output=threads '*'`"

test_begin_subtest "thread count is the default for notmuch search"
test_expect_equal \
    "$((`notmuch search '*' | wc -l`))" \
    "`notmuch count --output=threads '*'`"

test_begin_subtest "files count"
test_expect_equal \
    "$((`notmuch search --output=files '*' | wc -l`))" \
    "`notmuch count --output=files '*'`"

test_begin_subtest "files count for a duplicate message-id"
test_expect_equal \
    "2" \
    "`notmuch count --output=files id:20091117232137.GA7669@griffis1.net`"

test_begin_subtest "count with no matching messages"
test_expect_equal \
    "0" \
    "`notmuch count --output=messages from:cworth and not from:cworth`"

test_begin_subtest "count with no matching threads"
test_expect_equal \
    "0" \
    "`notmuch count --output=threads from:cworth and not from:cworth`"

test_begin_subtest "message count is the default for batch count"
notmuch count --batch >OUTPUT <<EOF

from:cworth
EOF
notmuch count --output=messages >EXPECTED
notmuch count --output=messages from:cworth >>EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "batch message count"
notmuch count --batch --output=messages >OUTPUT <<EOF
from:cworth

tag:inbox
EOF
notmuch count --output=messages from:cworth >EXPECTED
notmuch count --output=messages >>EXPECTED
notmuch count --output=messages tag:inbox >>EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "batch thread count"
notmuch count --batch --output=threads >OUTPUT <<EOF

from:cworth
from:cworth and not from:cworth
foo
EOF
notmuch count --output=threads >EXPECTED
notmuch count --output=threads from:cworth >>EXPECTED
notmuch count --output=threads from:cworth and not from:cworth >>EXPECTED
notmuch count --output=threads foo >>EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "batch message count with input file"
cat >INPUT <<EOF
from:cworth

tag:inbox
EOF
notmuch count --input=INPUT --output=messages >OUTPUT
notmuch count --output=messages from:cworth >EXPECTED
notmuch count --output=messages >>EXPECTED
notmuch count --output=messages tag:inbox >>EXPECTED
test_expect_equal_file EXPECTED OUTPUT

backup_database
test_begin_subtest "error message for database open"
dd if=/dev/zero of="${MAIL_DIR}/.notmuch/xapian/postlist.${db_ending}" count=3
notmuch count '*' 2>OUTPUT 1>/dev/null
output=$(sed 's/^\(A Xapian exception [^:]*\):.*$/\1/' OUTPUT)
test_expect_equal "${output}" "A Xapian exception occurred opening database"
restore_database

cat <<EOF > count-files.gdb
set breakpoint pending on
set logging file count-files-gdb.log
set logging on
break count_files
commands
shell cp /dev/null ${MAIL_DIR}/.notmuch/xapian/postlist.${db_ending}
continue
end
run
EOF

backup_database
test_begin_subtest "error message from query_search_messages"
${TEST_GDB} --batch-silent --return-child-result -x count-files.gdb \
    --args notmuch count --output=files '*' 2>OUTPUT 1>/dev/null
cat <<EOF > EXPECTED
notmuch count: A Xapian exception occurred
A Xapian exception occurred performing query
Query string was: *
EOF
sed 's/^\(A Xapian exception [^:]*\):.*$/\1/' < OUTPUT > OUTPUT.clean
test_expect_equal_file EXPECTED OUTPUT.clean
restore_database

test_begin_subtest "count library function is non-destructive"
cat<<EOF > EXPECTED
1: 52 messages
2: 52 messages
Exclude 'spam'
3: 52 messages
4: 52 messages
EOF
test_python <<EOF
import sys
import notmuch

query_string = 'tag:inbox or tag:spam'
tag_string = 'spam'

database = notmuch.Database(mode=notmuch.Database.MODE.READ_ONLY)
query = notmuch.Query(database, query_string)

print("1: {} messages".format(query.count_messages()))
print("2: {} messages".format(query.count_messages()))
print("Exclude '{}'".format(tag_string))
query.exclude_tag(tag_string)
print("3: {} messages".format(query.count_messages()))
print("4: {} messages".format(query.count_messages()))
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
