#!/usr/bin/env bash

test_description="command line arguments"
. ./test-lib.sh || exit 1

add_message

test_begin_subtest 'bad option to show'
notmuch show --frobnicate >& OUTPUT
cat <<EOF > EXPECTED
Unrecognized option: --frobnicate
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'string option with space'
cp /dev/null EXPECTED
notmuch dump --output foo.txt '*' >& OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'string option with ='
cp /dev/null EXPECTED
notmuch dump --output=foo.txt '*' >& OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'string option with :'
cp /dev/null EXPECTED
notmuch dump --output:foo.txt '*' >& OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'single keyword option with space'
cat <<EOF > EXPECTED
id:msg-001@notmuch-test-suite
EOF
notmuch search --output messages '*' >& OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'single keyword option with ='
cat <<EOF > EXPECTED
id:msg-001@notmuch-test-suite
EOF
notmuch search --output=messages '*' >& OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'single keyword option with :'
cat <<EOF > EXPECTED
id:msg-001@notmuch-test-suite
EOF
notmuch search --output:messages '*' >& OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'multiple keyword options with space'
cat <<EOF > EXPECTED
["msg-001@notmuch-test-suite"]
EOF
notmuch search --output messages --format json '*' >& OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'multiple keyword options with ='
cat <<EOF > EXPECTED
["msg-001@notmuch-test-suite"]
EOF
notmuch search --output=messages --format=json '*' >& OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'mixed space and = delimiters'
cat <<EOF > EXPECTED
["msg-001@notmuch-test-suite"]
EOF
notmuch search --output messages --format=json '*' >& OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'mixed space and : delimiters'
cat <<EOF > EXPECTED
["msg-001@notmuch-test-suite"]
EOF
notmuch search --output:messages --format json '*' >& OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'show --entire-thread'
test_expect_success 'notmuch show --entire-thread tag:test > /dev/null'

test_begin_subtest 'show --exclude'
test_expect_success 'notmuch show --exclude tag:test > /dev/null'

test_done
