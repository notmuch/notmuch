#!/usr/bin/env bash
test_description='"notmuch show"'

. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus

test_begin_subtest "exit code for show invalid query"
notmuch show foo..
exit_code=$?
test_expect_equal 1 $exit_code

test_begin_subtest "notmuch show --sort=newest-first"
notmuch show --entire-thread=true '*' > EXPECTED
notmuch show --entire-thread=true --sort=newest-first '*' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch show --sort=oldest-first"
notmuch show --entire-thread=true '*' | grep ^depth:0 > EXPECTED
notmuch show --entire-thread=true --sort=oldest-first '*' | grep ^depth:0 > OLDEST
perl -e 'print reverse<>' OLDEST > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch show --sort for single thread"
QUERY="id:yun1vjwegii.fsf@aiko.keithp.com"
notmuch show --entire-thread=true --sort=newest-first $QUERY > EXPECTED
notmuch show --entire-thread=true --sort=oldest-first $QUERY > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_done
