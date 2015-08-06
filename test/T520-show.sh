#!/usr/bin/env bash
test_description='"notmuch show"'

. ./test-lib.sh || exit 1

add_email_corpus

test_begin_subtest "exit code for show invalid query"
notmuch show foo..
exit_code=$?
test_expect_equal 1 $exit_code

test_done
