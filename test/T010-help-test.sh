#!/usr/bin/env bash

test_description="online help"
. ./test-lib.sh || exit 1

test_begin_subtest 'notmuch --help'
test_expect_success 'notmuch --help'

test_begin_subtest 'notmuch help'
test_expect_success 'notmuch help'

test_begin_subtest 'notmuch --version'
test_expect_success 'notmuch --version'

if [ $NOTMUCH_HAVE_MAN -eq 1 ]; then
    test_begin_subtest 'notmuch --help tag'
    test_expect_success 'notmuch --help tag'

    test_begin_subtest 'notmuch help tag'
    test_expect_success 'notmuch help tag'
else
    test_begin_subtest 'notmuch --help tag (man pages not available)'
    test_expect_success 'test_must_fail notmuch --help tag >/dev/null'

    test_begin_subtest 'notmuch help tag (man pages not available)'
    test_expect_success 'test_must_fail notmuch help tag >/dev/null'
fi

test_done
