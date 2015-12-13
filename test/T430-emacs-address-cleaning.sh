#!/usr/bin/env bash

test_description="emacs address cleaning"
. ./test-lib.sh || exit 1

test_begin_subtest "notmuch-test-address-clean part 1"
test_emacs_expect_t '(notmuch-test-address-cleaning-1)'

test_begin_subtest "notmuch-test-address-clean part 2"
test_emacs_expect_t '(notmuch-test-address-cleaning-2)'

test_begin_subtest "notmuch-test-address-clean part 3"
test_emacs_expect_t '(notmuch-test-address-cleaning-3)'

test_done
