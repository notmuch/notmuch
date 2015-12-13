#!/usr/bin/env bash

test_description="emacs test function sanity"
. ./test-lib.sh || exit 1

test_begin_subtest "emacs test function sanity"
test_emacs_expect_t 't'

test_done
