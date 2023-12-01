#!/usr/bin/env bash

test_description="emacs attachment warnings"
. $(dirname "$0")/test-lib.sh || exit 1
. $NOTMUCH_SRCDIR/test/test-lib-emacs.sh || exit 1

test_require_emacs

test_begin_subtest "notmuch-test-attachment-warning part 1"
test_emacs_expect_t '(notmuch-test-attachment-warning-1)'

test_done
