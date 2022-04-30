#!/usr/bin/env bash

test_description='emacs operations'

. $(dirname "$0")/perf-test-lib.sh || exit 1
. $NOTMUCH_SRCDIR/test/test-lib-emacs.sh || exit 1

test_require_emacs

time_start

print_emacs_header

MSGS=$(notmuch search --output=messages "*" | shuf -n 50 | awk '{printf " \"%s\"",$1}')

time_emacs "tag messages" \
"(dolist (msg (list $MSGS))
   (notmuch-tag msg (list \"+test\"))
   (notmuch-tag msg (list \"-test\"))))"

time_done
