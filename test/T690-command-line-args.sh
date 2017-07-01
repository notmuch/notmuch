#!/usr/bin/env bash

test_description="command line arguments"
. ./test-lib.sh || exit 1

NOTMUCH_NEW > /dev/null

test_begin_subtest 'bad option to show'
notmuch show --frobnicate >& OUTPUT
cat <<EOF > EXPECTED
Unrecognized option: --frobnicate
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
