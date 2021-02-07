#!/usr/bin/env bash
#
# Copyright (c) 2011 David Bremner
#

# This test tests whether hiding Xapian::Error symbols in libnotmuch
# also hides them for other users of libxapian. This is motivated by
# the discussion in https://gcc.gnu.org/wiki/Visibility'

test_description='exception symbol hiding'

. $(dirname "$0")/test-lib.sh || exit 1

test_begin_subtest 'running test' run_test
mkdir -p ${PWD}/fakedb/.notmuch
$TEST_DIRECTORY/symbol-test ${PWD}/fakedb ${PWD}/nonexistent 2>&1 \
	| notmuch_dir_sanitize | sed -e "s,\`,\',g" -e "s,No [^[:space:]]* database,No XXXXXX database,g" > OUTPUT

cat <<EOF > EXPECTED
Cannot open Xapian database at CWD/fakedb/.notmuch/xapian: Couldn't stat 'CWD/fakedb/.notmuch/xapian'
caught No XXXXXX database found at path 'CWD/nonexistent'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'checking output'
test_expect_equal "$result" "$output"

test_begin_subtest 'comparing existing to exported symbols'
readelf -Ws $NOTMUCH_BUILDDIR/lib/libnotmuch.so | sed -e 's/\[[^]]*\]//' |\
    awk '$4 == "FUNC" && $5 == "GLOBAL" && $7 != "UND" {print $8}' | sort -u > ACTUAL
sed -n 's/^\(notmuch_[a-zA-Z0-9_]*\)[[:blank:]]*(.*/\1/p' $NOTMUCH_SRCDIR/lib/notmuch.h | sort -u > EXPORTED
test_expect_equal_file EXPORTED ACTUAL

test_done
