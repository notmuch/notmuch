#!/usr/bin/env bash
#
# Copyright (c) 2011 David Bremner
#

# This test tests whether hiding Xapian::Error symbols in libnotmuch
# also hides them for other users of libxapian. This is motivated by
# the discussion in http://gcc.gnu.org/wiki/Visibility'

test_description='exception symbol hiding'

. ./test-lib.sh || exit 1

test_begin_subtest 'running test' run_test
mkdir -p ${PWD}/fakedb/.notmuch
( LD_LIBRARY_PATH="$TEST_DIRECTORY/../lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
		 $TEST_DIRECTORY/symbol-test ${PWD}/fakedb ${PWD}/nonexistent \
		 2>&1 | sed "s,${PWD},CWD,g") > OUTPUT

cat <<EOF > EXPECTED
A Xapian exception occurred opening database: Couldn't stat 'CWD/fakedb/.notmuch/xapian'
caught No chert database found at path \`CWD/nonexistent'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'checking output'
test_expect_equal "$result" "$output"

test_begin_subtest 'comparing existing to exported symbols'
nm -P $TEST_DIRECTORY/../lib/libnotmuch.so | awk '$2 == "T" && $1 ~ "^notmuch" {print $1}' | sort | uniq > ACTUAL
sed -n 's/[[:blank:]]*\(notmuch_[^;]*\);/\1/p' $TEST_DIRECTORY/../notmuch.sym | sort | uniq > EXPORTED
test_expect_equal_file EXPORTED ACTUAL

test_done
