#!/usr/bin/env bash
#
# Copyright (c) 2011 David Bremner
#

# This test tests whether hiding Xapian::Error symbols in libnotmuch
# also hides them for other users of libxapian. This is motivated by
# the discussion in http://gcc.gnu.org/wiki/Visibility'

test_description='exception symbol hiding'

. ./test-lib.sh

run_test(){
    result=$(LD_LIBRARY_PATH="$TEST_DIRECTORY/../lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" $TEST_DIRECTORY/symbol-test 2>&1)
}

output="A Xapian exception occurred opening database: Couldn't stat 'fakedb/.notmuch/xapian'
caught No chert database found at path \`./nonexistant'"

mkdir -p fakedb/.notmuch

test_expect_success 'running test' run_test

test_begin_subtest 'checking output'
test_expect_equal "$result" "$output"

test_begin_subtest 'comparing existing to exported symbols'
objdump -t $TEST_DIRECTORY/../lib/*.o | awk '$4 == ".text" && $6 ~ "^notmuch" {print $6}' | sort | uniq > ACTUAL
sed -n 's/[[:blank:]]*\(notmuch_[^;]*\);/\1/p' $TEST_DIRECTORY/../notmuch.sym | sort | uniq > EXPORTED
test_expect_equal_file EXPORTED ACTUAL

test_done
