#!/usr/bin/env bash
#
# Copyright (c) 2005 Junio C Hamano
#

test_description='the test framework itself.'

################################################################
# It appears that people try to run tests without building...

if ! test -x ../notmuch
then
	echo >&2 'You do not seem to have built notmuch yet.'
	exit 1
fi

. ./test-lib.sh || exit 1

################################################################
# Test harness
test_begin_subtest 'success is reported like this'
test_expect_success ':'

test_begin_subtest 'test runs if prerequisite is satisfied'
test_set_prereq HAVEIT
haveit=no
test_expect_success 'test_have_prereq HAVEIT && haveit=yes'

test_begin_subtest 'tests clean up after themselves'
clean=no
test_expect_success 'test_when_finished clean=yes'

test_begin_subtest 'tests clean up even after a failure'
cleaner=no
test_expect_code 1 'test_when_finished cleaner=yes && (exit 1)'

if test $clean$cleaner != yesyes
then
	say "bug in test framework: cleanup commands do not work reliably"
	exit 1
fi

test_begin_subtest 'failure to clean up causes the test to fail'
test_expect_code 2 'test_when_finished "(exit 2)"'

EXPECTED=$TEST_DIRECTORY/test.expected-output
suppress_diff_date() {
    sed -e 's/\(.*\-\-\- test-verbose\.4\.\expected\).*/\1/' \
	-e 's/\(.*\+\+\+ test-verbose\.4\.\output\).*/\1/'
}

test_begin_subtest "Ensure that test output is suppressed unless the test fails"
output=$(cd $TEST_DIRECTORY; NOTMUCH_TEST_QUIET= ./test-verbose 2>&1 | suppress_diff_date)
expected=$(cat $EXPECTED/test-verbose-no | suppress_diff_date)
test_expect_equal "$output" "$expected"

test_begin_subtest "Ensure that -v does not suppress test output"
output=$(cd $TEST_DIRECTORY; NOTMUCH_TEST_QUIET= ./test-verbose -v 2>&1 | suppress_diff_date)
expected=$(cat $EXPECTED/test-verbose-yes | suppress_diff_date)
# Do not include the results of test-verbose in totals
rm $TEST_DIRECTORY/test-results/test-verbose
rm -r $TEST_DIRECTORY/tmp.test-verbose
test_expect_equal "$output" "$expected"


################################################################
# Test mail store prepared in test-lib.sh

test_begin_subtest 'test that mail store was created'
test_expect_success 'test -d "${MAIL_DIR}"'

test_begin_subtest 'mail store should be empty'
find "${MAIL_DIR}" -type f -print >should-be-empty
test_expect_success 'cmp -s /dev/null should-be-empty'

test_begin_subtest 'NOTMUCH_CONFIG is set and points to an existing file'
test_expect_success 'test -f "${NOTMUCH_CONFIG}"'

test_begin_subtest 'PATH is set to build directory'
test_expect_equal \
    "$(dirname ${TEST_DIRECTORY})" \
    "$(echo $PATH|cut -f1 -d: | sed -e 's,/test/valgrind/bin$,,')"

test_begin_subtest 'notmuch is compiled with debugging symbols'
readelf --sections $(command -v notmuch) | grep \.debug
test_expect_equal 0 $?

test_done
