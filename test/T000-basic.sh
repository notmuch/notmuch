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

. ./test-lib.sh

################################################################
# Test harness
test_expect_success 'success is reported like this' '
    :
'
test_set_prereq HAVEIT
haveit=no
test_expect_success HAVEIT 'test runs if prerequisite is satisfied' '
    test_have_prereq HAVEIT &&
    haveit=yes
'

clean=no
test_expect_success 'tests clean up after themselves' '
    test_when_finished clean=yes
'

cleaner=no
test_expect_code 1 'tests clean up even after a failure' '
    test_when_finished cleaner=yes &&
    (exit 1)
'

if test $clean$cleaner != yesyes
then
	say "bug in test framework: cleanup commands do not work reliably"
	exit 1
fi

test_expect_code 2 'failure to clean up causes the test to fail' '
    test_when_finished "(exit 2)"
'

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

test_expect_success \
    'test that mail store was created' \
    'test -d "${MAIL_DIR}"'


find "${MAIL_DIR}" -type f -print >should-be-empty
test_expect_success \
    'mail store should be empty' \
    'cmp -s /dev/null should-be-empty'

test_expect_success \
    'NOTMUCH_CONFIG is set and points to an existing file' \
    'test -f "${NOTMUCH_CONFIG}"'

test_begin_subtest 'PATH is set to build directory'
test_expect_equal \
    "$(dirname ${TEST_DIRECTORY})" \
    "$(echo $PATH|cut -f1 -d: | sed -e 's,/test/valgrind/bin$,,')"

test_done
