#!/bin/bash
#
# Copyright (c) 2005 Junio C Hamano
#

test_description='Tests the test framework itself.'

################################################################
# It appears that people try to run tests without building...

if ! test -x ../notmuch
then
	echo >&2 'You do not seem to have built notmuch yet.'
	exit 1
fi

. ./test-lib.sh

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

test_expect_success \
    'PATH is set to this repository' \
    'test "`echo $PATH|cut -f1 -d:`" = "`dirname ${TEST_DIRECTORY}`"'

################################################################
# Test harness
test_expect_success 'success is reported like this' '
    :
'
test_expect_failure 'pretend we have a known breakage' '
    false
'
test_expect_failure 'pretend we have fixed a known breakage' '
    :
'
test_set_prereq HAVEIT
haveit=no
test_expect_success HAVEIT 'test runs if prerequisite is satisfied' '
    test_have_prereq HAVEIT &&
    haveit=yes
'
donthaveit=yes
test_expect_success DONTHAVEIT 'unmet prerequisite causes test to be skipped' '
    donthaveit=no
'
if test $haveit$donthaveit != yesyes
then
	say "bug in test framework: prerequisite tags do not work reliably"
	exit 1
fi

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

test_done
