#!/usr/bin/env bash
test_description="database version and feature compatibility"

. ./test-lib.sh || exit 1

test_begin_subtest "future database versions abort open"
${TEST_DIRECTORY}/make-db-version ${MAIL_DIR} 9999 ""
output=$(notmuch search x 2>&1 | sed 's/\(database at\) .*/\1 FILENAME/')
rm -rf ${MAIL_DIR}/.notmuch
test_expect_equal "$output" "\
Error: Notmuch database at FILENAME
       has a newer database format version (9999) than supported by this
       version of notmuch (3)."

test_begin_subtest "unknown 'rw' feature aborts read/write open"
${TEST_DIRECTORY}/make-db-version ${MAIL_DIR} 3 $'test feature\trw'
output=$(notmuch new 2>&1 | sed 's/\(database at\) .*/\1 FILENAME/')
rm -rf ${MAIL_DIR}/.notmuch
test_expect_equal "$output" "\
Error: Notmuch database at FILENAME
       requires features (test feature)
       not supported by this version of notmuch."

test_begin_subtest "unknown 'rw' feature aborts read-only open"
${TEST_DIRECTORY}/make-db-version ${MAIL_DIR} 3 $'test feature\trw'
output=$(notmuch search x 2>&1 | sed 's/\(database at\) .*/\1 FILENAME/')
rm -rf ${MAIL_DIR}/.notmuch
test_expect_equal "$output" "\
Error: Notmuch database at FILENAME
       requires features (test feature)
       not supported by this version of notmuch."

test_begin_subtest "unknown 'w' feature aborts read/write open"
${TEST_DIRECTORY}/make-db-version ${MAIL_DIR} 3 $'test feature\tw'
output=$(notmuch new 2>&1 | sed 's/\(database at\) .*/\1 FILENAME/')
rm -rf ${MAIL_DIR}/.notmuch
test_expect_equal "$output" "\
Error: Notmuch database at FILENAME
       requires features (test feature)
       not supported by this version of notmuch."

test_begin_subtest "unknown 'w' feature does not abort read-only open"
${TEST_DIRECTORY}/make-db-version ${MAIL_DIR} 3 $'test feature\tw'
output=$(notmuch search x 2>&1 | sed 's/\(database at\) .*/\1 FILENAME/')
rm -rf ${MAIL_DIR}/.notmuch
test_expect_equal "$output" ""

test_done
