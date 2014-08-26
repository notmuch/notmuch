#!/usr/bin/env bash
test_description='"notmuch compact"'
. ./test-lib.sh

add_message '[subject]=One'
add_message '[subject]=Two'
add_message '[subject]=Three'

notmuch tag +tag1 \*
notmuch tag +tag2 subject:Two
notmuch tag -tag1 +tag3 subject:Three

if ! ${TEST_DIRECTORY}/have-compact; then
    test_begin_subtest "Compact unsupported: error message"
    output=$(notmuch compact --quiet 2>&1)
    test_expect_equal "$output" "notmuch was compiled against a xapian version lacking compaction support.
Compaction failed: Unsupported operation"

    test_expect_code 1 "Compact unsupported: status code" "notmuch compact"

    test_done
fi

test_expect_success "Running compact" "notmuch compact --backup=${TEST_DIRECTORY}/xapian.old"

test_begin_subtest "Compact preserves database"
output=$(notmuch search \* | notmuch_search_sanitize)
test_expect_equal "$output" "\
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; One (inbox tag1 unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Two (inbox tag1 tag2 unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Three (inbox tag3 unread)"

test_expect_success 'Restoring Backup' \
    'rm -Rf ${MAIL_DIR}/.notmuch/xapian &&
     mv ${TEST_DIRECTORY}/xapian.old ${MAIL_DIR}/.notmuch/xapian'

test_begin_subtest "Checking restored backup"
output=$(notmuch search \* | notmuch_search_sanitize)
test_expect_equal "$output" "\
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; One (inbox tag1 unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Two (inbox tag1 tag2 unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Three (inbox tag3 unread)"

test_done
