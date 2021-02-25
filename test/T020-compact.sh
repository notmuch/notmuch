#!/usr/bin/env bash
test_description='"notmuch compact"'
. $(dirname "$0")/test-lib.sh || exit 1

add_message '[subject]=One'
add_message '[subject]=Two'
add_message '[subject]=Three'

notmuch tag +tag1 \*
notmuch tag +tag2 subject:Two
notmuch tag -tag1 +tag3 subject:Three

test_begin_subtest "Running compact"
test_expect_success "notmuch compact --backup=${TMP_DIRECTORY}/xapian.old"

test_begin_subtest "Compact preserves database"
output=$(notmuch search \* | notmuch_search_sanitize)
test_expect_equal "$output" "\
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; One (inbox tag1 unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Two (inbox tag1 tag2 unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Three (inbox tag3 unread)"

test_begin_subtest "Restoring Backup"
test_expect_success 'rm -Rf ${MAIL_DIR}/.notmuch/xapian &&
     mv ${TMP_DIRECTORY}/xapian.old ${MAIL_DIR}/.notmuch/xapian'

test_begin_subtest "Checking restored backup"
output=$(notmuch search \* | notmuch_search_sanitize)
test_expect_equal "$output" "\
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; One (inbox tag1 unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Two (inbox tag1 tag2 unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Three (inbox tag3 unread)"

test_done
