#!/usr/bin/env bash

test_description="emacs interface"
. $(dirname "$0")/test-lib.sh || exit 1

# testing protected headers with emacs
add_gnupg_home
add_email_corpus protected-headers

test_begin_subtest "notmuch-search should show not unindexed protected subject header in emacs"
test_emacs '(notmuch-search "id:protected-header@crypto.notmuchmail.org")
	    (notmuch-test-wait)
	    (test-output)'
cat <<EOF >EXPECTED
  2000-01-01 [1/1]   test_suite@notmuchmail.org  Subject Unavailable (encrypted inbox unread)
End of search results.
EOF
test_expect_equal_file EXPECTED OUTPUT

# protected headers should behave differently after re-indexing
test_begin_subtest 'defaulting to indexing cleartext'
test_expect_success 'notmuch config set index.decrypt true'
test_begin_subtest 'try reindexing protected header message'
test_expect_success 'notmuch reindex id:protected-header@crypto.notmuchmail.org'

test_begin_subtest "notmuch-search should show indexed protected subject header in emacs"
test_emacs '(notmuch-search "id:protected-header@crypto.notmuchmail.org")
	    (notmuch-test-wait)
	    (test-output)'
cat <<EOF >EXPECTED
  2000-01-01 [1/1]   test_suite@notmuchmail.org  This is a protected header (encrypted inbox unread)
End of search results.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
