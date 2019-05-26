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

test_begin_subtest "notmuch-show should not show unindexed protected subject header in emacs when nm-c-process-mime is nil"
test_emacs '(let ((notmuch-crypto-process-mime nil))
             (notmuch-show "id:protected-header@crypto.notmuchmail.org")
             (test-output))'
cat <<EOF >EXPECTED
test_suite@notmuchmail.org (2000-01-01) (encrypted inbox)
Subject: Subject Unavailable
To: test_suite@notmuchmail.org
Date: Sat, 01 Jan 2000 12:00:00 +0000

[ multipart/encrypted ]
[ Unknown encryption status ]
[ application/pgp-encrypted ]
[ application/octet-stream ]
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch-show should show protected subject header in emacs"
test_emacs '(notmuch-show "id:protected-header@crypto.notmuchmail.org")
	    (test-output)'
cat <<EOF >EXPECTED
test_suite@notmuchmail.org (2000-01-01) (encrypted inbox)
Subject: This is a protected header
To: test_suite@notmuchmail.org
Date: Sat, 01 Jan 2000 12:00:00 +0000

[ multipart/encrypted ]
[ Decryption successful ]
[ application/pgp-encrypted ]
[ text/plain ]
This is the sekrit message
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
  2000-01-01 [1/1]   test_suite@notmuchmail.org  This is a protected header (encrypted inbox)
End of search results.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
