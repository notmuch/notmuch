#!/usr/bin/env bash

test_description="protected headers in emacs interface"
. $(dirname "$0")/test-lib.sh || exit 1
. $NOTMUCH_SRCDIR/test/test-lib-emacs.sh || exit 1

# testing protected headers with emacs
test_require_emacs
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

# notmuch-emacs still leaks the subject line; as long as it leaks the
# subject line, it should emit the external subject, not the protected
# subject, even if it knows what the true subject is:
test_begin_subtest "Reply within emacs to a message with protected headers, not leaking subject"
test_emacs "(let ((message-hidden-headers '()))
	    (notmuch-show \"id:protected-header@crypto.notmuchmail.org\")
	    (notmuch-show-reply)
	    (test-output))"
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: test_suite@notmuchmail.org
Subject: Re: Subject Unavailable
In-Reply-To: <protected-header@crypto.notmuchmail.org>
Fcc: ${MAIL_DIR}/sent
References: <protected-header@crypto.notmuchmail.org>
--text follows this line--
<#secure method=pgpmime mode=signencrypt>
test_suite@notmuchmail.org writes:

> This is the sekrit message
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

# notmuch-emacs still leaks the subject line:
test_begin_subtest "don't leak protected subject during reply, even if indexed"
test_emacs "(let ((message-hidden-headers '()))
	    (notmuch-show \"id:protected-header@crypto.notmuchmail.org\")
	    (notmuch-show-reply)
	    (test-output))"
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: test_suite@notmuchmail.org
Subject: Re: Subject Unavailable
In-Reply-To: <protected-header@crypto.notmuchmail.org>
Fcc: ${MAIL_DIR}/sent
References: <protected-header@crypto.notmuchmail.org>
--text follows this line--
<#secure method=pgpmime mode=signencrypt>
test_suite@notmuchmail.org writes:

> This is the sekrit message
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
