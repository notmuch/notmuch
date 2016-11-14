#!/usr/bin/env bash
test_description="Emacs Draft Handling"
. ./test-lib.sh || exit 1

add_email_corpus

notmuch config set search.exclude_tags deleted

test_begin_subtest "Saving a draft indexes it"
test_emacs '(notmuch-mua-mail)
	    (message-goto-subject)
	    (insert "draft-test-0001")
	    (notmuch-draft-save)
	    (test-output)'
count1=$(notmuch count tag:draft)
count2=$(notmuch count subject:draft-test-0001)
test_expect_equal "$count1=$count2" "1=1"

test_begin_subtest "Saving a draft tags previous draft as deleted"
test_emacs '(notmuch-mua-mail)
	    (message-goto-subject)
	    (insert "draft-test-0002")
	    (notmuch-draft-save)
	    (notmuch-draft-save)
	    (test-output)'
count1=$(notmuch count tag:draft)
count2=$(notmuch count subject:draft-test-0002)

test_expect_equal "$count1,$count2" "2,1"

test_begin_subtest "Saving a signed draft adds header"
test_emacs '(notmuch-mua-mail)
	    (message-goto-subject)
	    (insert "draft-test-0003")
            ;; We would use (mml-secure-message-sign) but on emacs23
            ;; that only signs the part, not the whole message.
            (mml-secure-message mml-secure-method '\''sign)
	    (notmuch-draft-save)
	    (test-output)'
header_count=$(notmuch show --format=raw subject:draft-test-0003 | grep -c ^X-Notmuch-Emacs-Secure)
body_count=$(notmuch notmuch show --format=raw subject:draft-test-0003 | grep -c '^\<#secure')
test_expect_equal "$header_count,$body_count" "1,0"

test_begin_subtest "Refusing to save an encrypted draft"
test_emacs '(notmuch-mua-mail)
	    (message-goto-subject)
	    (insert "draft-test-0004")
	    (mml-secure-message-sign-encrypt)
	    (let ((notmuch-draft-save-plaintext nil))
		     (notmuch-draft-save))
	    (test-output)'
count1=$(notmuch count tag:draft)
count2=$(notmuch count subject:draft-test-0004)

test_expect_equal "$count1,$count2" "3,0"

test_begin_subtest "Resuming a signed draft"

test_emacs '(notmuch-show "subject:draft-test-0003")
	    (notmuch-show-resume-message)
	    (test-output)'
notmuch_dir_sanitize OUTPUT > OUTPUT.clean
cat <<EOF | notmuch_dir_sanitize >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: 
Subject: draft-test-0003
Fcc: MAIL_DIR/sent
--text follows this line--
<#secure method=pgpmime mode=sign>
EOF
test_expect_equal_file EXPECTED OUTPUT.clean
test_done
