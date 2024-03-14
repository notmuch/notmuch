#!/usr/bin/env bash
test_description="Emacs Draft Handling"
. $(dirname "$0")/test-lib.sh || exit 1
. $NOTMUCH_SRCDIR/test/test-lib-emacs.sh || exit 1

test_require_emacs
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

add_email_corpus attachment
test_begin_subtest "Saving a draft keeps hidden headers"
test_emacs '(notmuch-mua-new-reply "id:874llc2bkp.fsf@curie.anarc.at")
            (message-goto-subject)
            (delete-line)
            (insert "Subject: draft-test-reply\n")
	    (test-output "DRAFT")
	    (notmuch-draft-postpone)
	    (notmuch-show "subject:draft-test-reply")
	    (notmuch-show-resume-message)
	    (test-output)'
notmuch_dir_sanitize OUTPUT > OUTPUT.clean

cat <<EOF > EXPECTED
References: <87d10042pu.fsf@curie.anarc.at> <87woy8vx7i.fsf@tesseract.cs.unb.ca> <87a7v42bv9.fsf@curie.anarc.at> <874llc2bkp.fsf@curie.anarc.at>
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Antoine Beaupré <anarcat@orangeseeds.org>
Subject: draft-test-reply
In-Reply-To: <874llc2bkp.fsf@curie.anarc.at>
Fcc: MAIL_DIR/sent
--text follows this line--
Antoine Beaupré <anarcat@orangeseeds.org> writes:

> And obviously I forget the frigging attachment.
>
>
> PS: don't we have a "you forgot to actually attach the damn file" plugin
> when we detect the word "attachment" and there's no attach? :p
EOF
test_expect_equal_file EXPECTED OUTPUT.clean

test_done
