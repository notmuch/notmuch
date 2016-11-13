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
	    (mml-secure-message-sign)
	    (notmuch-draft-save)
	    (test-output)'
header_count=$(notmuch show --format=raw subject:draft-test-0003 | grep -c ^X-Notmuch-Emacs-Secure)
body_count=$(notmuch notmuch show --format=raw subject:draft-test-0003 | grep -c '^\<#secure')
test_expect_equal "$header_count,$body_count" "1,0"

test_done
