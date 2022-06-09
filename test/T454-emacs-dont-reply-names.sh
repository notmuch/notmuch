#!/usr/bin/env bash

test_description="message-dont-reply-to-names in emacs replies"
. $(dirname "$0")/test-lib.sh || exit 1
. $NOTMUCH_SRCDIR/test/test-lib-emacs.sh || exit 1

EXPECTED=$NOTMUCH_SRCDIR/test/emacs-show.expected-output

test_require_emacs

add_email_corpus default

test_begin_subtest "regular expression"
test_emacs '(let ((message-dont-reply-to-names "notmuchmail\\|noreply\\|harvard"))
	      (notmuch-mua-new-reply
	        "id:20091117203301.GV3165@dottiness.seas.harvard.edu" nil t)
	      (test-visible-output "OUTPUT-FULL.raw"))'

notmuch_dir_sanitize < OUTPUT-FULL.raw > OUTPUT-FULL
head -6 OUTPUT-FULL > OUTPUT

cat <<EOF > EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Mikhail Gusarov <dottedmag@dottedmag.net>
Subject: Re: [notmuch] Working with Maildir storage?
In-Reply-To: <20091117203301.GV3165@dottiness.seas.harvard.edu>
Fcc: MAIL_DIR/sent
--text follows this line--
EOF

test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "predicate"
test_emacs '(let ((message-dont-reply-to-names
	           (lambda (m) (string-prefix-p "Mikhail" m))))
	      (notmuch-mua-new-reply
	        "id:20091117203301.GV3165@dottiness.seas.harvard.edu" nil t)
	      (test-visible-output "OUTPUT-FULL-PRED.raw"))'

notmuch_dir_sanitize < OUTPUT-FULL-PRED.raw > OUTPUT-FULL-PRED
head -7 OUTPUT-FULL-PRED > OUTPUT-PRED

cat <<EOF > EXPECTED-PRED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Lars Kellogg-Stedman <lars@seas.harvard.edu>
Cc: notmuch@notmuchmail.org
Subject: Re: [notmuch] Working with Maildir storage?
In-Reply-To: <20091117203301.GV3165@dottiness.seas.harvard.edu>
Fcc: MAIL_DIR/sent
--text follows this line--
EOF

test_expect_equal_file EXPECTED-PRED OUTPUT-PRED

test_begin_subtest "nil value"
test_emacs '(let ((message-dont-reply-to-names nil))
	      (notmuch-mua-new-reply
	        "id:20091117203301.GV3165@dottiness.seas.harvard.edu" nil t)
	      (test-visible-output "OUTPUT-FULL-NIL.raw"))'

notmuch_dir_sanitize < OUTPUT-FULL-NIL.raw > OUTPUT-FULL-NIL
head -7 OUTPUT-FULL-NIL > OUTPUT-NIL

cat <<EOF > EXPECTED-NIL
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Lars Kellogg-Stedman <lars@seas.harvard.edu>, Mikhail Gusarov <dottedmag@dottedmag.net>
Cc: notmuch@notmuchmail.org
Subject: Re: [notmuch] Working with Maildir storage?
In-Reply-To: <20091117203301.GV3165@dottiness.seas.harvard.edu>
Fcc: MAIL_DIR/sent
--text follows this line--
EOF

test_expect_equal_file EXPECTED-NIL OUTPUT-NIL

test_done
