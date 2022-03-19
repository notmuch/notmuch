#!/usr/bin/env bash

test_description="emacs reply"
. $(dirname "$0")/test-lib.sh || exit 1
. $NOTMUCH_SRCDIR/test/test-lib-emacs.sh || exit 1

EXPECTED=$NOTMUCH_SRCDIR/test/emacs-show.expected-output

test_require_emacs

add_email_corpus attachment

test_begin_subtest "tar not inlined by default"
test_emacs '(notmuch-mua-new-reply "id:874llc2bkp.fsf@curie.anarc.at")
	(test-visible-output "OUTPUT.raw")'
cat <<EOF > EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Antoine Beaupré <anarcat@orangeseeds.org>
Subject: Re: bug: "no top level messages" crash on Zen email loops
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
notmuch_dir_sanitize < OUTPUT.raw > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_done
