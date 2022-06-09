#!/usr/bin/env bash

test_description="emacs notmuch-show view"
. $(dirname "$0")/test-lib.sh || exit 1
. $NOTMUCH_SRCDIR/test/test-lib-emacs.sh || exit 1

EXPECTED=$NOTMUCH_SRCDIR/test/emacs-show.expected-output

test_require_emacs
add_email_corpus

test_begin_subtest "Hiding Original Message region at beginning of a message"
message_id='OriginalMessageHiding.1@notmuchmail.org'
add_message \
    [id]="$message_id" \
    '[subject]="Hiding Original Message region at beginning of a message"' \
    '[body]="-----Original Message-----
Text here."'

cat <<EOF >EXPECTED
Notmuch Test Suite <test_suite@notmuchmail.org> (2001-01-05) (inbox)
Subject: Hiding Original Message region at beginning of a message
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: GENERATED_DATE

[ 2-line hidden original message. Click/Enter to show. ]
EOF

test_emacs "(notmuch-show \"id:$message_id\")
	    (test-visible-output \"OUTPUT.raw\")"
notmuch_date_sanitize < OUTPUT.raw > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Bare subject #1"
output=$(test_emacs '(notmuch-show-strip-re "Re: subject")')
test_expect_equal "$output" '"subject"'

test_begin_subtest "Bare subject #2"
output=$(test_emacs '(notmuch-show-strip-re "re:Re: re:  Re:  re:subject")')
test_expect_equal "$output" '"subject"'

test_begin_subtest "Bare subject #3"
output=$(test_emacs '(notmuch-show-strip-re "the cure: fix the regexp")')
test_expect_equal "$output" '"the cure: fix the regexp"'

test_begin_subtest "don't process cryptographic MIME parts"
test_emacs '(let ((notmuch-crypto-process-mime nil))
	(notmuch-show "id:20091117203301.GV3165@dottiness.seas.harvard.edu")
	(test-visible-output))'
test_expect_equal_file $EXPECTED/notmuch-show-process-crypto-mime-parts-off OUTPUT

test_begin_subtest "process cryptographic MIME parts"
test_emacs '(let ((notmuch-crypto-process-mime t))
	(notmuch-show "id:20091117203301.GV3165@dottiness.seas.harvard.edu")
	(test-visible-output))'
test_expect_equal_file $EXPECTED/notmuch-show-process-crypto-mime-parts-on OUTPUT

test_begin_subtest "process cryptographic MIME parts (w/ notmuch-show-toggle-process-crypto)"
test_emacs '(let ((notmuch-crypto-process-mime nil))
	(notmuch-show "id:20091117203301.GV3165@dottiness.seas.harvard.edu")
	(notmuch-show-toggle-process-crypto)
	(test-visible-output))'
test_expect_equal_file $EXPECTED/notmuch-show-process-crypto-mime-parts-on OUTPUT

test_begin_subtest "notmuch-search-show-thread returns non-nil on success"
test_emacs_expect_t  '(notmuch-search "id:20091117203301.GV3165@dottiness.seas.harvard.edu")
		      (notmuch-test-wait)
		      (and (notmuch-search-show-thread)
			   (not (notmuch-show-next-thread)))'

test_begin_subtest "notmuch-search-show-thread returns nil when there are no messages"
test_emacs_expect_t  '(notmuch-search "id:non-existing-id")
		      (notmuch-test-wait)
		      (not (notmuch-search-show-thread))'

test_begin_subtest "notmuch-show: don't elide non-matching messages"
test_emacs '(let ((notmuch-show-only-matching-messages nil))
	(notmuch-search "from:lars@seas.harvard.edu and subject:\"Maildir storage\"")
	(notmuch-test-wait)
	(notmuch-search-show-thread)
	(notmuch-test-wait)
	(test-visible-output))'
test_expect_equal_file $EXPECTED/notmuch-show-elide-non-matching-messages-off OUTPUT

test_begin_subtest "notmuch-show: elide non-matching messages"
test_emacs '(let ((notmuch-show-only-matching-messages t))
	(notmuch-search "from:lars@seas.harvard.edu and subject:\"Maildir storage\"")
	(notmuch-test-wait)
	(notmuch-search-show-thread)
	(notmuch-test-wait)
	(test-visible-output))'
test_expect_equal_file $EXPECTED/notmuch-show-elide-non-matching-messages-on OUTPUT

test_begin_subtest "notmuch-show: elide non-matching messages (w/ notmuch-show-toggle-elide-non-matching)"
test_emacs '(let ((notmuch-show-only-matching-messages nil))
	(notmuch-search "from:lars@seas.harvard.edu and subject:\"Maildir storage\"")
	(notmuch-test-wait)
	(notmuch-search-show-thread)
	(notmuch-test-wait)
	(notmuch-show-toggle-elide-non-matching)
	(test-visible-output))'
test_expect_equal_file $EXPECTED/notmuch-show-elide-non-matching-messages-on OUTPUT

test_begin_subtest "notmuch-show: elide non-matching messages (w/ prefix arg to notmuch-show)"
test_emacs '(let ((notmuch-show-only-matching-messages nil))
	(notmuch-search "from:lars@seas.harvard.edu and subject:\"Maildir storage\"")
	(notmuch-test-wait)
	(notmuch-search-show-thread t)
	(notmuch-test-wait)
	(test-visible-output))'
test_expect_equal_file $EXPECTED/notmuch-show-elide-non-matching-messages-on OUTPUT

test_begin_subtest "notmuch-show: disable indentation of thread content (w/ notmuch-show-toggle-thread-indentation)"
test_emacs '(notmuch-search "from:lars@seas.harvard.edu and subject:\"Maildir storage\"")
	(notmuch-test-wait)
	(notmuch-search-show-thread)
	(notmuch-test-wait)
	(notmuch-show-toggle-thread-indentation)
	(test-visible-output)'
test_expect_equal_file $EXPECTED/notmuch-show-indent-thread-content-off OUTPUT

test_begin_subtest "id buttonization"
add_message '[body]="
id:abc
id:abc.def. id:abc,def, id:abc;def; id:abc:def:
id:foo@bar.?baz? id:foo@bar!.baz!
(id:foo@bar.baz) [id:foo@bar.baz]
id:foo@bar.baz...
id:2+2=5
id:=_-:/.[]@$%+
id:abc)def
id:ab\"c def
id:\"abc\"def
id:\"ab\"\"c\"def
id:\"ab c\"def
id:\"abc\".def
id:\"abc
\"
id:)
id:
cid:xxx
mid:abc mid:abc/def
mid:abc%20def
mid:abc. mid:abc, mid:abc;"'
test_emacs '(notmuch-show "id:'$gen_msg_id'")
	(notmuch-test-mark-links)
	(test-visible-output "OUTPUT.raw")'
cat <<EOF >EXPECTED
Notmuch Test Suite <test_suite@notmuchmail.org> (2001-01-05) (inbox)
Subject: id buttonization
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: GENERATED_DATE

<<id:abc>>
<<id:abc.def>>. <<id:abc,def>>, <<id:abc;def>>; <<id:abc:def>>:
<<id:foo@bar.?baz>>? <<id:foo@bar!.baz>>!
(<<id:foo@bar.baz>>) [<<id:foo@bar.baz>>]
<<id:foo@bar.baz>>...
<<id:2+2=5>>
<<id:=_-:/.[]@$%+>>
<<id:abc>>)def
<<id:ab"c>> def
<<id:"abc">>def
<<id:"ab""c">>def
<<id:"ab c">>def
<<id:"abc">>.def
id:"abc
"
id:)
id:
cid:xxx
<<mid:abc>> <<mid:abc/def>>
<<mid:abc%20def>>
<<mid:abc>>. <<mid:abc>>, <<mid:abc>>;
EOF
notmuch_date_sanitize < OUTPUT.raw > OUTPUT
test_expect_equal_file EXPECTED OUTPUT


test_begin_subtest "Show handles subprocess errors"
cat > notmuch_fail <<EOF
#!/bin/sh
echo This is output
echo This is an error >&2
exit 1
EOF
chmod a+x notmuch_fail
test_emacs "(let ((notmuch-command \"$PWD/notmuch_fail\"))
	       (with-current-buffer \"*Messages*\"
                  (let ((inhibit-read-only t)) (erase-buffer)))
	       (condition-case err
		   (notmuch-show \"*\")
		 (error (message \"%s\" (cadr err))))
	       (notmuch-test-wait)
	       (with-current-buffer \"*Messages*\"
		  (test-output \"MESSAGES\"))
	       (with-current-buffer \"*Notmuch errors*\"
		  (test-output \"ERROR\"))
	       (test-output))"
test_expect_equal "$(notmuch_emacs_error_sanitize notmuch_fail OUTPUT MESSAGES ERROR)" "\
=== OUTPUT ===
=== MESSAGES ===
This is an error (see *Notmuch errors* for more details)
=== ERROR ===
This is an error
command: YYY/notmuch_fail show --format\\=sexp --format-version\\=5 --decrypt\\=true --exclude\\=false \\' \\* \\'
exit status: 1
stderr:
This is an error
stdout:
This is output"

test_begin_subtest "text/enriched exploit mitigation"
add_message '[content-type]="text/enriched"
             [body]="
<x-display><param>(when (progn (read-only-mode -1) (insert ?p ?0 ?w ?n ?e ?d)) nil)</param>test</x-display>
"'
test_emacs '(notmuch-show "id:'$gen_msg_id'")
	(test-visible-output "OUTPUT.raw")'
output=$(head -1 OUTPUT.raw|cut -f1-4 -d' ')
test_expect_equal "$output" "Notmuch Test Suite <test_suite@notmuchmail.org>"

test_begin_subtest "multipart/alternative hides html by default"
test_emacs '(notmuch-show "id:cf0c4d610911171136h1713aa59w9cf9aa31f052ad0a@mail.gmail.com")
	    (test-visible-output)'
test_expect_equal_file $EXPECTED/notmuch-show-multipart-alternative OUTPUT

# switching to the crypto corpus, using gpg from here on:
add_gnupg_home
add_email_corpus crypto

test_begin_subtest "show decrypted message"
test_emacs '(notmuch-show "id:basic-encrypted@crypto.notmuchmail.org")
            (test-visible-output)'
test_expect_equal_file $EXPECTED/notmuch-show-decrypted-message OUTPUT

test_begin_subtest "show encrypted rfc822 message"
if ${TEST_EMACS} --quick --batch --eval '(kill-emacs (if (version< emacs-version "28") 0 1))'; then
    test_subtest_known_broken
fi
test_emacs '(notmuch-show "id:encrypted-rfc822-attachment@crypto.notmuchmail.org")
            (test-visible-output)'
test_expect_code 1 'fgrep "!!!" OUTPUT'

test_begin_subtest "show undecryptable message"
test_emacs '(notmuch-show "id:simple-encrypted@crypto.notmuchmail.org")
            (test-visible-output)'
test_expect_equal_file $EXPECTED/notmuch-show-undecryptable-message OUTPUT

test_begin_subtest "show encrypted message when not processing crypto"
test_emacs '(let ((notmuch-crypto-process-mime nil))
             (notmuch-show "id:basic-encrypted@crypto.notmuchmail.org")
             (test-visible-output))'
test_expect_equal_file $EXPECTED/notmuch-show-decrypted-message-no-crypto OUTPUT

test_begin_subtest "notmuch-show with nonexistent CWD"
tid=$(notmuch search --limit=1 --output=threads '*' | sed s/thread://)
test_emacs "(test-log-error
	      (let ((default-directory \"/nonexistent\"))
	        (notmuch-show \"$tid\")))"
test_expect_equal "$(cat MESSAGES)" "COMPLETE"

add_email_corpus attachment

test_begin_subtest "tar not inlined by default"
test_emacs '(notmuch-show "id:874llc2bkp.fsf@curie.anarc.at")
	(test-visible-output "OUTPUT")'
cat <<EOF > EXPECTED
Antoine Beaupré <anarcat@orangeseeds.org> (2018-03-19) (attachment inbox)
Subject: Re: bug: "no top level messages" crash on Zen email loops
To: David Bremner <david@tethera.net>, notmuch@notmuchmail.org
Date: Mon, 19 Mar 2018 13:56:54 -0400

[ multipart/mixed ]
[ text/plain ]
And obviously I forget the frigging attachment.
[ zendesk-email-loop2.tgz: application/x-gtar-compressed ]
[ text/plain ]

PS: don't we have a "you forgot to actually attach the damn file" plugin
when we detect the word "attachment" and there's no attach? :p
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
