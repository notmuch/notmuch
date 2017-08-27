#!/usr/bin/env bash
test_description="output of multipart message"
. ./test-lib.sh || exit 1

cat <<EOF > embedded_message_body
Content-Type: multipart/alternative; boundary="==-=-=="

--==-=-==
Content-Type: text/html

<p>This is an embedded message, with a multipart/alternative part.</p>

--==-=-==
Content-Type: text/plain

This is an embedded message, with a multipart/alternative part.

--==-=-==--
EOF
cat <<EOF > embedded_message
From: Carl Worth <cworth@cworth.org>
To: cworth@cworth.org
Subject: html message
Date: Fri, 05 Jan 2001 15:42:57 +0000
User-Agent: Notmuch/0.5 (http://notmuchmail.org) Emacs/23.3.1 (i486-pc-linux-gnu)
Message-ID: <87liy5ap01.fsf@yoom.home.cworth.org>
MIME-Version: 1.0
EOF

cat embedded_message_body >> embedded_message

cat <<EOF > multipart_body
Content-Type: multipart/signed; boundary="==-=-=";
	micalg=pgp-sha1; protocol="application/pgp-signature"

--==-=-=
Content-Type: multipart/mixed; boundary="=-=-="

--=-=-=
Content-Type: message/rfc822
Content-Disposition: inline

EOF

cat embedded_message >> multipart_body
cat <<EOF >> multipart_body

--=-=-=
Content-Disposition: attachment; filename=attachment

This is a text attachment.

--=-=-=

And this message is signed.

-Carl

--=-=-=--

--==-=-=
Content-Type: application/pgp-signature

-----BEGIN PGP SIGNATURE-----
Version: GnuPG v1.4.11 (GNU/Linux)

iEYEARECAAYFAk3SA/gACgkQ6JDdNq8qSWj0sACghqVJEQJUs3yV8zbTzhgnSIcD
W6cAmQE4dcYrx/LPLtYLZm1jsGauE5hE
=zkga
-----END PGP SIGNATURE-----
--==-=-=--
EOF

cat <<EOF > ${MAIL_DIR}/multipart
From: Carl Worth <cworth@cworth.org>
To: cworth@cworth.org
Subject: Multipart message
Date: Fri, 05 Jan 2001 15:43:57 +0000
User-Agent: Notmuch/0.5 (http://notmuchmail.org) Emacs/23.3.1 (i486-pc-linux-gnu)
Message-ID: <87liy5ap00.fsf@yoom.home.cworth.org>
MIME-Version: 1.0
EOF

cat multipart_body >> ${MAIL_DIR}/multipart

cat <<EOF > ${MAIL_DIR}/base64-part-with-crlf
From: Carl Worth <cworth@cworth.org>
To: cworth@cworth.org
Subject: Test message with a BASE64 encoded binary containing CRLF pair
Date: Fri, 05 Jan 2001 15:43:57 +0000
User-Agent: Notmuch/0.5 (http://notmuchmail.org) Emacs/23.3.1 (i486-pc-linux-gnu)
Message-ID: <base64-part-with-crlf>
MIME-Version: 1.0
Content-Type: multipart/mixed; boundary="==-=-=";

--==-=-=

The attached BASE64-encoded part expands to a binary containing a CRLF
pair (that is one bye of 0x0D followed by one byte of 0x0A). This is
designed to ensure that notmuch is not corrupting the output of this
part by converting the CRLF pair to an LF only (as would be appropriate
for display of a text part on a Linux system, for example).

The part should be a 3-byte file with the following sequence of 3
hexadecimal bytes:

	EF 0D 0A

--==-=-=
Content-Type: application/octet-stream
Content-Disposition: attachment; filename=crlf.bin
Content-Transfer-Encoding: base64

7w0K
--==-=-=--
EOF

cat <<EOF > content_types
From: Todd <todd@example.com>
To: todd@example.com
Subject: odd content types
Date: Mon, 12 Jan 2014 18:12:32 +0000
User-Agent: Notmuch/0.5 (http://notmuchmail.org) Emacs/23.3.1 (i486-pc-linux-gnu)
Message-ID: <KfjfO2WJBw2hrV2p0gjT@example.com>
MIME-Version: 1.0
Content-Type: multipart/alternative; boundary="==-=-=="

--==-=-==
Content-Type: application/unique_identifier

<p>This is an embedded message, with a multipart/alternative part.</p>

--==-=-==
Content-Type: text/some_other_identifier

This is an embedded message, with a multipart/alternative part.

--==-=-==--
EOF
cat content_types >> ${MAIL_DIR}/odd_content_type
notmuch new > /dev/null

test_begin_subtest "--format=text --part=0, full message"
notmuch show --format=text --part=0 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
message{ id:87liy5ap00.fsf@yoom.home.cworth.org depth:0 match:1 excluded:0 filename:${MAIL_DIR}/multipart
header{
Carl Worth <cworth@cworth.org> (2001-01-05) (attachment inbox signed unread)
Subject: Multipart message
From: Carl Worth <cworth@cworth.org>
To: cworth@cworth.org
Date: Fri, 05 Jan 2001 15:43:57 +0000
header}
body{
part{ ID: 1, Content-type: multipart/signed
part{ ID: 2, Content-type: multipart/mixed
part{ ID: 3, Content-type: message/rfc822
header{
Subject: html message
From: Carl Worth <cworth@cworth.org>
To: cworth@cworth.org
Date: Fri, 05 Jan 2001 15:42:57 +0000
header}
body{
part{ ID: 4, Content-type: multipart/alternative
part{ ID: 5, Content-type: text/html
Non-text part: text/html
part}
part{ ID: 6, Content-type: text/plain
This is an embedded message, with a multipart/alternative part.
part}
part}
body}
part}
attachment{ ID: 7, Filename: attachment, Content-type: text/plain
This is a text attachment.
attachment}
part{ ID: 8, Content-type: text/plain
And this message is signed.

-Carl
part}
part}
part{ ID: 9, Content-type: application/pgp-signature
Non-text part: application/pgp-signature
part}
part}
body}
message}
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=text --part=1, message body"
notmuch show --format=text --part=1 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
part{ ID: 1, Content-type: multipart/signed
part{ ID: 2, Content-type: multipart/mixed
part{ ID: 3, Content-type: message/rfc822
header{
Subject: html message
From: Carl Worth <cworth@cworth.org>
To: cworth@cworth.org
Date: Fri, 05 Jan 2001 15:42:57 +0000
header}
body{
part{ ID: 4, Content-type: multipart/alternative
part{ ID: 5, Content-type: text/html
Non-text part: text/html
part}
part{ ID: 6, Content-type: text/plain
This is an embedded message, with a multipart/alternative part.
part}
part}
body}
part}
attachment{ ID: 7, Filename: attachment, Content-type: text/plain
This is a text attachment.
attachment}
part{ ID: 8, Content-type: text/plain
And this message is signed.

-Carl
part}
part}
part{ ID: 9, Content-type: application/pgp-signature
Non-text part: application/pgp-signature
part}
part}
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=text --part=2, multipart/mixed"
notmuch show --format=text --part=2 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
part{ ID: 2, Content-type: multipart/mixed
part{ ID: 3, Content-type: message/rfc822
header{
Subject: html message
From: Carl Worth <cworth@cworth.org>
To: cworth@cworth.org
Date: Fri, 05 Jan 2001 15:42:57 +0000
header}
body{
part{ ID: 4, Content-type: multipart/alternative
part{ ID: 5, Content-type: text/html
Non-text part: text/html
part}
part{ ID: 6, Content-type: text/plain
This is an embedded message, with a multipart/alternative part.
part}
part}
body}
part}
attachment{ ID: 7, Filename: attachment, Content-type: text/plain
This is a text attachment.
attachment}
part{ ID: 8, Content-type: text/plain
And this message is signed.

-Carl
part}
part}
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=text --part=3, rfc822 part"
notmuch show --format=text --part=3 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
part{ ID: 3, Content-type: message/rfc822
header{
Subject: html message
From: Carl Worth <cworth@cworth.org>
To: cworth@cworth.org
Date: Fri, 05 Jan 2001 15:42:57 +0000
header}
body{
part{ ID: 4, Content-type: multipart/alternative
part{ ID: 5, Content-type: text/html
Non-text part: text/html
part}
part{ ID: 6, Content-type: text/plain
This is an embedded message, with a multipart/alternative part.
part}
part}
body}
part}
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=text --part=4, rfc822's multipart"
notmuch show --format=text --part=4 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
part{ ID: 4, Content-type: multipart/alternative
part{ ID: 5, Content-type: text/html
Non-text part: text/html
part}
part{ ID: 6, Content-type: text/plain
This is an embedded message, with a multipart/alternative part.
part}
part}
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=text --part=5, rfc822's html part"
notmuch show --format=text --part=5 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
part{ ID: 5, Content-type: text/html
Non-text part: text/html
part}
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=text --part=6, rfc822's text part"
notmuch show --format=text --part=6 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
part{ ID: 6, Content-type: text/plain
This is an embedded message, with a multipart/alternative part.
part}
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=text --part=7, inline attachement"
notmuch show --format=text --part=7 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
attachment{ ID: 7, Filename: attachment, Content-type: text/plain
This is a text attachment.
attachment}
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=text --part=8, plain text part"
notmuch show --format=text --part=8 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
part{ ID: 8, Content-type: text/plain
And this message is signed.

-Carl
part}
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=text --part=9, pgp signature (unverified)"
notmuch show --format=text --part=9 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
part{ ID: 9, Content-type: application/pgp-signature
Non-text part: application/pgp-signature
part}
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=text --part=8, no part, expect error"
test_expect_success "notmuch show --format=text --part=8 'id:87liy5ap00.fsf@yoom.home.cworth.org'"

test_begin_subtest "--format=json --part=0, full message"
notmuch show --format=json --part=0 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
{"id": "87liy5ap00.fsf@yoom.home.cworth.org", "match": true, "excluded": false, "filename": ["${MAIL_DIR}/multipart"], "timestamp": 978709437, "date_relative": "2001-01-05", "tags": ["attachment","inbox","signed","unread"], "headers": {"Subject": "Multipart message", "From": "Carl Worth <cworth@cworth.org>", "To": "cworth@cworth.org", "Date": "Fri, 05 Jan 2001 15:43:57 +0000"}, "body": [
{"id": 1, "content-type": "multipart/signed", "content": [
{"id": 2, "content-type": "multipart/mixed", "content": [
{"id": 3, "content-type": "message/rfc822", "content-disposition": "inline", "content": [{"headers": {"Subject": "html message", "From": "Carl Worth <cworth@cworth.org>", "To": "cworth@cworth.org", "Date": "Fri, 05 Jan 2001 15:42:57 +0000"}, "body": [
{"id": 4, "content-type": "multipart/alternative", "content": [
{"id": 5, "content-type": "text/html", "content-length": 71},
{"id": 6, "content-type": "text/plain", "content": "This is an embedded message, with a multipart/alternative part.\n"}]}]}]}, 
{"id": 7, "content-type": "text/plain", "content-disposition": "attachment", "filename": "attachment", "content": "This is a text attachment.\n"},
{"id": 8, "content-type": "text/plain", "content": "And this message is signed.\n\n-Carl\n"}]}, 
{"id": 9, "content-type": "application/pgp-signature", "content-length": 197}]}]}
EOF
test_expect_equal_json "$(cat OUTPUT)" "$(cat EXPECTED)"

test_begin_subtest "--format=json --part=1, message body"
notmuch show --format=json --part=1 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
{"id": 1, "content-type": "multipart/signed", "content": [
{"id": 2, "content-type": "multipart/mixed", "content": [
{"id": 3, "content-type": "message/rfc822", "content-disposition": "inline", "content": [{"headers": {"Subject": "html message", "From": "Carl Worth <cworth@cworth.org>", "To": "cworth@cworth.org", "Date": "Fri, 05 Jan 2001 15:42:57 +0000"}, "body": [
{"id": 4, "content-type": "multipart/alternative", "content": [
{"id": 5, "content-type": "text/html", "content-length": 71},
{"id": 6, "content-type": "text/plain", "content": "This is an embedded message, with a multipart/alternative part.\n"}]}]}]}, 
{"id": 7, "content-type": "text/plain", "content-disposition": "attachment", "filename": "attachment", "content": "This is a text attachment.\n"},
{"id": 8, "content-type": "text/plain", "content": "And this message is signed.\n\n-Carl\n"}]}, 
{"id": 9, "content-type": "application/pgp-signature", "content-length": 197}]}
EOF
test_expect_equal_json "$(cat OUTPUT)" "$(cat EXPECTED)"

test_begin_subtest "--format=json --part=2, multipart/mixed"
notmuch show --format=json --part=2 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
{"id": 2, "content-type": "multipart/mixed", "content": [
{"id": 3, "content-type": "message/rfc822", "content-disposition": "inline", "content": [{"headers": {"Subject": "html message", "From": "Carl Worth <cworth@cworth.org>", "To": "cworth@cworth.org", "Date": "Fri, 05 Jan 2001 15:42:57 +0000"}, "body": [
{"id": 4, "content-type": "multipart/alternative", "content": [
{"id": 5, "content-type": "text/html", "content-length": 71},
{"id": 6, "content-type": "text/plain", "content": "This is an embedded message, with a multipart/alternative part.\n"}]}]}]}, 
{"id": 7, "content-type": "text/plain", "content-disposition": "attachment", "filename": "attachment", "content": "This is a text attachment.\n"},
{"id": 8, "content-type": "text/plain", "content": "And this message is signed.\n\n-Carl\n"}]}
EOF
test_expect_equal_json "$(cat OUTPUT)" "$(cat EXPECTED)"

test_begin_subtest "--format=json --part=3, rfc822 part"
notmuch show --format=json --part=3 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
{"id": 3, "content-type": "message/rfc822", "content-disposition": "inline", "content": [{"headers": {"Subject": "html message", "From": "Carl Worth <cworth@cworth.org>", "To": "cworth@cworth.org", "Date": "Fri, 05 Jan 2001 15:42:57 +0000"}, "body": [
{"id": 4, "content-type": "multipart/alternative", "content": [
{"id": 5, "content-type": "text/html", "content-length": 71},
{"id": 6, "content-type": "text/plain", "content": "This is an embedded message, with a multipart/alternative part.\n"}]}]}]}
EOF
test_expect_equal_json "$(cat OUTPUT)" "$(cat EXPECTED)"

test_begin_subtest "--format=json --part=4, rfc822's multipart/alternative"
notmuch show --format=json --part=4 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
{"id": 4, "content-type": "multipart/alternative", "content": [
{"id": 5, "content-type": "text/html", "content-length": 71},
{"id": 6, "content-type": "text/plain", "content": "This is an embedded message, with a multipart/alternative part.\n"}]}
EOF
test_expect_equal_json "$(cat OUTPUT)" "$(cat EXPECTED)"

test_begin_subtest "--format=json --part=5, rfc822's html part"
notmuch show --format=json --part=5 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
{"id": 5, "content-type": "text/html", "content-length": 71}
EOF
test_expect_equal_json "$(cat OUTPUT)" "$(cat EXPECTED)"

test_begin_subtest "--format=json --part=6, rfc822's text part"
notmuch show --format=json --part=6 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
{"id": 6, "content-type": "text/plain", "content": "This is an embedded message, with a multipart/alternative part.\n"}
EOF
test_expect_equal_json "$(cat OUTPUT)" "$(cat EXPECTED)"

test_begin_subtest "--format=json --part=7, inline attachment"
notmuch show --format=json --part=7 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
{"id": 7,
 "content-type": "text/plain",
 "filename": "attachment",
 "content": "This is a text attachment.\n",
 "content-disposition": "attachment"}
EOF
test_expect_equal_json "$(cat OUTPUT)" "$(cat EXPECTED)"

test_begin_subtest "--format=json --part=8, plain text part"
notmuch show --format=json --part=8 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
{"id": 8, "content-type": "text/plain", "content": "And this message is signed.\n\n-Carl\n"}
EOF
test_expect_equal_json "$(cat OUTPUT)" "$(cat EXPECTED)"

test_begin_subtest "--format=json --part=9, pgp signature (unverified)"
notmuch show --format=json --part=9 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
{"id": 9, "content-type": "application/pgp-signature", "content-length": 197}
EOF
test_expect_equal_json "$(cat OUTPUT)" "$(cat EXPECTED)"

test_begin_subtest "--format=json --part=10, no part, expect error"
test_expect_success "notmuch show --format=json --part=10 'id:87liy5ap00.fsf@yoom.home.cworth.org'"

test_begin_subtest "--format=raw"
notmuch show --format=raw 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
test_expect_equal_file "${MAIL_DIR}"/multipart  OUTPUT

test_begin_subtest "--format=raw --part=0, full message"
notmuch show --format=raw --part=0 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
test_expect_equal_file "${MAIL_DIR}"/multipart OUTPUT

test_begin_subtest "--format=raw --part=1, message body"
test_subtest_broken_gmime_2
notmuch show --format=raw --part=1 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
test_expect_equal_file multipart_body OUTPUT

test_begin_subtest "--format=raw --part=2, multipart/mixed"
notmuch show --format=raw --part=2 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
Content-Type: multipart/mixed; boundary="=-=-="

--=-=-=
Content-Type: message/rfc822
Content-Disposition: inline

From: Carl Worth <cworth@cworth.org>
To: cworth@cworth.org
Subject: html message
Date: Fri, 05 Jan 2001 15:42:57 +0000
User-Agent: Notmuch/0.5 (http://notmuchmail.org) Emacs/23.3.1 (i486-pc-linux-gnu)
Message-ID: <87liy5ap01.fsf@yoom.home.cworth.org>
MIME-Version: 1.0
Content-Type: multipart/alternative; boundary="==-=-=="

--==-=-==
Content-Type: text/html

<p>This is an embedded message, with a multipart/alternative part.</p>

--==-=-==
Content-Type: text/plain

This is an embedded message, with a multipart/alternative part.

--==-=-==--

--=-=-=
Content-Disposition: attachment; filename=attachment

This is a text attachment.

--=-=-=

And this message is signed.

-Carl

--=-=-=--
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=raw --part=3, rfc822 part"
notmuch show --format=raw --part=3 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
test_expect_equal_file embedded_message OUTPUT

test_begin_subtest "--format=raw --part=4, rfc822's multipart"
test_subtest_broken_gmime_2
notmuch show --format=raw --part=4 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
test_expect_equal_file embedded_message_body OUTPUT

test_begin_subtest "--format=raw --part=5, rfc822's html part"
notmuch show --format=raw --part=5 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
<p>This is an embedded message, with a multipart/alternative part.</p>
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=raw --part=6, rfc822's text part"
notmuch show --format=raw --part=6 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
This is an embedded message, with a multipart/alternative part.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=raw --part=7, inline attachment"
notmuch show --format=raw --part=7 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
This is a text attachment.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=raw --part=8, plain text part"
notmuch show --format=raw --part=8 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
And this message is signed.

-Carl
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=raw --part=9, pgp signature (unverified)"
notmuch show --format=raw --part=9 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
# output should *not* include newline
echo >>OUTPUT
cat <<EOF >EXPECTED
-----BEGIN PGP SIGNATURE-----
Version: GnuPG v1.4.11 (GNU/Linux)

iEYEARECAAYFAk3SA/gACgkQ6JDdNq8qSWj0sACghqVJEQJUs3yV8zbTzhgnSIcD
W6cAmQE4dcYrx/LPLtYLZm1jsGauE5hE
=zkga
-----END PGP SIGNATURE-----
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=raw --part=10, no part, expect error"
test_expect_success "notmuch show --format=raw --part=8 'id:87liy5ap00.fsf@yoom.home.cworth.org'"

test_begin_subtest "--format=mbox"
notmuch show --format=mbox 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
printf "From cworth@cworth.org Fri Jan  5 15:43:57 2001\n" >EXPECTED
cat "${MAIL_DIR}"/multipart >>EXPECTED
# mbox output is expected to include a blank line
echo >>EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "--format=mbox --part=1, incompatible, expect error"
test_expect_success "! notmuch show --format=mbox --part=1 'id:87liy5ap00.fsf@yoom.home.cworth.org'"

test_begin_subtest "'notmuch reply' to a multipart message"
notmuch reply 'id:87liy5ap00.fsf@yoom.home.cworth.org' >OUTPUT
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: Multipart message
To: Carl Worth <cworth@cworth.org>, cworth@cworth.org
In-Reply-To: <87liy5ap00.fsf@yoom.home.cworth.org>
References: <87liy5ap00.fsf@yoom.home.cworth.org>

On Fri, 05 Jan 2001 15:43:57 +0000, Carl Worth <cworth@cworth.org> wrote:
> From: Carl Worth <cworth@cworth.org>
> To: cworth@cworth.org
> Subject: html message
> Date: Fri, 05 Jan 2001 15:42:57 +0000
>
Non-text part: text/html
> This is an embedded message, with a multipart/alternative part.
> This is a text attachment.
> And this message is signed.
> 
> -Carl
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "'notmuch reply' to a multipart message with json format"
notmuch reply --format=json 'id:87liy5ap00.fsf@yoom.home.cworth.org' | notmuch_json_show_sanitize >OUTPUT
notmuch_json_show_sanitize <<EOF >EXPECTED
{"reply-headers": {"Subject": "Re: Multipart message",
 "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
 "To": "Carl Worth <cworth@cworth.org>, cworth@cworth.org",
 "In-reply-to": "<87liy5ap00.fsf@yoom.home.cworth.org>",
 "References": "<87liy5ap00.fsf@yoom.home.cworth.org>"},
 "original": {"id": "XXXXX",
 "match": false,
 "excluded": false,
 "filename": ["YYYYY"],
 "timestamp": 978709437,
 "date_relative": "2001-01-05",
 "tags": ["attachment","inbox","signed","unread"],
 "headers": {"Subject": "Multipart message",
 "From": "Carl Worth <cworth@cworth.org>",
 "To": "cworth@cworth.org",
 "Date": "Fri, 05 Jan 2001 15:43:57 +0000"},
 "body": [{"id": 1,
 "content-type": "multipart/signed",
 "content": [{"id": 2,
 "content-type": "multipart/mixed",
 "content": [{"id": 3,
 "content-type": "message/rfc822",
 "content-disposition": "inline",
 "content": [{"headers": {"Subject": "html message",
 "From": "Carl Worth <cworth@cworth.org>",
 "To": "cworth@cworth.org",
 "Date": "Fri, 05 Jan 2001 15:42:57 +0000"},
 "body": [{"id": 4,
 "content-type": "multipart/alternative",
 "content": [{"id": 5,
 "content-type": "text/html",
 "content-length": 71},
 {"id": 6,
 "content-type": "text/plain",
 "content": "This is an embedded message, with a multipart/alternative part.\n"}]}]}]},
 {"id": 7,
 "content-type": "text/plain",
 "content-disposition": "attachment",
 "filename": "attachment",
 "content": "This is a text attachment.\n"},
 {"id": 8,
 "content-type": "text/plain",
 "content": "And this message is signed.\n\n-Carl\n"}]},
 {"id": 9,
 "content-type": "application/pgp-signature",
 "content-length": 197}]}]}}
EOF
test_expect_equal_json "$(cat OUTPUT)" "$(cat EXPECTED)"

test_begin_subtest "'notmuch show --part' does not corrupt a part with CRLF pair"
notmuch show --format=raw --part=3 id:base64-part-with-crlf > crlf.out
echo -n -e "\xEF\x0D\x0A" > crlf.expected
test_expect_equal_file crlf.out crlf.expected


# The ISO-8859-1 encoding of U+00BD is a single byte: octal 275
# (Portability note: Dollar-Single ($'...', ANSI C-style escape sequences)
# quoting works on bash, ksh, zsh, *BSD sh but not on dash, ash nor busybox sh)
readonly u_00bd_latin1=$'\275'

# The Unicode fraction symbol 1/2 is U+00BD and is encoded
# in UTF-8 as two bytes: octal 302 275
readonly u_00bd_utf8=$'\302\275'

cat <<EOF > ${MAIL_DIR}/include-html
From: A <a@example.com>
To: B <b@example.com>
Subject: html message
Date: Sat, 01 January 2000 00:00:00 +0000
Message-ID: <htmlmessage>
MIME-Version: 1.0
Content-Type: multipart/alternative; boundary="==-=="

--==-==
Content-Type: text/html; charset=UTF-8

<p>0.5 equals ${u_00bd_utf8}</p>

--==-==
Content-Type: text/html; charset=ISO-8859-1

<p>0.5 equals ${u_00bd_latin1}</p>

--==-==
Content-Type: text/plain; charset=UTF-8

0.5 equals ${u_00bd_utf8}

--==-==--
EOF

notmuch new > /dev/null

cat_expected_head ()
{
        cat <<EOF
[[[{"id": "htmlmessage", "match":true, "excluded": false, "date_relative":"2000-01-01",
   "timestamp": 946684800,
   "filename": ["${MAIL_DIR}/include-html"],
   "tags": ["inbox", "unread"],
   "headers": { "Date": "Sat, 01 Jan 2000 00:00:00 +0000", "From": "A <a@example.com>",
                "Subject": "html message", "To": "B <b@example.com>"},
   "body": [{
     "content-type": "multipart/alternative", "id": 1,
EOF
}

cat_expected_head > EXPECTED.nohtml
cat <<EOF >> EXPECTED.nohtml
"content": [
  { "id": 2, "content-charset": "UTF-8", "content-length": 21, "content-type": "text/html"},
  { "id": 3, "content-charset": "ISO-8859-1", "content-length": 20, "content-type": "text/html"},
  { "id": 4, "content-type": "text/plain", "content": "0.5 equals \\u00bd\\n"}
]}]},[]]]]
EOF

# Both the UTF-8 and ISO-8859-1 part should have U+00BD
cat_expected_head > EXPECTED.withhtml
cat <<EOF >> EXPECTED.withhtml
"content": [
  { "id": 2, "content-type": "text/html", "content": "<p>0.5 equals \\u00bd</p>\\n"},
  { "id": 3, "content-type": "text/html", "content": "<p>0.5 equals \\u00bd</p>\\n"},
  { "id": 4, "content-type": "text/plain", "content": "0.5 equals \\u00bd\\n"}
]}]},[]]]]
EOF

test_begin_subtest "html parts excluded by default"
notmuch show --format=json id:htmlmessage > OUTPUT
test_expect_equal_json "$(cat OUTPUT)" "$(cat EXPECTED.nohtml)"

test_begin_subtest "html parts included"
notmuch show --format=json --include-html id:htmlmessage > OUTPUT
test_expect_equal_json "$(cat OUTPUT)" "$(cat EXPECTED.withhtml)"

test_begin_subtest "indexes mime-type #1"
output=$(notmuch search mimetype:application/unique_identifier | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2014-01-12 [1/1] Todd; odd content types (inbox unread)"

test_begin_subtest "indexes mime-type #2"
output=$(notmuch search mimetype:text/some_other_identifier | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2014-01-12 [1/1] Todd; odd content types (inbox unread)"

test_begin_subtest "indexes mime-type #3"
output=$(notmuch search from:todd and mimetype:multipart/alternative | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2014-01-12 [1/1] Todd; odd content types (inbox unread)"

test_begin_subtest "case of Content-Disposition doesn't matter for indexing"
cat <<EOF > ${MAIL_DIR}/content-disposition
Return-path: <david@tethera.net>
Envelope-to: david@tethera.net
Delivery-date: Sun, 04 Oct 2015 09:16:03 -0300
Received: from gitolite.debian.net ([87.98.215.224])
	by yantan.tethera.net with esmtps (TLS1.2:DHE_RSA_AES_128_CBC_SHA1:128)
	(Exim 4.80)
	(envelope-from <david@tethera.net>)
	id 1ZiiCx-0007iz-RK
	for david@tethera.net; Sun, 04 Oct 2015 09:16:03 -0300
Received: from remotemail by gitolite.debian.net with local (Exim 4.80)
	(envelope-from <david@tethera.net>)
	id 1ZiiC8-0002Rz-Uf; Sun, 04 Oct 2015 12:15:12 +0000
Received: (nullmailer pid 28621 invoked by uid 1000); Sun, 04 Oct 2015
 12:14:53 -0000
From: David Bremner <david@tethera.net>
To: David Bremner <david@tethera.net>
Subject: test attachment
User-Agent: Notmuch/0.20.2+93~g33c8777 (http://notmuchmail.org) Emacs/24.5.1
 (x86_64-pc-linux-gnu)
Date: Sun, 04 Oct 2015 09:14:53 -0300
Message-ID: <87io6m96f6.fsf@zancas.localnet>
MIME-Version: 1.0
Content-Type: multipart/mixed; boundary="=-=-="

--=-=-=
Content-Type: text/plain
Content-Disposition: ATTACHMENT; filename=hello.txt
Content-Description: this is a very exciting file

hello

--=-=-=
Content-Type: text/plain


world

--=-=-=--

EOF
NOTMUCH_NEW

cat <<EOF > EXPECTED
attachment
inbox
unread
EOF

notmuch search --output=tags id:87io6m96f6.fsf@zancas.localnet > OUTPUT
test_expect_equal_file EXPECTED OUTPUT
test_done
