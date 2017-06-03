#!/usr/bin/env bash

# TODO:
# - decryption/verification with signer key not available
# - verification of signatures from expired/revoked keys

test_description='PGP/MIME signature verification and decryption'
. ./test-lib.sh || exit 1

add_gnupg_home ()
{
    local output
    [ -d ${GNUPGHOME} ] && return
    _gnupg_exit () { gpgconf --kill all 2>/dev/null || true; }
    at_exit_function _gnupg_exit
    mkdir -m 0700 "$GNUPGHOME"
    gpg --no-tty --import <$TEST_DIRECTORY/gnupg-secret-key.asc >"$GNUPGHOME"/import.log 2>&1
    test_debug "cat $GNUPGHOME/import.log"
    if (gpg --quick-random --version >/dev/null 2>&1) ; then
	echo quick-random >> "$GNUPGHOME"/gpg.conf
    elif (gpg --debug-quick-random --version >/dev/null 2>&1) ; then
	echo debug-quick-random >> "$GNUPGHOME"/gpg.conf
    fi
    echo no-emit-version >> "$GNUPGHOME"/gpg.conf
}

##################################################

add_gnupg_home
# Change this if we ship a new test key
FINGERPRINT="5AEAB11F5E33DCE875DDB75B6D92612D94E46381"

test_begin_subtest "emacs delivery of signed message"
test_expect_success \
'emacs_fcc_message \
    "test signed message 001" \
    "This is a test signed message." \
    "(mml-secure-message-sign)"'

test_begin_subtest "signature verification"
output=$(notmuch show --format=json --verify subject:"test signed message 001" \
    | notmuch_json_show_sanitize \
    | sed -e 's|"created": [1234567890]*|"created": 946728000|')
expected='[[[{"id": "XXXXX",
 "match": true,
 "excluded": false,
 "filename": ["YYYYY"],
 "timestamp": 946728000,
 "date_relative": "2000-01-01",
 "tags": ["inbox","signed"],
 "headers": {"Subject": "test signed message 001",
 "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
 "To": "test_suite@notmuchmail.org",
 "Date": "Sat, 01 Jan 2000 12:00:00 +0000"},
 "body": [{"id": 1,
 "sigstatus": [{"status": "good",
 "fingerprint": "'$FINGERPRINT'",
 "created": 946728000}],
 "content-type": "multipart/signed",
 "content": [{"id": 2,
 "content-type": "text/plain",
 "content": "This is a test signed message.\n"},
 {"id": 3,
 "content-type": "application/pgp-signature",
 "content-length": "NONZERO"}]}]},
 []]]]'
test_expect_equal_json \
    "$output" \
    "$expected"

test_begin_subtest "detection of modified signed contents"
emacs_fcc_message \
    "bad signed message 001" \
    "Incriminating stuff. This is a test signed message." \
    "(mml-secure-message-sign)"

file=$(notmuch search --output=files subject:"bad signed message 001")

sed -i 's/Incriminating stuff. //' ${file}

output=$(notmuch show --format=json --verify subject:"bad signed message 001" \
    | notmuch_json_show_sanitize \
    | sed -e 's|"created": [1234567890]*|"created": 946728000|')
expected='[[[{"id": "XXXXX",
 "match": true,
 "excluded": false,
 "filename": ["YYYYY"],
 "timestamp": 946728000,
 "date_relative": "2000-01-01",
 "tags": ["inbox","signed"],
 "headers": {"Subject": "bad signed message 001",
 "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
 "To": "test_suite@notmuchmail.org",
 "Date": "Sat, 01 Jan 2000 12:00:00 +0000"},
 "body": [{"id": 1,
 "sigstatus": [{"status": "bad",
 "keyid": "'$(echo $FINGERPRINT | cut -c 25-)'"}],
 "content-type": "multipart/signed",
 "content": [{"id": 2,
 "content-type": "text/plain",
 "content": "This is a test signed message.\n"},
 {"id": 3,
 "content-type": "application/pgp-signature",
 "content-length": "NONZERO"}]}]},
 []]]]'
test_expect_equal_json \
    "$output" \
    "$expected"

test_begin_subtest "corrupted pgp/mime signature"
emacs_fcc_message \
    "bad signed message 002" \
    "Incriminating stuff. This is a test signed message." \
    "(mml-secure-message-sign)"

file=$(notmuch search --output=files subject:"bad signed message 002")

awk '/-----BEGIN PGP SIGNATURE-----/{flag=1;print;next} \
     /-----END PGP SIGNATURE-----/{flag=0;print;next} \
     flag{gsub(/[A-Za-z]/,"0");print}!flag{print}' $file > $file.new

rm $file
mv $file.new $file

output=$(notmuch show --format=json --verify subject:"bad signed message 002" \
    | notmuch_json_show_sanitize \
    | sed -e 's|"created": [1234567890]*|"created": 946728000|')
expected='[[[{"id": "XXXXX",
 "match": true,
 "excluded": false,
 "filename": ["YYYYY"],
 "timestamp": 946728000,
 "date_relative": "2000-01-01",
 "tags": ["inbox","signed"],
 "headers": {"Subject": "bad signed message 002",
 "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
 "To": "test_suite@notmuchmail.org",
 "Date": "Sat, 01 Jan 2000 12:00:00 +0000"},
 "body": [{"id": 1,
 "sigstatus": [],
 "content-type": "multipart/signed",
 "content": [{"id": 2,
 "content-type": "text/plain",
 "content": "Incriminating stuff. This is a test signed message.\n"},
 {"id": 3,
 "content-type": "application/pgp-signature",
 "content-length": "NONZERO"}]}]},
 []]]]'
test_expect_equal_json \
    "$output" \
    "$expected"

test_begin_subtest "signature verification with full owner trust"
test_subtest_broken_gmime_2
# give the key full owner trust
echo "${FINGERPRINT}:6:" | gpg --no-tty --import-ownertrust >>"$GNUPGHOME"/trust.log 2>&1
gpg --no-tty --check-trustdb >>"$GNUPGHOME"/trust.log 2>&1
output=$(notmuch show --format=json --verify subject:"test signed message 001" \
    | notmuch_json_show_sanitize \
    | sed -e 's|"created": [1234567890]*|"created": 946728000|')
expected='[[[{"id": "XXXXX",
 "match": true,
 "excluded": false,
 "filename": ["YYYYY"],
 "timestamp": 946728000,
 "date_relative": "2000-01-01",
 "tags": ["inbox","signed"],
 "headers": {"Subject": "test signed message 001",
 "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
 "To": "test_suite@notmuchmail.org",
 "Date": "Sat, 01 Jan 2000 12:00:00 +0000"},
 "body": [{"id": 1,
 "sigstatus": [{"status": "good",
 "fingerprint": "'$FINGERPRINT'",
 "created": 946728000,
 "userid": "Notmuch Test Suite <test_suite@notmuchmail.org> (INSECURE!)"}],
 "content-type": "multipart/signed",
 "content": [{"id": 2,
 "content-type": "text/plain",
 "content": "This is a test signed message.\n"},
 {"id": 3,
 "content-type": "application/pgp-signature",
 "content-length": "NONZERO"}]}]},
 []]]]'
test_expect_equal_json \
    "$output" \
    "$expected"

test_begin_subtest "signature verification with signer key unavailable"
# move the gnupghome temporarily out of the way
mv "${GNUPGHOME}"{,.bak}
output=$(notmuch show --format=json --verify subject:"test signed message 001" \
    | notmuch_json_show_sanitize \
    | sed -e 's|"created": [1234567890]*|"created": 946728000|')
expected='[[[{"id": "XXXXX",
 "match": true,
 "excluded": false,
 "filename": ["YYYYY"],
 "timestamp": 946728000,
 "date_relative": "2000-01-01",
 "tags": ["inbox","signed"],
 "headers": {"Subject": "test signed message 001",
 "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
 "To": "test_suite@notmuchmail.org",
 "Date": "Sat, 01 Jan 2000 12:00:00 +0000"},
 "body": [{"id": 1,
 "sigstatus": [{"status": "error",
 "keyid": "'$(echo $FINGERPRINT | cut -c 25-)'",
 "errors": {"key-missing": true}}],
 "content-type": "multipart/signed",
 "content": [{"id": 2,
 "content-type": "text/plain",
 "content": "This is a test signed message.\n"},
 {"id": 3,
 "content-type": "application/pgp-signature",
 "content-length": "NONZERO"}]}]},
 []]]]'
test_expect_equal_json \
    "$output" \
    "$expected"
mv "${GNUPGHOME}"{.bak,}

test_begin_subtest "emacs delivery of encrypted message with attachment"
# create a test encrypted message with attachment
cat <<EOF >TESTATTACHMENT
This is a test file.
EOF
test_expect_success \
'emacs_fcc_message \
    "test encrypted message 001" \
    "This is a test encrypted message.\n" \
    "(mml-attach-file \"TESTATTACHMENT\") (mml-secure-message-encrypt)"'

test_begin_subtest "decryption, --format=text"
output=$(notmuch show --format=text --decrypt subject:"test encrypted message 001" \
    | notmuch_show_sanitize_all \
    | sed -e 's|"created": [1234567890]*|"created": 946728000|')
expected='message{ id:XXXXX depth:0 match:1 excluded:0 filename:XXXXX
header{
Notmuch Test Suite <test_suite@notmuchmail.org> (2000-01-01) (encrypted inbox)
Subject: test encrypted message 001
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: test_suite@notmuchmail.org
Date: Sat, 01 Jan 2000 12:00:00 +0000
header}
body{
part{ ID: 1, Content-type: multipart/encrypted
part{ ID: 2, Content-type: application/pgp-encrypted
Non-text part: application/pgp-encrypted
part}
part{ ID: 3, Content-type: multipart/mixed
part{ ID: 4, Content-type: text/plain
This is a test encrypted message.
part}
attachment{ ID: 5, Filename: TESTATTACHMENT, Content-type: application/octet-stream
Non-text part: application/octet-stream
attachment}
part}
part}
body}
message}'
test_expect_equal \
    "$output" \
    "$expected"

test_begin_subtest "decryption, --format=json"
output=$(notmuch show --format=json --decrypt subject:"test encrypted message 001" \
    | notmuch_json_show_sanitize \
    | sed -e 's|"created": [1234567890]*|"created": 946728000|')
expected='[[[{"id": "XXXXX",
 "match": true,
 "excluded": false,
 "filename": ["YYYYY"],
 "timestamp": 946728000,
 "date_relative": "2000-01-01",
 "tags": ["encrypted","inbox"],
 "headers": {"Subject": "test encrypted message 001",
 "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
 "To": "test_suite@notmuchmail.org",
 "Date": "Sat, 01 Jan 2000 12:00:00 +0000"},
 "body": [{"id": 1,
 "encstatus": [{"status": "good"}],
 "sigstatus": [],
 "content-type": "multipart/encrypted",
 "content": [{"id": 2,
 "content-type": "application/pgp-encrypted",
 "content-length": "NONZERO"},
 {"id": 3,
 "content-type": "multipart/mixed",
 "content": [{"id": 4,
 "content-type": "text/plain",
 "content": "This is a test encrypted message.\n"},
 {"id": 5,
 "content-type": "application/octet-stream",
 "content-disposition": "attachment",
 "content-length": "NONZERO",
 "content-transfer-encoding": "base64",
 "filename": "TESTATTACHMENT"}]}]}]},
 []]]]'
test_expect_equal_json \
    "$output" \
    "$expected"

test_begin_subtest "decryption, --format=json, --part=4"
output=$(notmuch show --format=json --part=4 --decrypt subject:"test encrypted message 001" \
    | notmuch_json_show_sanitize \
    | sed -e 's|"created": [1234567890]*|"created": 946728000|')
expected='{"id": 4,
 "content-type": "text/plain",
 "content": "This is a test encrypted message.\n"}'
test_expect_equal_json \
    "$output" \
    "$expected"

test_begin_subtest "decrypt attachment (--part=5 --format=raw)"
notmuch show \
    --format=raw \
    --part=5 \
    --decrypt \
    subject:"test encrypted message 001" >OUTPUT
test_expect_equal_file TESTATTACHMENT OUTPUT

test_begin_subtest "decryption failure with missing key"
mv "${GNUPGHOME}"{,.bak}
output=$(notmuch show --format=json --decrypt subject:"test encrypted message 001" \
    | notmuch_json_show_sanitize \
    | sed -e 's|"created": [1234567890]*|"created": 946728000|')
expected='[[[{"id": "XXXXX",
 "match": true,
 "excluded": false,
 "filename": ["YYYYY"],
 "timestamp": 946728000,
 "date_relative": "2000-01-01",
 "tags": ["encrypted","inbox"],
 "headers": {"Subject": "test encrypted message 001",
 "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
 "To": "test_suite@notmuchmail.org",
 "Date": "Sat, 01 Jan 2000 12:00:00 +0000"},
 "body": [{"id": 1,
 "encstatus": [{"status": "bad"}],
 "content-type": "multipart/encrypted",
 "content": [{"id": 2,
 "content-type": "application/pgp-encrypted",
 "content-length": "NONZERO"},
 {"id": 3,
 "content-type": "application/octet-stream",
 "content-length": "NONZERO"}]}]},
 []]]]'
test_expect_equal_json \
    "$output" \
    "$expected"
mv "${GNUPGHOME}"{.bak,}

test_begin_subtest "emacs delivery of encrypted + signed message"
test_expect_success \
'emacs_fcc_message \
    "test encrypted message 002" \
    "This is another test encrypted message.\n" \
    "(mml-secure-message-sign-encrypt)"'

test_begin_subtest "decryption + signature verification"
test_subtest_broken_gmime_2
output=$(notmuch show --format=json --decrypt subject:"test encrypted message 002" \
    | notmuch_json_show_sanitize \
    | sed -e 's|"created": [1234567890]*|"created": 946728000|')
expected='[[[{"id": "XXXXX",
 "match": true,
 "excluded": false,
 "filename": ["YYYYY"],
 "timestamp": 946728000,
 "date_relative": "2000-01-01",
 "tags": ["encrypted","inbox"],
 "headers": {"Subject": "test encrypted message 002",
 "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
 "To": "test_suite@notmuchmail.org",
 "Date": "Sat, 01 Jan 2000 12:00:00 +0000"},
 "body": [{"id": 1,
 "encstatus": [{"status": "good"}],
 "sigstatus": [{"status": "good",
 "fingerprint": "'$FINGERPRINT'",
 "created": 946728000,
 "userid": "Notmuch Test Suite <test_suite@notmuchmail.org> (INSECURE!)"}],
 "content-type": "multipart/encrypted",
 "content": [{"id": 2,
 "content-type": "application/pgp-encrypted",
 "content-length": "NONZERO"},
 {"id": 3,
 "content-type": "text/plain",
 "content": "This is another test encrypted message.\n"}]}]},
 []]]]'
test_expect_equal_json \
    "$output" \
    "$expected"

test_begin_subtest "reply to encrypted message"
output=$(notmuch reply --decrypt subject:"test encrypted message 002" \
    | grep -v -e '^In-Reply-To:' -e '^References:')
expected='From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: test encrypted message 002

On 01 Jan 2000 12:00:00 -0000, Notmuch Test Suite <test_suite@notmuchmail.org> wrote:
> This is another test encrypted message.'
test_expect_equal \
    "$output" \
    "$expected"

test_begin_subtest "Reply within emacs to an encrypted message"
test_emacs "(let ((message-hidden-headers '())
      (notmuch-crypto-process-mime 't))
  (notmuch-show \"subject:test.encrypted.message.002\")
  (notmuch-show-reply)
  (test-output))"
# the empty To: is probably a bug, but it's not to do with encryption
grep -v -e '^In-Reply-To:' -e '^References:' -e '^Fcc:' -e 'To:' < OUTPUT > OUTPUT.clean
cat <<EOF >EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: test encrypted message 002
--text follows this line--
<#secure method=pgpmime mode=signencrypt>
Notmuch Test Suite <test_suite@notmuchmail.org> writes:

> This is another test encrypted message.
EOF
test_expect_equal_file EXPECTED OUTPUT.clean

test_begin_subtest "signature verification with revoked key"
# generate revocation certificate and load it to revoke key
echo "y
1
Notmuch Test Suite key revocation (automated) $(date '+%F_%T%z')

y

" \
    | gpg --no-tty --quiet --command-fd 0 --armor --gen-revoke "0x${FINGERPRINT}!" 2>/dev/null \
    | gpg --no-tty --quiet --import
output=$(notmuch show --format=json --verify subject:"test signed message 001" \
    | notmuch_json_show_sanitize \
    | sed -e 's|"created": [1234567890]*|"created": 946728000|')
expected='[[[{"id": "XXXXX",
 "match": true,
 "excluded": false,
 "filename": ["YYYYY"],
 "timestamp": 946728000,
 "date_relative": "2000-01-01",
 "tags": ["inbox","signed"],
 "headers": {"Subject": "test signed message 001",
 "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
 "To": "test_suite@notmuchmail.org",
 "Date": "Sat, 01 Jan 2000 12:00:00 +0000"},
 "body": [{"id": 1,
 "sigstatus": [{"status": "error",
 "keyid": "6D92612D94E46381",
 "errors": {"key-revoked": true}}],
 "content-type": "multipart/signed",
 "content": [{"id": 2,
 "content-type": "text/plain",
 "content": "This is a test signed message.\n"},
 {"id": 3,
 "content-type": "application/pgp-signature",
 "content-length": "NONZERO"}]}]},
 []]]]'
test_expect_equal_json \
    "$output" \
    "$expected"

test_done
