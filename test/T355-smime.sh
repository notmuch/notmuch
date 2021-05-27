#!/usr/bin/env bash

test_description='S/MIME signature verification and decryption'
. $(dirname "$0")/test-lib.sh || exit 1
. $NOTMUCH_SRCDIR/test/test-lib-emacs.sh || exit 1

test_require_emacs
test_require_external_prereq openssl
test_require_external_prereq gpgsm

FINGERPRINT=$(openssl x509 -sha1 -fingerprint -in "$NOTMUCH_SRCDIR/test/smime/key+cert.pem" -noout | sed -e 's/^.*=//' -e s/://g)

add_gpgsm_home

test_begin_subtest "emacs delivery of S/MIME signed message"
test_expect_success \
     'emacs_fcc_message \
     "test signed message 001" \
     "This is a test signed message." \
     "(mml-secure-message-sign \"smime\")"'

test_begin_subtest "emacs delivery of S/MIME encrypted + signed message"
# Hard code the MML to avoid several interactive questions
test_expect_success \
'emacs_fcc_message \
    "test encrypted message 001" \
    "<#secure method=smime mode=signencrypt>\nThis is a test encrypted message.\n"'

test_begin_subtest "Signature verification (openssl)"
notmuch show --format=raw subject:"test signed message 001" |\
    openssl smime -verify -CAfile $NOTMUCH_SRCDIR/test/smime/test.crt 2>OUTPUT
cat <<EOF > EXPECTED
Verification successful
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "signature verification (notmuch CLI)"
output=$(notmuch show --format=json --verify subject:"test signed message 001" \
    | notmuch_json_show_sanitize \
    | sed -e 's|"created": [-1234567890]*|"created": 946728000|g' \
	  -e 's|"expires": [-1234567890]*|"expires": 424242424|g' )
expected='[[[{"id": "XXXXX",
 "match": true,
 "excluded": false,
 "filename": ["YYYYY"],
 "timestamp": 946728000,
 "date_relative": "2000-01-01",
 "tags": ["inbox","signed"],
 "crypto": {"signed": {"status": [{"fingerprint": "'$FINGERPRINT'", "status": "good","userid": "CN=Notmuch Test Suite", "email": "<test_suite@notmuchmail.org>", "expires": 424242424, "created": 946728000}]}},
 "headers": {"Subject": "test signed message 001",
 "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
 "To": "test_suite@notmuchmail.org",
 "Date": "Sat, 01 Jan 2000 12:00:00 +0000"},
 "body": [{"id": 1,
 "sigstatus": [{"fingerprint": "'$FINGERPRINT'",
 "status": "good",
 "userid": "CN=Notmuch Test Suite",
 "email": "<test_suite@notmuchmail.org>",
 "expires": 424242424,
 "created": 946728000}],
 "content-type": "multipart/signed",
 "content": [{"id": 2,
 "content-type": "text/plain",
 "content": "This is a test signed message.\n"},
 {"id": 3,
  "content-disposition": "attachment",
  "content-length": "NONZERO",
  "content-transfer-encoding": "base64",
  "content-type": "application/pkcs7-signature",
  "filename": "smime.p7s"}]}]},
 []]]]'
test_expect_equal_json \
    "$output" \
    "$expected"

test_begin_subtest "Decryption and signature verification (openssl)"
notmuch show --format=raw subject:"test encrypted message 001" |\
    openssl smime -decrypt -recip $NOTMUCH_SRCDIR/test/smime/key+cert.pem |\
    openssl smime -verify -CAfile $NOTMUCH_SRCDIR/test/smime/test.crt 2>OUTPUT
cat <<EOF > EXPECTED
Verification successful
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Decryption (notmuch CLI)"
notmuch show --decrypt=true subject:"test encrypted message 001" |\
    grep "^This is a" > OUTPUT
cat <<EOF > EXPECTED
This is a test encrypted message.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Cryptographic message status (encrypted+signed)"
output=$(notmuch show --format=json --decrypt=true subject:"test encrypted message 001")
test_json_nodes <<<"$output" \
                'crypto_encrypted:[0][0][0]["crypto"]["decrypted"]["status"]="full"' \
                'crypto_sigok:[0][0][0]["crypto"]["signed"]["status"][0]["status"]="good"' \
                'crypto_fpr:[0][0][0]["crypto"]["signed"]["status"][0]["fingerprint"]="616F46CD73834C63847756AF0DFB64A6E0972A47"' \
                'crypto_uid:[0][0][0]["crypto"]["signed"]["status"][0]["userid"]="CN=Notmuch Test Suite"'

test_begin_subtest "encrypted+signed message is known to be encrypted, but signature is unknown"
output=$(notmuch search subject:"test encrypted message 001")
test_expect_equal "$output" "thread:0000000000000002   2000-01-01 [1/1] Notmuch Test Suite; test encrypted message 001 (encrypted inbox)"

test_begin_subtest "Encrypted body is not indexed"
output=$(notmuch search 'this is a test encrypted message')
test_expect_equal "$output" ""

test_begin_subtest "Reindex cleartext"
test_expect_success "notmuch reindex --decrypt=true subject:'test encrypted message 001'"

test_begin_subtest "signature is now known"
output=$(notmuch search subject:"test encrypted message 001")
test_expect_equal "$output" "thread:0000000000000002   2000-01-01 [1/1] Notmuch Test Suite; test encrypted message 001 (encrypted inbox signed)"

test_begin_subtest "Encrypted body is indexed"
output=$(notmuch search 'this is a test encrypted message')
test_expect_equal "$output" "thread:0000000000000002   2000-01-01 [1/1] Notmuch Test Suite; test encrypted message 001 (encrypted inbox signed)"

add_email_corpus pkcs7

test_begin_subtest "index PKCS#7 SignedData message"
output=$(notmuch search --output=messages Thanks)
expected=id:smime-onepart-signed@protected-headers.example
test_expect_equal "$expected" "$output"

test_begin_subtest "do not index embedded certificates from PKCS#7 SignedData"
output=$(notmuch search --output=messages 'LAMPS Certificate')
expected=''
test_expect_equal "$expected" "$output"

test_begin_subtest "know the MIME type of the embedded part in PKCS#7 SignedData"
output=$(notmuch search --output=messages 'mimetype:text/plain')
expected=id:smime-onepart-signed@protected-headers.example
test_expect_equal "$expected" "$output"

test_begin_subtest "PKCS#7 SignedData message is tagged 'signed'"
output=$(notmuch dump id:smime-onepart-signed@protected-headers.example)
expected='#notmuch-dump batch-tag:3 config,properties,tags
+inbox +signed +unread -- id:smime-onepart-signed@protected-headers.example'
test_expect_equal "$expected" "$output"

test_begin_subtest "show contents of PKCS#7 SignedData message"
output=$(notmuch show --format=raw --part=2 id:smime-onepart-signed@protected-headers.example)
whitespace=' '
expected="Bob, we need to cancel this contract.

Please start the necessary processes to make that happen today.

Thanks, Alice
--${whitespace}
Alice Lovelace
President
OpenPGP Example Corp"
test_expect_equal "$expected" "$output"

test_begin_subtest "reply to PKCS#7 SignedData message with proper quoting and attribution"
output=$(notmuch reply id:smime-onepart-signed@protected-headers.example)
expected="From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: The FooCorp contract
To: Alice Lovelace <alice@smime.example>, Bob Babbage <bob@smime.example>
In-Reply-To: <smime-onepart-signed@protected-headers.example>
References: <smime-onepart-signed@protected-headers.example>

On Tue, 26 Nov 2019 20:11:29 -0400, Alice Lovelace <alice@smime.example> wrote:
> Bob, we need to cancel this contract.
>${whitespace}
> Please start the necessary processes to make that happen today.
>${whitespace}
> Thanks, Alice
> --${whitespace}
> Alice Lovelace
> President
> OpenPGP Example Corp"
test_expect_equal "$expected" "$output"

test_begin_subtest "show PKCS#7 SignedData outputs valid JSON"
output=$(notmuch show --format=json id:smime-onepart-signed@protected-headers.example)
test_valid_json "$output"

test_begin_subtest "Verify signature on PKCS#7 SignedData message"
if [ $NOTMUCH_HAVE_64BIT_TIME_T -ne 1 ]; then
    test_subtest_known_broken
fi
output=$(notmuch show --format=json id:smime-onepart-signed@protected-headers.example)

test_json_nodes <<<"$output" \
                'created:[0][0][0]["crypto"]["signed"]["status"][0]["created"]=1574813489' \
                'expires:[0][0][0]["crypto"]["signed"]["status"][0]["expires"]=2611032858' \
                'fingerprint:[0][0][0]["crypto"]["signed"]["status"][0]["fingerprint"]="702BA4B157F1E2B7D16B0C6A5FFC8A7DE2057DEB"' \
                'status:[0][0][0]["crypto"]["signed"]["status"][0]["status"]="good"'

test_begin_subtest "Verify signature on PKCS#7 SignedData message signer User ID"
if [ $NOTMUCH_GMIME_X509_CERT_VALIDITY -ne 1 ]; then
    test_subtest_known_broken
fi
test_json_nodes <<<"$output" \
                'userid:[0][0][0]["crypto"]["signed"]["status"][0]["userid"]="CN=Alice Lovelace"'

test_done
