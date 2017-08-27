#!/usr/bin/env bash

test_description='S/MIME signature verification and decryption'
. ./test-lib.sh || exit 1

add_gpgsm_home ()
{
    local fpr
    [ -d ${GNUPGHOME} ] && return
    _gnupg_exit () { gpgconf --kill all 2>/dev/null || true; }
    at_exit_function _gnupg_exit
    mkdir -m 0700 "$GNUPGHOME"
    gpgsm --no-tty --no-common-certs-import --disable-dirmngr --import < $TEST_DIRECTORY/smime/test.crt >"$GNUPGHOME"/import.log 2>&1
    fpr=$(gpgsm  --list-key test_suite@notmuchmail.org | sed -n 's/.*fingerprint: //p')
    echo "$fpr S relax" >> $GNUPGHOME/trustlist.txt
    test_debug "cat $GNUPGHOME/import.log"
}

test_require_external_prereq openssl
test_require_external_prereq gpgsm

cp $TEST_DIRECTORY/smime/key+cert.pem test_suite.pem

FINGERPRINT=$(openssl x509 -fingerprint -in test_suite.pem -noout | sed -e 's/^.*=//' -e s/://g)

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
    "<#secure method=smime mode=signencrypt keyfile=\\\"test_suite.pem\\\" certfile=\\\"test_suite.pem\\\">\nThis is a test encrypted message.\n"'

test_begin_subtest "Signature verification (openssl)"
notmuch show --format=raw subject:"test signed message 001" |\
    openssl smime -verify -CAfile $TEST_DIRECTORY/smime/test.crt 2>OUTPUT
cat <<EOF > EXPECTED
Verification successful
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "signature verification (notmuch CLI)"
output=$(notmuch show --format=json --verify subject:"test signed message 001" \
    | notmuch_json_show_sanitize \
    | sed -e 's|"created": [-1234567890]*|"created": 946728000|' \
	  -e 's|"expires": [-1234567890]*|"expires": 424242424|' )
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
 "sigstatus": [{"fingerprint": "'$FINGERPRINT'",
 "status": "good",
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
  "content-type": "application/x-pkcs7-signature",
  "filename": "smime.p7s"}]}]},
 []]]]'
test_expect_equal_json \
    "$output" \
    "$expected"

test_begin_subtest "Decryption and signature verification (openssl)"
notmuch show --format=raw subject:"test encrypted message 001" |\
    openssl smime -decrypt -recip test_suite.pem |\
    openssl smime -verify -CAfile $TEST_DIRECTORY/smime/test.crt 2>OUTPUT
cat <<EOF > EXPECTED
Verification successful
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
