#!/usr/bin/env bash

test_description='S/MIME signature verification and decryption'
. ./test-lib.sh || exit 1

test_require_external_prereq openssl
test_require_external_prereq gpgsm

cp $TEST_DIRECTORY/smime/key+cert.pem test_suite.pem

FINGERPRINT=$(openssl x509 -fingerprint -in test_suite.pem -noout | sed -e 's/^.*=//' -e s/://g)

test_expect_success 'emacs delivery of S/MIME signed message' \
     'emacs_fcc_message \
     "test signed message 001" \
     "This is a test signed message." \
     "(mml-secure-message-sign \"smime\")"'

# Hard code the MML to avoid several interactive questions
test_expect_success 'emacs delivery of S/MIME encrypted + signed message' \
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

test_begin_subtest "Decryption and signature verification (openssl)"
notmuch show --format=raw subject:"test encrypted message 001" |\
    openssl smime -decrypt -recip test_suite.pem |\
    openssl smime -verify -CAfile $TEST_DIRECTORY/smime/test.crt 2>OUTPUT
cat <<EOF > EXPECTED
Verification successful
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
