#!/usr/bin/env bash

# TODO: test index.decryption=failed

test_description='indexing decrypted mail'
. $(dirname "$0")/test-lib.sh || exit 1

##################################################

add_gnupg_home
# get key fingerprint
FINGERPRINT=$(gpg --no-tty --list-secret-keys --with-colons --fingerprint | grep '^fpr:' | cut -d: -f10)

# create a test encrypted message
test_begin_subtest 'emacs delivery of encrypted message'
test_expect_success \
'emacs_fcc_message \
    "test encrypted message for cleartext index 001" \
    "This is a test encrypted message with a wumpus.\n" \
    "(mml-secure-message-encrypt)"'

test_begin_subtest "search for unindexed cleartext"
output=$(notmuch search wumpus)
expected=''
test_expect_equal \
    "$output" \
    "$expected"

# create a test encrypted message that is indexed in the clear
test_begin_subtest 'emacs delivery of encrypted message'
test_expect_success \
'emacs_fcc_message --try-decrypt=true \
    "test encrypted message for cleartext index 002" \
    "This is a test encrypted message with a wumpus.\n" \
    "(mml-secure-message-encrypt)"'

test_begin_subtest "emacs delivery of encrypted message, indexed cleartext"
output=$(notmuch search wumpus)
expected='thread:0000000000000002   2000-01-01 [1/1] Notmuch Test Suite; test encrypted message for cleartext index 002 (encrypted inbox)'
test_expect_equal \
    "$output" \
    "$expected"

# and the same search, but by property ($expected is untouched):
test_begin_subtest "emacs search by property for one message"
output=$(notmuch search property:index.decryption=success)
test_expect_equal \
    "$output" \
    "$expected"

test_done
