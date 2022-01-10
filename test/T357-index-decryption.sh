#!/usr/bin/env bash

# TODO: test index.decryption=failed

test_description='indexing decrypted mail'
. $(dirname "$0")/test-lib.sh || exit 1
. $NOTMUCH_SRCDIR/test/test-lib-emacs.sh || exit 1

##################################################

test_require_emacs
add_gnupg_home

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
'emacs_fcc_message --decrypt=true \
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

test_begin_subtest "show the message body of the encrypted message"
notmuch dump wumpus
output=$(notmuch show wumpus | notmuch_show_part 3)
expected='This is a test encrypted message with a wumpus.'
test_expect_equal \
    "$output" \
    "$expected"


test_begin_subtest "message should go away after deletion"
# cache the message in an env var and remove it:
fname=$(notmuch search --output=files wumpus)
contents="$(notmuch show --format=raw wumpus)"
rm -f "$fname"
notmuch new
output=$(notmuch search wumpus)
expected=''
test_expect_equal \
    "$output" \
    "$expected"

# try reinserting it without decryption, should stay the same:
test_begin_subtest "message cleartext not present after insert"
notmuch insert --folder=sent <<<"$contents"
output=$(notmuch search wumpus)
test_expect_equal \
    "$output" \
    "$expected"

# show the message using stashing decryption
test_begin_subtest "stash decryption during show"
output=$(notmuch show --decrypt=stash tag:encrypted subject:002 | notmuch_show_part 3)
expected='This is a test encrypted message with a wumpus.'
test_expect_equal \
    "$output" \
    "$expected"

test_begin_subtest "search should now find the contents"
output=$(notmuch search wumpus)
expected='thread:0000000000000003   2000-01-01 [1/1] Notmuch Test Suite; test encrypted message for cleartext index 002 (encrypted inbox unread)'
test_expect_equal \
    "$output" \
    "$expected"

# try reinserting it with decryption, should appear again, but now we
# have two copies of the message:
test_begin_subtest "message cleartext is present after reinserting with --decrypt=true"
notmuch insert --folder=sent --decrypt=true <<<"$contents"
output=$(notmuch search wumpus)
expected='thread:0000000000000003   2000-01-01 [1/1(2)] Notmuch Test Suite; test encrypted message for cleartext index 002 (encrypted inbox unread)'
test_expect_equal \
    "$output" \
    "$expected"

# remove all copies
test_begin_subtest "delete all copies of the message"
mid="$(notmuch search --output=messages wumpus)"
rm -f $(notmuch search --output=files wumpus)
notmuch new
output=$(notmuch search "id:$mid")
expected=''
test_expect_equal \
    "$output" \
    "$expected"

# try inserting it with decryption, should appear as a single copy
test_begin_subtest "message cleartext is present with insert --decrypt=true"
notmuch insert --folder=sent --decrypt=true <<<"$contents"
output=$(notmuch search wumpus | notmuch_search_sanitize)
expected='thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; test encrypted message for cleartext index 002 (encrypted inbox unread)'
test_expect_equal \
    "$output" \
    "$expected"


# add a tag to all messages to ensure that it stays after reindexing
test_begin_subtest 'tagging all messages'
test_expect_success 'notmuch tag +blarney "encrypted message"'
test_begin_subtest "verify that tags have not changed"
output=$(notmuch search tag:blarney | notmuch_search_sanitize)
expected='thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; test encrypted message for cleartext index 001 (blarney encrypted inbox)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; test encrypted message for cleartext index 002 (blarney encrypted inbox unread)'
test_expect_equal \
    "$output" \
    "$expected"

# see if first message shows up after reindexing with --decrypt=true (same $expected, untouched):
test_begin_subtest 'reindex old messages'
test_expect_success 'notmuch reindex --decrypt=true tag:encrypted and not property:index.decryption=success'
test_begin_subtest "reindexed encrypted message, including cleartext"
output=$(notmuch search wumpus | notmuch_search_sanitize)
test_expect_equal \
    "$output" \
    "$expected"

# and the same search, but by property ($expected is untouched):
test_begin_subtest "emacs search by property for both messages"
output=$(notmuch search property:index.decryption=success | notmuch_search_sanitize)
test_expect_equal \
    "$output" \
    "$expected"

# try a simple reindex
test_begin_subtest 'reindex in auto mode'
test_expect_success 'notmuch reindex tag:encrypted and property:index.decryption=success'
test_begin_subtest "reindexed encrypted messages, should not have changed"
output=$(notmuch search wumpus | notmuch_search_sanitize)
test_expect_equal \
    "$output" \
    "$expected"

# try to remove cleartext indexing
test_begin_subtest 'reindex without cleartext'
test_expect_success 'notmuch reindex --decrypt=false tag:encrypted and property:index.decryption=success'
test_begin_subtest "reindexed encrypted messages, without cleartext"
output=$(notmuch search wumpus)
expected=''
test_expect_equal \
    "$output" \
    "$expected"

# ensure no session keys are present:
test_begin_subtest 'reindex using only session keys'
test_expect_success 'notmuch reindex tag:encrypted and property:index.decryption=success'
test_begin_subtest "reindexed encrypted messages, decrypting only with session keys"
output=$(notmuch search wumpus)
expected=''
test_expect_equal \
    "$output" \
    "$expected"

# and the same search, but by property ($expected is untouched):
test_begin_subtest "emacs search by property with both messages unindexed"
output=$(notmuch search property:index.decryption=success)
test_expect_equal \
    "$output" \
    "$expected"

# ensure that the tags remain even when we are dropping the cleartext.
test_begin_subtest "verify that tags remain without cleartext"
output=$(notmuch search tag:blarney | notmuch_search_sanitize)
expected='thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; test encrypted message for cleartext index 001 (blarney encrypted inbox)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; test encrypted message for cleartext index 002 (blarney encrypted inbox unread)'
test_expect_equal \
    "$output" \
    "$expected"

test_begin_subtest "index cleartext without keeping session keys"
test_expect_success "notmuch reindex --decrypt=nostash tag:blarney"

test_begin_subtest "Ensure that the indexed terms are present"
output=$(notmuch search wumpus | notmuch_search_sanitize)
test_expect_equal \
    "$output" \
    "$expected"

test_begin_subtest "show one of the messages with --decrypt=true"
output=$(notmuch show --decrypt=true thread:0000000000000001 | notmuch_show_part 3)
expected='This is a test encrypted message with a wumpus.'
test_expect_equal \
    "$output" \
    "$expected"

test_begin_subtest "Ensure that we cannot show the message with --decrypt=auto"
output=$(notmuch show thread:0000000000000001 | notmuch_show_part 3)
expected='Non-text part: application/octet-stream'
test_expect_equal \
    "$output" \
    "$expected"

add_email_corpus crypto

test_begin_subtest "indexing message fails when secret key not available"
notmuch reindex --decrypt=true id:simple-encrypted@crypto.notmuchmail.org
output=$(notmuch dump | LC_ALL=C sort)
expected='#= simple-encrypted@crypto.notmuchmail.org index.decryption=failure
#notmuch-dump batch-tag:3 config,properties,tags
+encrypted +inbox +unread -- id:basic-encrypted@crypto.notmuchmail.org
+encrypted +inbox +unread -- id:encrypted-rfc822-attachment@crypto.notmuchmail.org
+encrypted +inbox +unread -- id:encrypted-signed@crypto.notmuchmail.org
+encrypted +inbox +unread -- id:simple-encrypted@crypto.notmuchmail.org'
test_expect_equal \
    "$output" \
    "$expected"

test_begin_subtest "cannot find cleartext index"
output=$(notmuch search sekrit)
expected=''
test_expect_equal \
    "$output" \
    "$expected"

test_begin_subtest "cleartext index recovery on reindexing with stashed session keys"
notmuch restore <<EOF
#notmuch-dump batch-tag:3 config,properties,tags
#= simple-encrypted@crypto.notmuchmail.org session-key=9%3AFC09987F5F927CC0CC0EE80A96E4C5BBF4A499818FB591207705DFDDD6112CF9
EOF
notmuch reindex id:simple-encrypted@crypto.notmuchmail.org
output=$(notmuch search sekrit | notmuch_search_sanitize)
expected='thread:XXX   2016-12-22 [1/1] Daniel Kahn Gillmor; encrypted message (encrypted inbox unread)'
test_expect_equal \
    "$output" \
    "$expected"

test_begin_subtest "notmuch reply should show cleartext if session key is present"
output=$(notmuch reply id:simple-encrypted@crypto.notmuchmail.org | grep '^>')
expected='> This is a top sekrit message.'
test_expect_equal \
    "$output" \
    "$expected"

test_begin_subtest "notmuch show should show cleartext if session key is present"
output=$(notmuch show id:simple-encrypted@crypto.notmuchmail.org | notmuch_show_part 3)
expected='This is a top sekrit message.'
test_expect_equal \
    "$output" \
    "$expected"

test_begin_subtest "notmuch show should show nothing if decryption is explicitly disallowed"
output=$(notmuch show --decrypt=false id:simple-encrypted@crypto.notmuchmail.org | notmuch_show_part 3)
expected='Non-text part: application/octet-stream'
test_expect_equal \
    "$output" \
    "$expected"

test_begin_subtest "purging stashed session keys should lose access to the cleartext"
notmuch reindex --decrypt=false id:simple-encrypted@crypto.notmuchmail.org
output=$(notmuch search sekrit)
expected=''
test_expect_equal \
    "$output" \
    "$expected"

test_begin_subtest "and cleartext should be unrecoverable now that there are no stashed session keys"
notmuch dump
notmuch reindex --decrypt=true id:simple-encrypted@crypto.notmuchmail.org
output=$(notmuch search sekrit)
expected=''
test_expect_equal \
    "$output" \
    "$expected"

goodsig='good_sig:[0][0][0]["crypto"]["signed"]["status"][0]["status"]="good"'
nosig='no_sig:[0][0][0]["crypto"]!"signed"'

test_begin_subtest "verify signature without a session key stashed when --decrypt=true"
output=$(notmuch show --format=json --decrypt=true id:encrypted-signed@crypto.notmuchmail.org)
test_json_nodes <<<"$output" "$goodsig"

test_begin_subtest "do not verify sig without a session key stashed if --decrypt=auto"
output=$(notmuch show --format=json id:encrypted-signed@crypto.notmuchmail.org)
test_json_nodes <<<"$output" "$nosig"

test_begin_subtest "verify signature when --decrypt=stash"
output=$(notmuch show --format=json --decrypt=stash id:encrypted-signed@crypto.notmuchmail.org)
test_json_nodes <<<"$output" "$goodsig"

test_begin_subtest "verify signature with stashed session key"
output=$(notmuch show --format=json id:encrypted-signed@crypto.notmuchmail.org)
if [ $NOTMUCH_GMIME_VERIFY_WITH_SESSION_KEY -ne 1 ]; then
    test_subtest_known_broken
fi
test_json_nodes <<<"$output" "$goodsig"

# TODO: test removal of a message from the message store between
# indexing and reindexing.

# TODO: insert the same message into the message store twice, index,
# remove one of them from the message store, and then reindex.
# reindexing should return a failure but the message should still be
# present? -- or what should the semantics be if you ask to reindex a
# message whose underlying files have been renamed or moved or
# removed?

test_done
