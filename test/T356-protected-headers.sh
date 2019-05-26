#!/usr/bin/env bash

# TODO:
#  * check S/MIME as well as PGP/MIME
#  * process headers protected by signature

test_description='Message decryption with protected headers'
. $(dirname "$0")/test-lib.sh || exit 1

##################################################

add_gnupg_home

add_email_corpus protected-headers

test_begin_subtest "verify protected header is not visible without decryption"
output=$(notmuch show --format=json id:protected-header@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'no_crypto_info:[0][0][0]["crypto"]={}' \
                'subject:[0][0][0]["headers"]["Subject"]="Subject Unavailable"'

test_begin_subtest "verify protected header is visible with decryption"
output=$(notmuch show --decrypt=true --format=json id:protected-header@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'crypto:[0][0][0]["crypto"]={"decrypted": {"status": "full"}}' \
                'subject:[0][0][0]["headers"]["Subject"]="This is a protected header"'

test_begin_subtest "misplaced protected headers should not be made visible during decryption"
output=$(notmuch show --decrypt=true --format=json id:misplaced-protected-header@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'crypto:[0][0][0]["crypto"]={"decrypted": {"status": "full"}}' \
                'subject:[0][0][0]["headers"]["Subject"]="Subject Unavailable"'

test_begin_subtest "verify double-wrapped phony protected header is not visible when inner decryption fails"
output=$(notmuch show --decrypt=true --format=json id:double-wrapped-with-phony-protected-header@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'crypto:[0][0][0]["crypto"]={"decrypted": {"status": "full"}}' \
                'subject:[0][0][0]["headers"]["Subject"]="Subject Unavailable"'

test_begin_subtest "cleartext phony protected headers should not be made visible when decryption fails"
output=$(notmuch show --decrypt=true --format=json id:phony-protected-header-bad-encryption@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'no_crypto_info:[0][0][0]["crypto"]={}' \
                'subject:[0][0][0]["headers"]["Subject"]="Subject Unavailable"'

test_begin_subtest "wrapped protected headers should not be made visible during decryption"
output=$(notmuch show --decrypt=true --format=json id:wrapped-protected-header@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'crypto:[0][0][0]["crypto"]={"decrypted": {"status": "partial"}}' \
                'subject:[0][0][0]["headers"]["Subject"]="[mailing-list] Subject Unavailable"'

test_begin_subtest "internal headers without protected-header attribute should be skipped"
output=$(notmuch show --decrypt=true --format=json id:no-protected-header-attribute@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'crypto:[0][0][0]["crypto"]={"decrypted": {"status": "full"}}' \
                'subject:[0][0][0]["headers"]["Subject"]="Subject Unavailable"'

test_begin_subtest "verify nested message/rfc822 protected header is visible"
output=$(notmuch show --decrypt=true --format=json id:nested-rfc822-message@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'crypto:[0][0][0]["crypto"]={"decrypted": {"status": "full"}}' \
                'subject:[0][0][0]["headers"]["Subject"]="This is a message using draft-melnikov-smime-header-signing"'

test_done
