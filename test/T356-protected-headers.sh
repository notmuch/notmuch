#!/usr/bin/env bash

test_description='Message decryption with protected headers'
. $(dirname "$0")/test-lib.sh || exit 1

##################################################

test_require_external_prereq gpgsm

add_gnupg_home
add_gpgsm_home

add_email_corpus protected-headers

test_begin_subtest "verify protected header is not visible without decryption"
output=$(notmuch show --format=json id:protected-header@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'no_crypto_info:[0][0][0]["crypto"]={}' \
                'subject:[0][0][0]["headers"]["Subject"]="Subject Unavailable"'

test_begin_subtest "verify protected header is visible with decryption"
output=$(notmuch show --decrypt=true --format=json id:protected-header@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'crypto:[0][0][0]["crypto"]={"decrypted": {"status": "full", "header-mask": {"Subject": "Subject Unavailable"}}}' \
                'subject:[0][0][0]["headers"]["Subject"]="This is a protected header"'

test_begin_subtest "when no external header is present, show masked subject as null"
output=$(notmuch show --decrypt=true --format=json id:subjectless-protected-header@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'crypto:[0][0][0]["crypto"]={"decrypted": {"status": "full", "header-mask": {"Subject": null}}}' \
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
                'crypto:[0][0][0]["crypto"]={"decrypted": {"status": "full", "header-mask": {"Subject": "Subject Unavailable"}}}' \
                'subject:[0][0][0]["headers"]["Subject"]="This is a message using draft-melnikov-smime-header-signing"'

test_begin_subtest "show cryptographic envelope on signed mail"
output=$(notmuch show --verify --format=json id:simple-signed-mail@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'crypto:[0][0][0]["crypto"]={"signed": {"status": [{"created": 1525609971, "fingerprint": "'$FINGERPRINT'", "email": "'"$SELF_EMAIL"'", "userid": "'"$SELF_USERID"'", "status": "good"}]}}'

test_begin_subtest "verify signed protected header"
output=$(notmuch show --verify --format=json id:signed-protected-header@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'crypto:[0][0][0]["crypto"]={"signed": {"status": [{"created": 1525350527, "fingerprint": "'$FINGERPRINT'", "email": "'"$SELF_EMAIL"'", "userid": "'"$SELF_USERID"'", "status": "good"}], "headers": ["Subject"]}}'

test_begin_subtest "protected subject does not leak by default in replies"
output=$(notmuch reply --decrypt=true --format=json id:protected-header@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'crypto:["original"]["crypto"]={"decrypted": {"status": "full", "header-mask": {"Subject": "Subject Unavailable"}}}' \
                'subject:["original"]["headers"]["Subject"]="This is a protected header"' \
                'reply-subject:["reply-headers"]["Subject"]="Re: Subject Unavailable"'

test_begin_subtest "protected subject is not indexed by default"
output=$(notmuch search --output=messages 'subject:"This is a protected header"')
test_expect_equal "$output" ''

test_begin_subtest "reindex message with protected header"
test_expect_success 'notmuch reindex --decrypt=true id:protected-header@crypto.notmuchmail.org'

test_begin_subtest "protected subject is indexed when cleartext is indexed"
output=$(notmuch search --output=messages 'subject:"This is a protected header"')
test_expect_equal "$output" 'id:protected-header@crypto.notmuchmail.org'

test_begin_subtest "indexed protected subject is visible in search"
output=$(notmuch search --format=json 'id:protected-header@crypto.notmuchmail.org')
test_json_nodes <<<"$output" \
                'subject:[0]["subject"]="This is a protected header"'

test_begin_subtest "indexed protected subject is not visible in reply header"
output=$(notmuch reply --format=json 'id:protected-header@crypto.notmuchmail.org')
test_json_nodes <<<"$output" \
                'subject:["original"]["headers"]["Subject"]="This is a protected header"' \
                'reply-subject:["reply-headers"]["Subject"]="Re: Subject Unavailable"'

test_begin_subtest "verify correct protected header when submessage exists"
output=$(notmuch show --decrypt=true --format=json id:encrypted-message-with-forwarded-attachment@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'crypto:[0][0][0]["crypto"]={"decrypted": {"status": "full", "header-mask": {"Subject": "Subject Unavailable"}}}' \
                'subject:[0][0][0]["headers"]["Subject"]="This is the cryptographic envelope subject"'

test_begin_subtest "verify protected header is both signed and encrypted"
output=$(notmuch show --decrypt=true --format=json id:encrypted-signed@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'crypto:[0][0][0]["crypto"]={
                   "signed":{"status": [{"status": "good", "fingerprint": "'$FINGERPRINT'", "email": "'"$SELF_EMAIL"'", "userid": "'"$SELF_USERID"'", "created": 1525812676}],
                   "encrypted": true, "headers": ["Subject"]},"decrypted": {"status": "full", "header-mask": {"Subject": "Subject Unavailable"}}}' \
                'subject:[0][0][0]["headers"]["Subject"]="Rhinoceros dinner"'

test_begin_subtest "verify protected header is signed even when not masked"
output=$(notmuch show --decrypt=true --format=json id:encrypted-signed-not-masked@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'crypto:[0][0][0]["crypto"]={
                   "signed":{"status": [{"status": "good", "fingerprint": "'$FINGERPRINT'", "userid": "'"$SELF_USERID"'", "email": "'"$SELF_EMAIL"'", "created": 1525812676}],
                   "encrypted": true, "headers": ["Subject"]},"decrypted": {"status": "full"}}' \
                'subject:[0][0][0]["headers"]["Subject"]="Rhinoceros dinner"'

test_begin_subtest "reindex everything, ensure headers are as expected"
notmuch reindex --decrypt=true from:test_suite@notmuchmail.org
output=$(notmuch search --output=messages 'subject:"protected header" or subject:"Rhinoceros" or subject:"draft-melnikov-smime-header-signing" or subject:"valid"' | sort)
test_expect_equal "$output" 'id:encrypted-signed-not-masked@crypto.notmuchmail.org
id:encrypted-signed@crypto.notmuchmail.org
id:nested-rfc822-message@crypto.notmuchmail.org
id:protected-header@crypto.notmuchmail.org
id:subjectless-protected-header@crypto.notmuchmail.org'

test_begin_subtest "when rendering protected headers, avoid rendering legacy-display part"
output=$(notmuch show --format=json id:protected-with-legacy-display@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'subject:[0][0][0]["headers"]["Subject"]="Interrupting Cow"' \
                'no_legacy_display:[0][0][0]["body"][0]["content"][1]["content-type"]="text/plain"'

test_begin_subtest "when replying, avoid rendering legacy-display part"
output=$(notmuch reply --format=json id:protected-with-legacy-display@crypto.notmuchmail.org)
test_json_nodes <<<"$output" \
                'no_legacy_display:["original"]["body"][0]["content"][1]["content-type"]="text/plain"'

test_begin_subtest "do not treat legacy-display part as body when indexing"
output=$(notmuch search --output=messages body:interrupting)
test_expect_equal "$output" ''

test_begin_subtest "identify message that had a legacy display part skipped during indexing"
output=$(notmuch search --output=messages property:index.repaired=skip-protected-headers-legacy-display)
test_expect_equal "$output" id:protected-with-legacy-display@crypto.notmuchmail.org

for variant in multipart-signed onepart-signed; do
    test_begin_subtest "verify signed PKCS#7 subject ($variant)"
    output=$(notmuch show --verify --format=json "id:smime-${variant}@protected-headers.example")
    test_json_nodes <<<"$output" \
                    'signed_subject:[0][0][0]["crypto"]["signed"]["headers"]=["Subject"]' \
                    'sig_good:[0][0][0]["crypto"]["signed"]["status"][0]["status"]="good"' \
                    'sig_fpr:[0][0][0]["crypto"]["signed"]["status"][0]["fingerprint"]="702BA4B157F1E2B7D16B0C6A5FFC8A7DE2057DEB"' \
                    'not_encrypted:[0][0][0]["crypto"]!"decrypted"'
    test_begin_subtest "verify signed PKCS#7 subject ($variant) signer User ID"
    if [ $NOTMUCH_GMIME_X509_CERT_VALIDITY -ne 1 ]; then
        test_subtest_known_broken
    fi
    test_json_nodes <<<"$output" \
                    'sig_uid:[0][0][0]["crypto"]["signed"]["status"][0]["userid"]="CN=Alice Lovelace"'
done

for variant in sign+enc sign+enc+legacy-disp; do
    test_begin_subtest "confirm signed and encrypted PKCS#7 subject ($variant)"
    output=$(notmuch show --decrypt=true --format=json "id:smime-${variant}@protected-headers.example")
    test_json_nodes <<<"$output" \
                    'signed_subject:[0][0][0]["crypto"]["signed"]["headers"]=["Subject"]' \
                    'sig_good:[0][0][0]["crypto"]["signed"]["status"][0]["status"]="good"' \
                    'sig_fpr:[0][0][0]["crypto"]["signed"]["status"][0]["fingerprint"]="702BA4B157F1E2B7D16B0C6A5FFC8A7DE2057DEB"' \
                    'encrypted:[0][0][0]["crypto"]["decrypted"]={"status":"full","header-mask":{"Subject":"..."}}'
    test_begin_subtest "confirm signed and encrypted PKCS#7 subject ($variant) signer User ID"
    if [ $NOTMUCH_GMIME_X509_CERT_VALIDITY -ne 1 ]; then
        test_subtest_known_broken
    fi
    test_json_nodes <<<"$output" \
                    'sig_uid:[0][0][0]["crypto"]["signed"]["status"][0]["userid"]="CN=Alice Lovelace"'

done

test_begin_subtest "confirm encryption-protected PKCS#7 subject (enc+legacy-disp)"
output=$(notmuch show --decrypt=true --format=json "id:smime-enc+legacy-disp@protected-headers.example")
test_json_nodes <<<"$output" \
                'encrypted:[0][0][0]["crypto"]["decrypted"]={"status":"full","header-mask":{"Subject":"..."}}' \
                'no_sig:[0][0][0]["crypto"]!"signed"'


# TODO: test that a part that looks like a legacy-display in
# multipart/signed, but not encrypted, is indexed and not stripped.

# TODO: test that a legacy-display in a decrypted subpart (not in the
# cryptographic payload) is indexed and not stripped.

# TODO: test that a legacy-display inside multiple MIME layers that
# include an encryption layer (e.g. multipart/encrypted around
# multipart/signed) is stripped and not indexed.

test_done
