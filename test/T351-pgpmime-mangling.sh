#!/usr/bin/env bash

test_description='PGP/MIME message mangling'
. $(dirname "$0")/test-lib.sh || exit 1

add_gnupg_home
add_email_corpus mangling

bodytext='["body"][0]["content"][1]["content"]="The password is \"abcd1234!\", please do not tell anyone.\n"'

test_begin_subtest "show 'Mixed-Up' mangled PGP/MIME message correctly"
output=$(notmuch show --format=json --decrypt=true id:mixed-up@mangling.notmuchmail.org)
test_json_nodes <<<"$output" \
                'body:[0][0][0]'"$bodytext"

test_begin_subtest "reply to 'Mixed-Up' mangled PGP/MIME message correctly"
output=$(notmuch reply --format=json --decrypt=true id:mixed-up@mangling.notmuchmail.org)
test_json_nodes <<<"$output" \
                'body:["original"]'"$bodytext"

test_begin_subtest "repaired 'Mixed-up' messages can be found with index.repaired=mixedup"
output=$(notmuch search --output=messages property:index.repaired=mixedup)
test_expect_equal "$output" id:mixed-up@mangling.notmuchmail.org

test_begin_subtest "index cleartext of 'Mixed-Up' mangled PGP/MIME message"
test_expect_success 'notmuch reindex --decrypt=true id:mixed-up@mangling.notmuchmail.org'

test_begin_subtest "search cleartext of 'Mixed-Up' mangled PGP/MIME message"
output=$(notmuch search --output=messages body:password)
test_expect_equal "$output" id:mixed-up@mangling.notmuchmail.org

test_done
