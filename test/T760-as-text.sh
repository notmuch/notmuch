#!/usr/bin/env bash
test_description='index attachments as text'
. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus indexing
test_begin_subtest "empty as_text; skip text/x-diff"
messages=$(notmuch count id:20200930101213.2m2pt3jrspvcrxfx@localhost.localdomain)
count=$(notmuch count id:20200930101213.2m2pt3jrspvcrxfx@localhost.localdomain and ersatz)
test_expect_equal "$messages,$count" "1,0"

notmuch config set index.as_text "^text/"
add_email_corpus indexing

test_begin_subtest "as_index is text/; find text/x-diff"
notmuch search id:20200930101213.2m2pt3jrspvcrxfx@localhost.localdomain > EXPECTED
notmuch search id:20200930101213.2m2pt3jrspvcrxfx@localhost.localdomain and ersatz > OUTPUT
test_expect_equal_file_nonempty EXPECTED OUTPUT

test_begin_subtest "reindex with empty as_text, skips text/x-diff"
notmuch config set index.as_text
notmuch reindex '*'
messages=$(notmuch count id:20200930101213.2m2pt3jrspvcrxfx@localhost.localdomain)
count=$(notmuch count id:20200930101213.2m2pt3jrspvcrxfx@localhost.localdomain and ersatz)
test_expect_equal "$messages,$count" "1,0"

test_begin_subtest "reindex with empty as_text; skips application/pdf"
notmuch config set index.as_text
notmuch reindex '*'
gmessages=$(notmuch count id:871qo9p4tf.fsf@tethera.net)
count=$(notmuch count id:871qo9p4tf.fsf@tethera.net and body:not-really-PDF)
test_expect_equal "$messages,$count" "1,0"

test_begin_subtest "reindex with as_text as text/; finds text/x-diff"
notmuch config set index.as_text "^text/"
notmuch reindex '*'
notmuch search id:20200930101213.2m2pt3jrspvcrxfx@localhost.localdomain > EXPECTED
notmuch search id:20200930101213.2m2pt3jrspvcrxfx@localhost.localdomain and ersatz > OUTPUT
test_expect_equal_file_nonempty EXPECTED OUTPUT

test_begin_subtest "reindex with as_text as text/; skips application/pdf"
notmuch config set index.as_text "^text/"
notmuch config set index.as_text
notmuch reindex '*'
messages=$(notmuch count id:871qo9p4tf.fsf@tethera.net)
count=$(notmuch count id:871qo9p4tf.fsf@tethera.net and body:not-really-PDF)
test_expect_equal "$messages,$count" "1,0"

test_begin_subtest "as_text has multiple regexes"
notmuch config set index.as_text "blahblah;^text/"
notmuch reindex '*'
notmuch search id:20200930101213.2m2pt3jrspvcrxfx@localhost.localdomain > EXPECTED
notmuch search id:20200930101213.2m2pt3jrspvcrxfx@localhost.localdomain and ersatz > OUTPUT
test_expect_equal_file_nonempty EXPECTED OUTPUT

test_begin_subtest "as_text is non-anchored regex"
notmuch config set index.as_text "e.t/"
notmuch reindex '*'
notmuch search id:20200930101213.2m2pt3jrspvcrxfx@localhost.localdomain > EXPECTED
notmuch search id:20200930101213.2m2pt3jrspvcrxfx@localhost.localdomain and ersatz > OUTPUT
test_expect_equal_file_nonempty EXPECTED OUTPUT

test_begin_subtest "as_text is 'application/pdf'"
notmuch config set index.as_text "^application/pdf$"
notmuch reindex '*'
notmuch search id:871qo9p4tf.fsf@tethera.net > EXPECTED
notmuch search id:871qo9p4tf.fsf@tethera.net and '"not really PDF"' > OUTPUT
test_expect_equal_file_nonempty EXPECTED OUTPUT

test_begin_subtest "as_text is bad regex"
notmuch config set index.as_text '['
notmuch reindex '*' >& OUTPUT
cat<<EOF > EXPECTED
Error in index.as_text: Invalid regular expression: [
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
