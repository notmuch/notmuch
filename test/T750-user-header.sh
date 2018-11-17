#!/usr/bin/env bash
test_description='indexing user specified headers'
. $(dirname "$0")/test-lib.sh || exit 1

test_begin_subtest "error adding user header before initializing DB"
notmuch config set index.header.List List-Id 2>&1 | notmuch_dir_sanitize > OUTPUT
cat <<EOF > EXPECTED
Error opening database at MAIL_DIR/.notmuch: No such file or directory
EOF
test_expect_equal_file EXPECTED OUTPUT

add_email_corpus

notmuch search '*' | notmuch_search_sanitize > initial-threads
notmuch search --output=messages '*' > initial-message-ids
notmuch dump > initial-dump

test_begin_subtest "adding user header"
test_expect_code 0 "notmuch config set index.header.List \"List-Id\""

test_begin_subtest "adding existing user header"
test_expect_code 0 "notmuch config set index.header.List \"List-Id\""


test_begin_subtest "retrieve user header"
output=$(notmuch config get index.header.List)
test_expect_equal "List-Id" "$output"

test_begin_subtest 'reindex after adding header preserves threads'
notmuch reindex '*'
notmuch search '*' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file initial-threads OUTPUT

test_begin_subtest "List all user headers"
notmuch config set index.header.Spam "X-Spam"
notmuch config list | grep ^index.header | notmuch_config_sanitize > OUTPUT
cat <<EOF > EXPECTED
index.header.List=List-Id
index.header.Spam=X-Spam
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
