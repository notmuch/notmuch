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

test_begin_subtest "adding illegal prefix name, bad utf8"
notmuch config set index.header.$'\xFF' "List-Id" 2>&1 | sed 's/:.*$//' >OUTPUT
cat <<EOF > EXPECTED
Invalid utf8
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "adding illegal prefix name, reserved for notmuch"
notmuch config set index.header.list "List-Id" 2>OUTPUT
cat <<EOF > EXPECTED
Prefix names starting with lower case letters are reserved: list
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "adding illegal prefix name, non-word character."
notmuch config set index.header.l:st "List-Id" 2>OUTPUT
cat <<EOF > EXPECTED
Non-word character in prefix name: l:st
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "adding empty prefix name."
notmuch config set index.header. "List-Id" 2>OUTPUT
Non-word character in prefix name: l:st
cat <<EOF > EXPECTED
Empty prefix name: index.header.
EOF
test_expect_equal_file EXPECTED OUTPUT


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

test_begin_subtest "parse user prefix"
NOTMUCH_DEBUG_QUERY=t notmuch count 'List:"notmuch"' 2>&1 | grep Tmail >OUTPUT
cat <<EOF > EXPECTED
Query((Tmail AND XUList:notmuch@1))
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "parse user prefix, stemmed"
NOTMUCH_DEBUG_QUERY=t notmuch count 'List:notmuch' 2>&1 | grep Tmail >OUTPUT
cat <<EOF > EXPECTED
Query((Tmail AND ZXUList:notmuch@1))
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "parse user prefix, phrase"
NOTMUCH_DEBUG_QUERY=t notmuch count 'List:notmuchmail.org' 2>&1 | grep Tmail >OUTPUT
cat <<EOF > EXPECTED
Query((Tmail AND (XUList:notmuchmail@1 PHRASE 2 XUList:org@2)))
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "index user header"
notmuch config set index.header.List "List-Id"
notmuch reindex '*'
notmuch search --output=files List:notmuch | notmuch_search_files_sanitize | sort > OUTPUT
cat <<EOF > EXPECTED
MAIL_DIR/bar/baz/05:2,
MAIL_DIR/bar/baz/23:2,
MAIL_DIR/bar/baz/24:2,
MAIL_DIR/bar/cur/20:2,
MAIL_DIR/bar/new/21:2,
MAIL_DIR/bar/new/22:2,
MAIL_DIR/foo/cur/08:2,
MAIL_DIR/foo/new/03:2,
MAIL_DIR/new/04:2,
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "index user header, config from file"
field_name="Test"
printf "\n[index]\nheader.${field_name} = List-Id\n" >> notmuch-config
notmuch reindex '*'
notmuch search --output=files ${field_name}:notmuch | notmuch_search_files_sanitize | sort > OUTPUT
cat <<EOF > EXPECTED
MAIL_DIR/bar/baz/05:2,
MAIL_DIR/bar/baz/23:2,
MAIL_DIR/bar/baz/24:2,
MAIL_DIR/bar/cur/20:2,
MAIL_DIR/bar/new/21:2,
MAIL_DIR/bar/new/22:2,
MAIL_DIR/foo/cur/08:2,
MAIL_DIR/foo/new/03:2,
MAIL_DIR/new/04:2,
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
