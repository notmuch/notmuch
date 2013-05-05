#!/usr/bin/env bash

test_description='"notmuch setup"'
. ./test-lib.sh

test_begin_subtest "Create a new config interactively"
notmuch --config=new-notmuch-config > /dev/null <<EOF
Test Suite
test.suite@example.com
another.suite@example.com

/path/to/maildir
foo bar
baz
EOF
output=$(notmuch --config=new-notmuch-config config list)
test_expect_equal "$output" "\
database.path=/path/to/maildir
user.name=Test Suite
user.primary_email=test.suite@example.com
user.other_email=another.suite@example.com;
new.tags=foo;bar;
new.ignore=
search.exclude_tags=baz;
maildir.synchronize_flags=true"

test_done
