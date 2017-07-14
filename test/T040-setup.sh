#!/usr/bin/env bash

test_description='"notmuch setup"'
. ./test-lib.sh || exit 1

test_begin_subtest "Notmuch new without a config suggests notmuch setup"
output=$(notmuch --config=new-notmuch-config new 2>&1)
test_expect_equal "$output" "\
Configuration file new-notmuch-config not found.
Try running 'notmuch setup' to create a configuration."

test_begin_subtest "Create a new config interactively"
notmuch --config=new-notmuch-config > /dev/null <<EOF
Test Suite
test.suite@example.com
another.suite@example.com

/path/to/maildir
foo bar
baz
EOF

if [ "${NOTMUCH_GMIME_MAJOR}" -lt 3 ]; then
    config_gpg_path="crypto.gpg_path=gpg
"
fi

output=$(notmuch --config=new-notmuch-config config list | notmuch_built_with_sanitize)
test_expect_equal "$output" "\
database.path=/path/to/maildir
user.name=Test Suite
user.primary_email=test.suite@example.com
user.other_email=another.suite@example.com;
new.tags=foo;bar;
new.ignore=
search.exclude_tags=baz;
maildir.synchronize_flags=true
""${config_gpg_path}""\
built_with.compact=something
built_with.field_processor=something
built_with.retry_lock=something"

test_done
