#!/usr/bin/env bash

test_description='"notmuch config"'
. ./test-lib.sh

test_begin_subtest "Get string value"
test_expect_equal "$(notmuch config get user.name)" "Notmuch Test Suite"

test_begin_subtest "Get list value"
test_expect_equal "$(notmuch config get new.tags)" "\
unread
inbox"

test_begin_subtest "Set string value"
notmuch config set foo.string "this is a string value"
test_expect_equal "$(notmuch config get foo.string)" "this is a string value"

test_begin_subtest "Set string value again"
notmuch config set foo.string "this is another string value"
test_expect_equal "$(notmuch config get foo.string)" "this is another string value"

test_begin_subtest "Set list value"
notmuch config set foo.list this "is a" "list value"
test_expect_equal "$(notmuch config get foo.list)" "\
this
is a
list value"

test_begin_subtest "Set list value again"
notmuch config set foo.list this "is another" "list value"
test_expect_equal "$(notmuch config get foo.list)" "\
this
is another
list value"

test_begin_subtest "Remove key"
notmuch config set foo.remove baz
notmuch config set foo.remove
test_expect_equal "$(notmuch config get foo.remove)" ""

test_begin_subtest "Remove non-existent key"
notmuch config set foo.nonexistent
test_expect_equal "$(notmuch config get foo.nonexistent)" ""

test_begin_subtest "List all items"
notmuch config set database.path "/canonical/path"
output=$(notmuch config list)
test_expect_equal "$output" "\
database.path=/canonical/path
user.name=Notmuch Test Suite
user.primary_email=test_suite@notmuchmail.org
user.other_email=test_suite_other@notmuchmail.org;test_suite@otherdomain.org
new.tags=unread;inbox;
new.ignore=
search.exclude_tags=
maildir.synchronize_flags=true
foo.string=this is another string value
foo.list=this;is another;list value;"

test_begin_subtest "Top level --config=FILE option"
cp "${NOTMUCH_CONFIG}" alt-config
notmuch --config=alt-config config set user.name "Another Name"
test_expect_equal "$(notmuch --config=alt-config config get user.name)" \
    "Another Name"

test_begin_subtest "Top level --config=FILE option changed the right file"
test_expect_equal "$(notmuch config get user.name)" \
    "Notmuch Test Suite"

test_begin_subtest "Read config file through a symlink"
ln -s alt-config alt-config-link
test_expect_equal "$(notmuch --config=alt-config-link config get user.name)" \
    "Another Name"

test_begin_subtest "Write config file through a symlink"
notmuch --config=alt-config-link config set user.name "Link Name"
test_expect_equal "$(notmuch --config=alt-config-link config get user.name)" \
    "Link Name"

test_begin_subtest "Writing config file through symlink follows symlink"
test_expect_equal "$(readlink alt-config-link)" "alt-config"

test_done
