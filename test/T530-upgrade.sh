#!/usr/bin/env bash
test_description="database upgrade"

. ./test-lib.sh || exit 1

dbtarball=database-v1.tar.xz

# XXX: Accomplish the same with test lib helpers
if [ ! -e ${TEST_DIRECTORY}/test-databases/${dbtarball} ]; then
    test_subtest_missing_external_prereq_["${dbtarball} - fetch with 'make download-test-databases'"]=t
fi

test_begin_subtest "database checksum"
test_expect_success \
    '( cd $TEST_DIRECTORY/test-databases &&
       sha256sum --quiet --check --status ${dbtarball}.sha256 )'

tar xf $TEST_DIRECTORY/test-databases/${dbtarball} -C ${MAIL_DIR} --strip-components=1

test_begin_subtest "folder: search does not work with old database version"
output=$(notmuch search folder:foo)
test_expect_equal "$output" ""

test_begin_subtest "path: search does not work with old database version"
output=$(notmuch search path:foo)
test_expect_equal "$output" ""

test_begin_subtest "pre upgrade dump"
test_expect_success 'notmuch dump | sort > pre-upgrade-dump'

test_begin_subtest "database upgrade from format version 1"
output=$(notmuch new | sed -e 's/^Backing up tags to .*$/Backing up tags to FILENAME/')
test_expect_equal "$output" "\
Welcome to a new version of notmuch! Your database will now be upgraded.
This process is safe to interrupt.
Backing up tags to FILENAME
Your notmuch database has now been upgraded.
No new mail."

test_begin_subtest "tag backup matches pre-upgrade dump"
gunzip -c ${MAIL_DIR}/.notmuch/dump-*.gz | sort > backup-dump
test_expect_equal_file pre-upgrade-dump backup-dump

test_begin_subtest "folder: no longer matches in the middle of path"
output=$(notmuch search folder:baz)
test_expect_equal "$output" ""

test_begin_subtest "folder: search"
output=$(notmuch search --output=files folder:foo | notmuch_search_files_sanitize | sort)
test_expect_equal "$output" "MAIL_DIR/foo/06:2,
MAIL_DIR/foo/cur/07:2,
MAIL_DIR/foo/cur/08:2,
MAIL_DIR/foo/new/03:2,
MAIL_DIR/foo/new/09:2,
MAIL_DIR/foo/new/10:2,"

test_begin_subtest "top level folder: search"
output=$(notmuch search --output=files folder:'""' | notmuch_search_files_sanitize | sort)
# bar/18:2, is a duplicate of cur/51:2,
test_expect_equal "$output" "MAIL_DIR/01:2,
MAIL_DIR/02:2,
MAIL_DIR/bar/18:2,
MAIL_DIR/cur/29:2,
MAIL_DIR/cur/30:2,
MAIL_DIR/cur/31:2,
MAIL_DIR/cur/32:2,
MAIL_DIR/cur/33:2,
MAIL_DIR/cur/34:2,
MAIL_DIR/cur/35:2,
MAIL_DIR/cur/36:2,
MAIL_DIR/cur/37:2,
MAIL_DIR/cur/38:2,
MAIL_DIR/cur/39:2,
MAIL_DIR/cur/40:2,
MAIL_DIR/cur/41:2,
MAIL_DIR/cur/42:2,
MAIL_DIR/cur/43:2,
MAIL_DIR/cur/44:2,
MAIL_DIR/cur/45:2,
MAIL_DIR/cur/46:2,
MAIL_DIR/cur/47:2,
MAIL_DIR/cur/48:2,
MAIL_DIR/cur/49:2,
MAIL_DIR/cur/50:2,
MAIL_DIR/cur/51:2,
MAIL_DIR/cur/52:2,
MAIL_DIR/cur/53:2,
MAIL_DIR/new/04:2,"

test_begin_subtest "path: search"
output=$(notmuch search --output=files path:"bar" | notmuch_search_files_sanitize | sort)
# cur/51:2, is a duplicate of bar/18:2,
test_expect_equal "$output" "MAIL_DIR/bar/17:2,
MAIL_DIR/bar/18:2,
MAIL_DIR/cur/51:2,"

test_begin_subtest "top level path: search"
output=$(notmuch search --output=files path:'""' | notmuch_search_files_sanitize | sort)
test_expect_equal "$output" "MAIL_DIR/01:2,
MAIL_DIR/02:2,"

test_begin_subtest "recursive path: search"
output=$(notmuch search --output=files path:"bar/**" | notmuch_search_files_sanitize | sort)
# cur/51:2, is a duplicate of bar/18:2,
test_expect_equal "$output" "MAIL_DIR/bar/17:2,
MAIL_DIR/bar/18:2,
MAIL_DIR/bar/baz/05:2,
MAIL_DIR/bar/baz/23:2,
MAIL_DIR/bar/baz/24:2,
MAIL_DIR/bar/baz/cur/25:2,
MAIL_DIR/bar/baz/cur/26:2,
MAIL_DIR/bar/baz/new/27:2,
MAIL_DIR/bar/baz/new/28:2,
MAIL_DIR/bar/cur/19:2,
MAIL_DIR/bar/cur/20:2,
MAIL_DIR/bar/new/21:2,
MAIL_DIR/bar/new/22:2,
MAIL_DIR/cur/51:2,"

test_done
