#!/usr/bin/env bash
test_description='database upgrades'
. $(dirname "$0")/test-lib.sh || exit 1

test_require_external_prereq xapian-metadata

XAPIAN_PATH=$MAIL_DIR/.notmuch/xapian
BACKUP_PATH=$MAIL_DIR/.notmuch/backups

delete_feature () {
    local key=$1
    features=$(xapian-metadata get $XAPIAN_PATH features | grep -v "^$key")
    xapian-metadata set $XAPIAN_PATH features "$features"
}

add_email_corpus

for key in 'multiple paths per message' \
	       'relative directory paths' \
	       'exact folder:/path: search' \
	       'mail documents for missing messages' \
	       'modification tracking'; do
    backup_database
    test_begin_subtest "upgrade is triggered by missing '$key'"
    delete_feature "$key"
    output=$(notmuch new | grep Welcome)
    test_expect_equal \
	"$output" \
	"Welcome to a new version of notmuch! Your database will now be upgraded."

    restore_database

    backup_database
    test_begin_subtest "backup can be restored ['$key']"
    notmuch dump > BEFORE
    delete_feature "$key"
    notmuch new
    notmuch tag -inbox '*'
    dump_file=$(echo ${BACKUP_PATH}/dump*)
    notmuch restore --input=$dump_file
    notmuch dump > AFTER
    test_expect_equal_file BEFORE AFTER
    restore_database
done

for key in 'from/subject/message-ID in database' \
	       'indexed MIME types' \
	       'index body and headers separately'; do
    backup_database
    test_begin_subtest "upgrade not triggered by missing '$key'"
    delete_feature "$key"
    output=$(notmuch new | grep Welcome)
    test_expect_equal "$output" ""
    restore_database
done

test_done
