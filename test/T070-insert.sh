#!/usr/bin/env bash
test_description='"notmuch insert"'
. $(dirname "$0")/test-lib.sh || exit 1

# subtests about file permissions assume that we're working with umask
# 022 by default.
umask 022

# Create directories and database before inserting.
mkdir -p "$MAIL_DIR"/{cur,new,tmp}
mkdir -p "$MAIL_DIR"/Drafts/{cur,new,tmp}
notmuch new > /dev/null

# We use generate_message to create the temporary message files.
# They happen to be in the mail directory already but that is okay
# since we do not call notmuch new hereafter.

gen_insert_msg() {
    generate_message \
	"[subject]=\"insert-subject\"" \
	"[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" \
	"[body]=\"insert-message\""
}

test_begin_subtest "Insert zero-length file"
test_expect_code 1 "notmuch insert < /dev/null"

# This test is a proxy for other errors that may occur while trying to
# add a message to the notmuch database, e.g. database locked.
test_begin_subtest "Insert non-message"
test_expect_code 1 "echo bad_message | notmuch insert"

test_begin_subtest "Database empty so far"
test_expect_equal "0" "`notmuch count --output=messages '*'`"

test_begin_subtest "Insert message"
gen_insert_msg
notmuch insert < "$gen_msg_filename"
cur_msg_filename=$(notmuch search --output=files "subject:insert-subject")
test_expect_equal_file "$cur_msg_filename" "$gen_msg_filename"

test_begin_subtest "Permissions on inserted message should be 0600"
test_expect_equal "600" "$(stat -c %a "$cur_msg_filename")"

test_begin_subtest "Insert message adds default tags"
output=$(notmuch show --format=json "subject:insert-subject")
expected='[[[{
 "id": "'"${gen_msg_id}"'",
 "crypto": {},
 "match": true,
 "excluded": false,
 "filename": ["'"${cur_msg_filename}"'"],
 "timestamp": 946728000,
 "date_relative": "2000-01-01",
 "tags": ["inbox","unread"],
 "headers": {
  "Subject": "insert-subject",
  "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
  "To": "Notmuch Test Suite <test_suite@notmuchmail.org>",
  "Date": "Sat, 01 Jan 2000 12:00:00 +0000"},
 "body": [{"id": 1,
  "content-type": "text/plain",
  "content": "insert-message\n"}]},
 []]]]'
test_expect_equal_json "$output" "$expected"

test_begin_subtest "Insert duplicate message"
notmuch insert +duptag -unread < "$gen_msg_filename"
output=$((`notmuch search --output=files "subject:insert-subject" | wc -l`))
test_expect_equal "$output" 2

test_begin_subtest "Duplicate message does not change tags"
output=$(notmuch search --format=json --output=tags "subject:insert-subject")
test_expect_equal_json "$output" '["inbox", "unread"]'

test_begin_subtest "Insert message, add tag"
gen_insert_msg
notmuch insert +custom < "$gen_msg_filename"
output=$(notmuch search --output=messages tag:custom)
test_expect_equal "$output" "id:$gen_msg_id"

test_begin_subtest "Insert tagged world-readable message"
gen_insert_msg
notmuch insert --world-readable +world-readable-test < "$gen_msg_filename"
cur_msg_filename=$(notmuch search --output=files "tag:world-readable-test")
test_expect_equal_file "$cur_msg_filename" "$gen_msg_filename"

test_begin_subtest "Permissions on inserted world-readable message should be 0644"
test_expect_equal "644" "$(stat -c %a "$cur_msg_filename")"

test_begin_subtest "Insert tagged world-readable message with group-only umask"
oldumask=$(umask)
umask 027
gen_insert_msg
notmuch insert --world-readable +world-readable-umask-test < "$gen_msg_filename"
cur_msg_filename=$(notmuch search --output=files "tag:world-readable-umask-test")
umask "$oldumask"
test_expect_equal_file "$cur_msg_filename" "$gen_msg_filename"

test_begin_subtest "Permissions on inserted world-readable message with funny umask should be 0640"
test_expect_equal "640" "$(stat -c %a "$cur_msg_filename")"

test_begin_subtest "Insert message, add/remove tags"
gen_insert_msg
notmuch insert +custom -unread < "$gen_msg_filename"
output=$(notmuch search --output=messages tag:custom NOT tag:unread)
test_expect_equal "$output" "id:$gen_msg_id"

test_begin_subtest "Insert message with default tags stays in new/"
gen_insert_msg
notmuch insert < "$gen_msg_filename"
output=$(notmuch search --output=files id:$gen_msg_id)
dirname=$(dirname "$output")
test_expect_equal "$dirname" "$MAIL_DIR/new"

test_begin_subtest "Insert message with non-maildir synced tags stays in new/"
gen_insert_msg
notmuch insert +custom -inbox < "$gen_msg_filename"
output=$(notmuch search --output=files id:$gen_msg_id)
dirname=$(dirname "$output")
test_expect_equal "$dirname" "$MAIL_DIR/new"

test_begin_subtest "Insert message with custom new.tags goes to cur/"
OLDCONFIG=$(notmuch config get new.tags)
notmuch config set new.tags test
gen_insert_msg
notmuch insert < "$gen_msg_filename"
output=$(notmuch search --output=files id:$gen_msg_id)
dirname=$(dirname "$output")
notmuch config set new.tags $OLDCONFIG
test_expect_equal "$dirname" "$MAIL_DIR/cur"

# additional check on the previous message
test_begin_subtest "Insert message with custom new.tags actually gets the tags"
output=$(notmuch search --output=tags id:$gen_msg_id)
test_expect_equal "$output" "test"

test_begin_subtest "Insert message with maildir synced tags goes to cur/"
gen_insert_msg
notmuch insert +flagged < "$gen_msg_filename"
output=$(notmuch search --output=files id:$gen_msg_id)
dirname=$(dirname "$output")
test_expect_equal "$dirname" "$MAIL_DIR/cur"

test_begin_subtest "Insert message with maildir sync off goes to new/"
OLDCONFIG=$(notmuch config get maildir.synchronize_flags)
notmuch config set maildir.synchronize_flags false
gen_insert_msg
notmuch insert +flagged < "$gen_msg_filename"
output=$(notmuch search --output=files id:$gen_msg_id)
dirname=$(dirname "$output")
notmuch config set maildir.synchronize_flags $OLDCONFIG
test_expect_equal "$dirname" "$MAIL_DIR/new"

test_begin_subtest "Insert message into folder"
gen_insert_msg
notmuch insert --folder=Drafts < "$gen_msg_filename"
output=$(notmuch search --output=files path:Drafts/new)
dirname=$(dirname "$output")
test_expect_equal "$dirname" "$MAIL_DIR/Drafts/new"

test_begin_subtest "Insert message into top level folder"
gen_insert_msg
notmuch insert --folder="" < "$gen_msg_filename"
output=$(notmuch search --output=files id:${gen_msg_id})
dirname=$(dirname "$output")
test_expect_equal "$dirname" "$MAIL_DIR/new"

test_begin_subtest "Insert message into folder with trailing /"
gen_insert_msg
notmuch insert --folder=Drafts/ < "$gen_msg_filename"
output=$(notmuch search --output=files id:${gen_msg_id})
dirname=$(dirname "$output")
test_expect_equal "$dirname" "$MAIL_DIR/Drafts/new"

test_begin_subtest "Insert message into folder, add/remove tags"
gen_insert_msg
notmuch insert --folder=Drafts +draft -unread < "$gen_msg_filename"
output=$(notmuch search --output=messages path:Drafts/cur tag:draft NOT tag:unread)
test_expect_equal "$output" "id:$gen_msg_id"

test_begin_subtest "Insert message into non-existent folder"
gen_insert_msg
test_expect_code 1 "notmuch insert --folder=nonesuch < $gen_msg_filename"

test_begin_subtest "Insert message, create folder"
gen_insert_msg
notmuch insert --folder=F --create-folder +folder < "$gen_msg_filename"
output=$(notmuch search --output=files path:F/new tag:folder)
basename=$(basename "$output")
test_expect_equal_file "$gen_msg_filename" "$MAIL_DIR/F/new/${basename}"

test_begin_subtest "Insert message, create subfolder"
gen_insert_msg
notmuch insert --folder=F/G/H/I/J --create-folder +folder < "$gen_msg_filename"
output=$(notmuch search --output=files path:F/G/H/I/J/new tag:folder)
basename=$(basename "$output")
test_expect_equal_file "$gen_msg_filename" "${MAIL_DIR}/F/G/H/I/J/new/${basename}"

test_begin_subtest "Created subfolder should have permissions 0700"
test_expect_equal "700" "$(stat -c %a "${MAIL_DIR}/F/G/H/I/J")"
test_begin_subtest "Created subfolder new/ should also have permissions 0700"
test_expect_equal "700" "$(stat -c %a "${MAIL_DIR}/F/G/H/I/J/new")"

test_begin_subtest "Insert message, create world-readable subfolder"
gen_insert_msg
notmuch insert --folder=F/G/H/I/J/K --create-folder --world-readable +folder-world-readable < "$gen_msg_filename"
output=$(notmuch search --output=files path:F/G/H/I/J/K/new tag:folder-world-readable)
basename=$(basename "$output")
test_expect_equal_file "$gen_msg_filename" "${MAIL_DIR}/F/G/H/I/J/K/new/${basename}"

test_begin_subtest "Created world-readable subfolder should have permissions 0755"
test_expect_equal "755" "$(stat -c %a "${MAIL_DIR}/F/G/H/I/J/K")"
test_begin_subtest "Created world-readable subfolder new/ should also have permissions 0755"
test_expect_equal "755" "$(stat -c %a "${MAIL_DIR}/F/G/H/I/J/K/new")"

test_begin_subtest "Insert message, create existing subfolder"
gen_insert_msg
notmuch insert --folder=F/G/H/I/J --create-folder +folder < "$gen_msg_filename"
output=$(notmuch count path:F/G/H/I/J/new tag:folder)
test_expect_equal "$output" "2"

test_begin_subtest "Insert message, create invalid subfolder"
gen_insert_msg
test_expect_code 1 "notmuch insert --folder=../G --create-folder < $gen_msg_filename"

OLDCONFIG=$(notmuch config get new.tags)

test_begin_subtest "Empty tags in new.tags are ignored"
notmuch config set new.tags "foo;;bar"
gen_insert_msg
notmuch insert < $gen_msg_filename
output=$(notmuch show --format=json id:$gen_msg_id)
test_json_nodes <<<"$output" \
		'new_tags:[0][0][0]["tags"] = ["bar", "foo"]'

test_begin_subtest "Tags starting with '-' in new.tags are forbidden"
notmuch config set new.tags "-foo;bar"
gen_insert_msg
output=$(notmuch insert < $gen_msg_filename 2>&1)
test_expect_equal "$output" "Error: tag '-foo' in new.tags: tag starting with '-' forbidden"

test_begin_subtest "Invalid tags set exit code"
test_expect_code 1 "notmuch insert < $gen_msg_filename 2>&1"

notmuch config set new.tags $OLDCONFIG

# DUPLICATE_MESSAGE_ID is not tested here, because it should actually pass.
# pregenerate all of the test shims
for code in  FILE_NOT_EMAIL READ_ONLY_DATABASE UPGRADE_REQUIRED PATH_ERROR OUT_OF_MEMORY XAPIAN_EXCEPTION; do
    make_shim shim-$code <<EOF
#include <notmuch.h>
#include <stdio.h>
notmuch_status_t
notmuch_database_index_file (notmuch_database_t *notmuch,
                             const char *filename,
                             notmuch_indexopts_t *indexopts,
                             notmuch_message_t **message_ret)
{
  return NOTMUCH_STATUS_$code;
}
EOF
done

gen_insert_msg

for code in  FILE_NOT_EMAIL READ_ONLY_DATABASE UPGRADE_REQUIRED PATH_ERROR; do
    test_begin_subtest "EXIT_FAILURE when index_file returns $code"
    test_expect_code 1 "notmuch_with_shim shim-$code insert < \"$gen_msg_filename\""

    test_begin_subtest "success exit with --keep when index_file returns $code"
    test_expect_code 0 "notmuch_with_shim shim-$code insert --keep < \"$gen_msg_filename\""
done

for code in OUT_OF_MEMORY XAPIAN_EXCEPTION ; do
    test_begin_subtest "EX_TEMPFAIL when index_file returns $code"
    test_expect_code 75 "notmuch_with_shim shim-$code insert < \"$gen_msg_filename\""

    test_begin_subtest "success exit with --keep when index_file returns $code"
    test_expect_code 0 "notmuch_with_shim shim-$code insert --keep < \"$gen_msg_filename\""
done

test_done
