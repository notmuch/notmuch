#!/bin/bash
test_description="\"notmuch new\" in several variations"
. ./test-lib.sh
test_expect_success "No new messages" '
output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "No new mail."

'
test_expect_success "Single new message" '
generate_message &&
output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "Added 1 new message to the database."

'
test_expect_success "Multiple new messages" '
generate_message &&
generate_message &&
output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "Added 2 new messages to the database."

'
test_expect_success "No new messages (non-empty DB)" '
output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "No new mail."

'
test_expect_success "New directories" '
rm -rf "${MAIL_DIR}"/* "${MAIL_DIR}"/.notmuch &&
mkdir "${MAIL_DIR}"/def &&
mkdir "${MAIL_DIR}"/ghi &&
generate_message [dir]=def &&

output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "Added 1 new message to the database."

'
test_expect_success "Alternate inode order" '

rm -rf "${MAIL_DIR}"/.notmuch &&
mv "${MAIL_DIR}"/ghi "${MAIL_DIR}"/abc &&
rm "${MAIL_DIR}"/def/* &&
generate_message [dir]=abc &&

output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "Added 1 new message to the database."

'
test_expect_success "Message moved in" '
rm -rf "${MAIL_DIR}"/* "${MAIL_DIR}"/.notmuch &&
generate_message &&
tmp_msg_filename=tmp/"$gen_msg_filename" &&
mkdir -p "$(dirname "$tmp_msg_filename")" &&
mv "$gen_msg_filename" "$tmp_msg_filename" &&
increment_mtime "${MAIL_DIR}" &&
$NOTMUCH new > /dev/null &&
mv "$tmp_msg_filename" "$gen_msg_filename" &&
increment_mtime "${MAIL_DIR}" &&
output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "Added 1 new message to the database."

'
test_expect_success "Renamed message" '

generate_message &&
$NOTMUCH new > /dev/null &&
mv "$gen_msg_filename" "${gen_msg_filename}"-renamed &&
increment_mtime "${MAIL_DIR}" &&
output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "No new mail. Detected 1 file rename."

'
test_expect_success "Deleted message" '

rm "${gen_msg_filename}"-renamed &&
increment_mtime "${MAIL_DIR}" &&
output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "No new mail. Removed 1 message."

'
test_expect_success "Renamed directory" '

generate_message [dir]=dir &&
generate_message [dir]=dir &&
generate_message [dir]=dir &&

$NOTMUCH new > /dev/null &&

mv "${MAIL_DIR}"/dir "${MAIL_DIR}"/dir-renamed &&
increment_mtime "${MAIL_DIR}" &&

output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "No new mail. Detected 3 file renames."

'
test_expect_success "Deleted directory" '

rm -rf "${MAIL_DIR}"/dir-renamed &&
increment_mtime "${MAIL_DIR}" &&

output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "No new mail. Removed 3 messages."

'
test_expect_success "New directory (at end of list)" '

generate_message [dir]=zzz &&
generate_message [dir]=zzz &&
generate_message [dir]=zzz &&

output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "Added 3 new messages to the database."

'
test_expect_success "Deleted directory (end of list)" '

rm -rf "${MAIL_DIR}"/zzz &&
increment_mtime "${MAIL_DIR}" &&

output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "No new mail. Removed 3 messages."

'
test_expect_success "New symlink to directory" '

rm -rf "${MAIL_DIR}"/.notmuch &&
mv "${MAIL_DIR}" "$PWD"/actual_maildir &&

mkdir "${MAIL_DIR}" &&
ln -s "$PWD"/actual_maildir "${MAIL_DIR}"/symlink &&

output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "Added 1 new message to the database."

'
test_expect_success "New symlink to a file" '
generate_message &&
external_msg_filename="$PWD"/external/"$(basename "$gen_msg_filename")" &&
mkdir -p "$(dirname "$external_msg_filename")" &&
mv "$gen_msg_filename" "$external_msg_filename" &&
ln -s "$external_msg_filename" "$gen_msg_filename" &&
increment_mtime "${MAIL_DIR}" &&
output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "Added 1 new message to the database."

'
test_expect_success "New two-level directory" '

generate_message [dir]=two/levels &&
generate_message [dir]=two/levels &&
generate_message [dir]=two/levels &&

output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "Added 3 new messages to the database."

'
test_expect_success "Deleted two-level directory" '

rm -rf "${MAIL_DIR}"/two &&
increment_mtime "${MAIL_DIR}" &&

output=$(NOTMUCH_NEW) &&
pass_if_equal "$output" "No new mail. Removed 3 messages."
'
test_done
