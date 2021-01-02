#!/usr/bin/env bash
test_description='"notmuch new" in several variations'
. $(dirname "$0")/test-lib.sh || exit 1

test_begin_subtest "No new messages"
output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "No new mail."


test_begin_subtest "Single new message"
generate_message
output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "Added 1 new message to the database."

test_begin_subtest "Single message (full-scan)"
generate_message
output=$(NOTMUCH_NEW --debug --full-scan 2>&1)
test_expect_equal "$output" "Added 1 new message to the database."

test_begin_subtest "Multiple new messages"
generate_message
generate_message
output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "Added 2 new messages to the database."

test_begin_subtest "Multiple new messages (full-scan)"
generate_message
generate_message
output=$(NOTMUCH_NEW --debug --full-scan 2>&1)
test_expect_equal "$output" "Added 2 new messages to the database."

test_begin_subtest "No new messages (non-empty DB)"
output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "No new mail."

test_begin_subtest "No new messages (full-scan)"
output=$(NOTMUCH_NEW --debug --full-scan 2>&1)
test_expect_equal "$output" "No new mail."

test_begin_subtest "New directories"
rm -rf "${MAIL_DIR}"/* "${MAIL_DIR}"/.notmuch
mkdir "${MAIL_DIR}"/def
mkdir "${MAIL_DIR}"/ghi
generate_message [dir]=def

output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "Added 1 new message to the database."


test_begin_subtest "Alternate inode order"

rm -rf "${MAIL_DIR}"/.notmuch
mv "${MAIL_DIR}"/ghi "${MAIL_DIR}"/abc
rm "${MAIL_DIR}"/def/*
generate_message [dir]=abc

output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "Added 1 new message to the database."


test_begin_subtest "Message moved in"
rm -rf "${MAIL_DIR}"/* "${MAIL_DIR}"/.notmuch
generate_message
tmp_msg_filename=tmp/"$gen_msg_filename"
mkdir -p "$(dirname "$tmp_msg_filename")"
mv "$gen_msg_filename" "$tmp_msg_filename"
notmuch new > /dev/null
mv "$tmp_msg_filename" "$gen_msg_filename"
output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "Added 1 new message to the database."


test_begin_subtest "Renamed message"

generate_message
notmuch new > /dev/null
mv "$gen_msg_filename" "${gen_msg_filename}"-renamed
output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "(D) add_files, pass 2: queuing passed file ${gen_msg_filename} for deletion from database
No new mail. Detected 1 file rename."


test_begin_subtest "Deleted message"

rm "${gen_msg_filename}"-renamed
output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "(D) add_files, pass 3: queuing leftover file ${gen_msg_filename}-renamed for deletion from database
No new mail. Removed 1 message."



test_begin_subtest "Renamed directory"

generate_message [dir]=dir
generate_message [dir]=dir
generate_message [dir]=dir

notmuch new > /dev/null

mv "${MAIL_DIR}"/dir "${MAIL_DIR}"/dir-renamed

output=$(NOTMUCH_NEW --debug --full-scan)
test_expect_equal "$output" "(D) add_files, pass 2: queuing passed directory ${MAIL_DIR}/dir for deletion from database
No new mail. Detected 3 file renames."


test_begin_subtest "Deleted directory"
rm -rf "${MAIL_DIR}"/dir-renamed

output=$(NOTMUCH_NEW --debug --full-scan)
test_expect_equal "$output" "(D) add_files, pass 2: queuing passed directory ${MAIL_DIR}/dir-renamed for deletion from database
No new mail. Removed 3 messages."


test_begin_subtest "New directory (at end of list)"

generate_message [dir]=zzz
generate_message [dir]=zzz
generate_message [dir]=zzz

output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "Added 3 new messages to the database."


test_begin_subtest "Deleted directory (end of list)"

rm -rf "${MAIL_DIR}"/zzz

output=$(NOTMUCH_NEW --debug --full-scan)
test_expect_equal "$output" "(D) add_files, pass 3: queuing leftover directory ${MAIL_DIR}/zzz for deletion from database
No new mail. Removed 3 messages."


test_begin_subtest "New symlink to directory"

rm -rf "${MAIL_DIR}"/.notmuch
mv "${MAIL_DIR}" "${TMP_DIRECTORY}"/actual_maildir

mkdir "${MAIL_DIR}"
ln -s "${TMP_DIRECTORY}"/actual_maildir "${MAIL_DIR}"/symlink

output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "Added 1 new message to the database."


test_begin_subtest "New symlink to a file"
generate_message
external_msg_filename="${TMP_DIRECTORY}"/external/"$(basename "$gen_msg_filename")"
mkdir -p "$(dirname "$external_msg_filename")"
mv "$gen_msg_filename" "$external_msg_filename"
ln -s "$external_msg_filename" "$gen_msg_filename"
output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "Added 1 new message to the database."


test_begin_subtest "Broken symlink aborts"
ln -s does-not-exist "${MAIL_DIR}/broken"
output=$(NOTMUCH_NEW --debug 2>&1)
test_expect_equal "$output" \
"Error reading file ${MAIL_DIR}/broken: No such file or directory
Note: A fatal error was encountered: Something went wrong trying to read or write a file
No new mail."
rm "${MAIL_DIR}/broken"


test_begin_subtest "New two-level directory"

generate_message [dir]=two/levels
generate_message [dir]=two/levels
generate_message [dir]=two/levels

output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "Added 3 new messages to the database."


test_begin_subtest "Deleted two-level directory"

rm -rf "${MAIL_DIR}"/two

output=$(NOTMUCH_NEW --debug --full-scan)
test_expect_equal "$output" "(D) add_files, pass 3: queuing leftover directory ${MAIL_DIR}/two for deletion from database
No new mail. Removed 3 messages."

test_begin_subtest "One character directory at top level"

generate_message [dir]=A
generate_message [dir]=A/B
generate_message [dir]=A/B/C

output=$(NOTMUCH_NEW --debug)
test_expect_equal "$output" "Added 3 new messages to the database."

test_begin_subtest "Support single-message mbox"
cat > "${MAIL_DIR}"/mbox_file1 <<EOF
From test_suite@notmuchmail.org Fri Jan  5 15:43:57 2001
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Test mbox message 1

Body.
EOF
output=$(NOTMUCH_NEW --debug 2>&1)
test_expect_equal "$output" "Added 1 new message to the database."

# This test requires that notmuch new has been run at least once.
test_begin_subtest "Skip and report non-mail files"
generate_message
mkdir -p "${MAIL_DIR}"/.git && touch "${MAIL_DIR}"/.git/config
touch "${MAIL_DIR}"/ignored_file
touch "${MAIL_DIR}"/.ignored_hidden_file
cat > "${MAIL_DIR}"/mbox_file <<EOF
From test_suite@notmuchmail.org Fri Jan  5 15:43:57 2001
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Test mbox message 1

Body.

From test_suite@notmuchmail.org Fri Jan  5 15:43:57 2001
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Test mbox message 2

Body 2.
EOF
output=$(NOTMUCH_NEW --debug --full-scan 2>&1)
test_expect_equal "$output" \
"Note: Ignoring non-mail file: ${MAIL_DIR}/.git/config
Note: Ignoring non-mail file: ${MAIL_DIR}/.ignored_hidden_file
Note: Ignoring non-mail file: ${MAIL_DIR}/ignored_file
Note: Ignoring non-mail file: ${MAIL_DIR}/mbox_file
Added 1 new message to the database."
rm "${MAIL_DIR}"/mbox_file

test_begin_subtest "Ignore files and directories specified in new.ignore"
generate_message
notmuch config set new.ignore .git ignored_file .ignored_hidden_file
touch "${MAIL_DIR}"/.git # change .git's mtime for notmuch new to rescan.
NOTMUCH_NEW --debug 2>&1 | sort > OUTPUT
cat <<EOF > EXPECTED
(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/.git
(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/.ignored_hidden_file
(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/ignored_file
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/.git
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/.ignored_hidden_file
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/ignored_file
Added 1 new message to the database.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Ignore files and directories specified in new.ignore (full-scan)"
generate_message
notmuch config set new.ignore .git ignored_file .ignored_hidden_file
NOTMUCH_NEW --debug --full-scan 2>&1 | sort > OUTPUT
# reuse EXPECTED from previous test
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Ignore files and directories specified in new.ignore (multiple occurrences)"
notmuch config set new.ignore .git ignored_file .ignored_hidden_file
notmuch new > /dev/null # ensure that files/folders will be printed in ASCII order.
mkdir -p "${MAIL_DIR}"/one/two/three/.git
touch "${MAIL_DIR}"/{one,one/two,one/two/three}/ignored_file
output=$(NOTMUCH_NEW --debug --full-scan 2>&1 | sort)
test_expect_equal "$output" \
"(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/.git
(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/.ignored_hidden_file
(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/ignored_file
(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/one/ignored_file
(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/one/two/ignored_file
(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/one/two/three/.git
(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/one/two/three/ignored_file
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/.git
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/.ignored_hidden_file
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/ignored_file
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/one/ignored_file
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/one/two/ignored_file
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/one/two/three/.git
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/one/two/three/ignored_file
No new mail."


test_begin_subtest "Don't stop for ignored broken symlinks"
notmuch config set new.ignore .git ignored_file .ignored_hidden_file broken_link
ln -s i_do_not_exist "${MAIL_DIR}"/broken_link
output=$(NOTMUCH_NEW 2>&1)
test_expect_equal "$output" "No new mail."

test_begin_subtest "Ignore files and directories specified in new.ignore (regexp)"
notmuch config set new.ignore ".git" "/^bro.*ink\$/" "/ignored.*file/"
output=$(NOTMUCH_NEW --debug --full-scan 2>&1 | sort)
test_expect_equal "$output" \
"(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/.git
(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/.ignored_hidden_file
(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/broken_link
(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/ignored_file
(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/one/ignored_file
(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/one/two/ignored_file
(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/one/two/three/.git
(D) add_files, pass 1: explicitly ignoring ${MAIL_DIR}/one/two/three/ignored_file
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/.git
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/.ignored_hidden_file
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/broken_link
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/ignored_file
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/one/ignored_file
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/one/two/ignored_file
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/one/two/three/.git
(D) add_files, pass 2: explicitly ignoring ${MAIL_DIR}/one/two/three/ignored_file
No new mail."

test_begin_subtest "Quiet: No new mail."
output=$(NOTMUCH_NEW --quiet)
test_expect_equal "$output" ""

test_begin_subtest "Quiet: new, removed and renamed messages."
# new
generate_message
# deleted
notmuch search --format=text0 --output=files --limit=1 '*' | xargs -0 rm
# moved
mkdir "${MAIL_DIR}"/moved_messages
notmuch search --format=text0 --output=files --offset=1 --limit=1 '*' | xargs -0 -I {} mv {} "${MAIL_DIR}"/moved_messages
output=$(NOTMUCH_NEW --quiet)
test_expect_equal "$output" ""

OLDCONFIG=$(notmuch config get new.tags)

test_begin_subtest "Empty tags in new.tags are ignored"
notmuch config set new.tags "foo;;bar"
output=$(NOTMUCH_NEW --quiet 2>&1)
test_expect_equal "$output" ""

test_begin_subtest "Tags starting with '-' in new.tags are forbidden"
notmuch config set new.tags "-foo;bar"
output=$(NOTMUCH_NEW --debug 2>&1)
test_expect_equal "$output" "Error: tag '-foo' in new.tags: tag starting with '-' forbidden"

test_begin_subtest "Invalid tags set exit code"
test_expect_code 1 "NOTMUCH_NEW --debug 2>&1"

notmuch config set new.tags $OLDCONFIG


test_begin_subtest "Xapian exception: read only files"
chmod u-w  ${MAIL_DIR}/.notmuch/xapian/*.*
output=$(NOTMUCH_NEW --debug 2>&1 | sed 's/: .*$//' )
chmod u+w  ${MAIL_DIR}/.notmuch/xapian/*.*
test_expect_equal "$output" "A Xapian exception occurred opening database"


test_begin_subtest "Handle files vanishing between scandir and add_file"

# A file for scandir to find. It won't get indexed, so can be empty.
touch ${MAIL_DIR}/vanish

# Breakpoint to remove the file before indexing
cat <<EOF > notmuch-new-vanish.gdb
set breakpoint pending on
set logging file notmuch-new-vanish-gdb.log
set logging on
break notmuch_database_index_file
commands
shell rm -f ${MAIL_DIR}/vanish
continue
end
run
EOF

${TEST_GDB} --batch-silent --return-child-result -x notmuch-new-vanish.gdb \
    --args notmuch new 2>OUTPUT 1>/dev/null
echo "exit status: $?" >> OUTPUT

# Clean up the file in case gdb isn't available.
rm -f ${MAIL_DIR}/vanish

cat <<EOF > EXPECTED
Unexpected error with file ${MAIL_DIR}/vanish
add_file: Something went wrong trying to read or write a file
Error opening ${MAIL_DIR}/vanish: No such file or directory
exit status: 75
EOF
test_expect_equal_file EXPECTED OUTPUT

add_email_corpus broken
test_begin_subtest "reference loop does not crash"
test_expect_code 0 "notmuch show --format=json id:mid-loop-12@example.org id:mid-loop-21@example.org > OUTPUT"

test_begin_subtest "reference loop ordered by date"
threadid=$(notmuch search --output=threads  id:mid-loop-12@example.org)
notmuch show --format=mbox $threadid | grep '^Date'  > OUTPUT
cat <<EOF > EXPECTED
Date: Thu, 16 Jun 2016 22:14:41 -0400
Date: Fri, 17 Jun 2016 22:14:41 -0400
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
