#!/usr/bin/env bash
test_description='hooks'
. ./test-lib.sh || exit 1

HOOK_DIR=${MAIL_DIR}/.notmuch/hooks

create_echo_hook () {
    local TOKEN="${RANDOM}"
    mkdir -p ${HOOK_DIR}
    cat <<EOF >"${HOOK_DIR}/${1}"
#!/bin/sh
echo "${TOKEN}" > ${3}
EOF
    chmod +x "${HOOK_DIR}/${1}"
    echo "${TOKEN}" > ${2}
}

create_failing_hook () {
    mkdir -p ${HOOK_DIR}
    cat <<EOF >"${HOOK_DIR}/${1}"
#!/bin/sh
exit 13
EOF
    chmod +x "${HOOK_DIR}/${1}"
}

rm_hooks () {
    rm -rf ${HOOK_DIR}
}

# add a message to generate mail dir and database
add_message
# create maildir structure for notmuch-insert
mkdir -p "$MAIL_DIR"/{cur,new,tmp}

test_begin_subtest "pre-new is run"
rm_hooks
generate_message
create_echo_hook "pre-new" expected output
notmuch new > /dev/null
test_expect_equal_file expected output

test_begin_subtest "post-new is run"
rm_hooks
generate_message
create_echo_hook "post-new" expected output
notmuch new > /dev/null
test_expect_equal_file expected output

test_begin_subtest "post-insert hook is run"
rm_hooks
generate_message
create_echo_hook "post-insert" expected output
notmuch insert < "$gen_msg_filename"
test_expect_equal_file expected output

test_begin_subtest "pre-new is run before post-new"
rm_hooks
generate_message
create_echo_hook "pre-new" pre-new.expected pre-new.output
create_echo_hook "post-new" post-new.expected post-new.output
notmuch new > /dev/null
test_expect_equal_file post-new.expected post-new.output

test_begin_subtest "pre-new non-zero exit status (hook status)"
rm_hooks
generate_message
create_failing_hook "pre-new"
output=`notmuch new 2>&1`
test_expect_equal "$output" "Error: pre-new hook failed with status 13"

# depends on the previous subtest leaving broken hook behind
test_begin_subtest "pre-new non-zero exit status (notmuch status)"
test_expect_code 1 "notmuch new"

# depends on the previous subtests leaving 1 new message behind
test_begin_subtest "pre-new non-zero exit status aborts new"
rm_hooks
output=$(NOTMUCH_NEW)
test_expect_equal "$output" "Added 1 new message to the database."

test_begin_subtest "post-new non-zero exit status (hook status)"
rm_hooks
generate_message
create_failing_hook "post-new"
NOTMUCH_NEW 2>output.stderr >output
cat output.stderr >> output
echo "Added 1 new message to the database." > expected
echo "Error: post-new hook failed with status 13" >> expected
test_expect_equal_file expected output

# depends on the previous subtest leaving broken hook behind
test_begin_subtest "post-new non-zero exit status (notmuch status)"
test_expect_code 1 "notmuch new"

test_begin_subtest "post-insert hook does not affect insert status"
rm_hooks
generate_message
create_failing_hook "post-insert"
test_expect_success "notmuch insert < \"$gen_msg_filename\" > /dev/null"

test_begin_subtest "hook without executable permissions"
rm_hooks
mkdir -p ${HOOK_DIR}
cat <<EOF >"${HOOK_DIR}/pre-new"
#!/bin/sh
echo foo
EOF
output=`notmuch new 2>&1`
test_expect_code 1 "notmuch new"

test_begin_subtest "hook execution failure"
rm_hooks
mkdir -p ${HOOK_DIR}
cat <<EOF >"${HOOK_DIR}/pre-new"
no hashbang, execl fails
EOF
chmod +x "${HOOK_DIR}/pre-new"
test_expect_code 1 "notmuch new"

test_done
