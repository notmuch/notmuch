#!/usr/bin/env bash
test_description='hooks'
. $(dirname "$0")/test-lib.sh || exit 1

create_echo_script () {
    local TOKEN="${RANDOM}"
    mkdir -p ${BIN_DIR}
    cat <<EOF >"${BIN_DIR}/${1}"
#!/bin/sh
echo "${TOKEN}" > ${3}
EOF
    chmod +x "${BIN_DIR}/${1}"
    echo "${TOKEN}" > ${2}
}

create_printenv_script () {
    mkdir -p ${BIN_DIR}
    cat <<EOF >"${BIN_DIR}/${1}"
#!/bin/sh
printenv "${2}" > "${3}"
EOF
    chmod +x "${BIN_DIR}/${1}"
}

# add a message to generate mail dir and database
add_message

BIN_DIR=`pwd`/bin
PATH=$BIN_DIR:$PATH

test_begin_subtest "'notmuch foo' runs notmuch-foo"
rm -rf ${BIN_DIR}
create_echo_script "notmuch-foo" EXPECTED OUTPUT $HOOK_DIR
notmuch foo
test_expect_equal_file_nonempty EXPECTED OUTPUT

create_printenv_script "notmuch-printenv" NOTMUCH_CONFIG OUTPUT

test_begin_subtest "NOTMUCH_CONFIG is set"
notmuch printenv
cat <<EOF > EXPECTED
${NOTMUCH_CONFIG}
EOF
test_expect_equal_file_nonempty EXPECTED OUTPUT

test_begin_subtest "NOTMUCH_CONFIG is set by --config"
cp "${NOTMUCH_CONFIG}" "${NOTMUCH_CONFIG}.alternate"
cat <<EOF > EXPECTED
${NOTMUCH_CONFIG}.alternate
EOF
notmuch --config "${NOTMUCH_CONFIG}.alternate" printenv
test_expect_equal_file_nonempty EXPECTED OUTPUT

test_done
