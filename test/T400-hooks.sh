#!/usr/bin/env bash
test_description='hooks'
. $(dirname "$0")/test-lib.sh || exit 1

test_require_external_prereq xapian-delve

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

create_write_hook () {
    local TOKEN="${RANDOM}"
    mkdir -p ${HOOK_DIR}
    cat <<EOF >"${HOOK_DIR}/${1}"
#!/bin/sh
if xapian-delve ${MAIL_DIR}/.notmuch/xapian | grep -q "writing = false"; then
   echo "${TOKEN}" > ${3}
fi
EOF
    chmod +x "${HOOK_DIR}/${1}"
    echo "${TOKEN}" > ${2}
}

create_change_hook () {
    mkdir -p ${HOOK_DIR}
    cat <<EOF >"${HOOK_DIR}/${1}"
#!/bin/sh
notmuch insert --no-hooks < ${2} > /dev/null
rm -f ${2}
EOF
    chmod +x "${HOOK_DIR}/${1}"
}

create_failing_hook () {
    local HOOK_DIR=${2}
    mkdir -p ${HOOK_DIR}
    cat <<EOF >"${HOOK_DIR}/${1}"
#!/bin/sh
exit 13
EOF
    chmod +x "${HOOK_DIR}/${1}"
}

# add a message to generate mail dir and database
add_message
# create maildir structure for notmuch-insert
mkdir -p "$MAIL_DIR"/{cur,new,tmp}

for config in traditional profile explicit relative XDG split; do
    unset NOTMUCH_PROFILE
    notmuch config set database.hook_dir
    notmuch config set database.path ${MAIL_DIR}
    case $config in
	traditional)
	    HOOK_DIR=${MAIL_DIR}/.notmuch/hooks
	    ;;
	profile)
	    dir=${HOME}/.config/notmuch/other
	    mkdir -p ${dir}
	    HOOK_DIR=${dir}/hooks
	    cp ${NOTMUCH_CONFIG} ${dir}/config
	    export NOTMUCH_PROFILE=other
	    ;;
	explicit)
	    HOOK_DIR=${HOME}/.notmuch-hooks
	    mkdir -p $HOOK_DIR
	    notmuch config set database.hook_dir $HOOK_DIR
	    ;;
	relative)
	    HOOK_DIR=${HOME}/.notmuch-hooks
	    mkdir -p $HOOK_DIR
	    notmuch config set database.hook_dir .notmuch-hooks
	    ;;
	XDG)
	    HOOK_DIR=${HOME}/.config/notmuch/default/hooks
	    ;;
	split)
	    dir="$TMP_DIRECTORY/database.$test_count"
	    notmuch config set database.path $dir
	    notmuch config set database.mail_root $MAIL_DIR
	    HOOK_DIR=${dir}/hooks
	    ;;
    esac

    test_begin_subtest "pre-new is run [${config}]"
    rm -rf ${HOOK_DIR}
    generate_message
    create_echo_hook "pre-new" expected output $HOOK_DIR
    notmuch new > /dev/null
    test_expect_equal_file expected output

    test_begin_subtest "post-new is run [${config}]"
    rm -rf ${HOOK_DIR}
    generate_message
    create_echo_hook "post-new" expected output $HOOK_DIR
    notmuch new > /dev/null
    test_expect_equal_file expected output

    test_begin_subtest "post-insert hook is run [${config}]"
    rm -rf ${HOOK_DIR}
    generate_message
    create_echo_hook "post-insert" expected output $HOOK_DIR
    notmuch insert < "$gen_msg_filename"
    test_expect_equal_file expected output

    test_begin_subtest "pre-new is run before post-new [${config}]"
    rm -rf ${HOOK_DIR}
    generate_message
    create_echo_hook "pre-new" pre-new.expected pre-new.output $HOOK_DIR
    create_echo_hook "post-new" post-new.expected post-new.output $HOOK_DIR
    notmuch new > /dev/null
    test_expect_equal_file post-new.expected post-new.output

    test_begin_subtest "pre-new non-zero exit status (hook status) [${config}]"
    rm -rf ${HOOK_DIR}
    generate_message
    create_failing_hook "pre-new" $HOOK_DIR
    output=`notmuch new 2>&1`
    test_expect_equal "$output" "Error: pre-new hook failed with status 13"

    # depends on the previous subtest leaving broken hook behind
    test_begin_subtest "pre-new non-zero exit status (notmuch status) [${config}]"
    test_expect_code 1 "notmuch new"

    # depends on the previous subtests leaving 1 new message behind
    test_begin_subtest "pre-new non-zero exit status aborts new [${config}]"
    rm -rf ${HOOK_DIR}
    output=$(NOTMUCH_NEW)
    test_expect_equal "$output" "Added 1 new message to the database."

    test_begin_subtest "post-new non-zero exit status (hook status) [${config}]"
    rm -rf ${HOOK_DIR}
    generate_message
    create_failing_hook "post-new" $HOOK_DIR
    NOTMUCH_NEW 2>output.stderr >output
    cat output.stderr >> output
    echo "Added 1 new message to the database." > expected
    echo "Error: post-new hook failed with status 13" >> expected
    test_expect_equal_file expected output

    # depends on the previous subtest leaving broken hook behind
    test_begin_subtest "post-new non-zero exit status (notmuch status) [${config}]"
    test_expect_code 1 "notmuch new"

    test_begin_subtest "post-insert hook does not affect insert status [${config}]"
    rm -rf ${HOOK_DIR}
    generate_message
    create_failing_hook "post-insert" $HOOK_DIR
    test_expect_success "notmuch insert < \"$gen_msg_filename\" > /dev/null"

    test_begin_subtest "hook without executable permissions [${config}]"
    rm -rf ${HOOK_DIR}
    mkdir -p ${HOOK_DIR}
    cat <<EOF >"${HOOK_DIR}/pre-new"
    #!/bin/sh
    echo foo
EOF
    output=`notmuch new 2>&1`
    test_expect_code 1 "notmuch new"

    test_begin_subtest "hook execution failure [${config}]"
    rm -rf ${HOOK_DIR}
    mkdir -p ${HOOK_DIR}
    cat <<EOF >"${HOOK_DIR}/pre-new"
    no hashbang, execl fails
EOF
    chmod +x "${HOOK_DIR}/pre-new"
    test_expect_code 1 "notmuch new"

    test_begin_subtest "post-new with write access [${config}]"
    rm -rf ${HOOK_DIR}
    create_write_hook "post-new" write.expected write.output $HOOK_DIR
    NOTMUCH_NEW
    test_expect_equal_file write.expected write.output

    test_begin_subtest "pre-new with write access [${config}]"
    rm -rf ${HOOK_DIR}
    create_write_hook "pre-new" write.expected write.output $HOOK_DIR
    NOTMUCH_NEW
    test_expect_equal_file write.expected write.output

    test_begin_subtest "add message in pre-new [${config}]"
    rm -rf ${HOOK_DIR}
    generate_message '[subject]="add msg in pre-new"'
    id1=$gen_msg_id
    create_change_hook "pre-new" $gen_msg_filename $HOOK_DIR
    generate_message '[subject]="add msg in new"'
    NOTMUCH_NEW
    notmuch search id:$id1 or id:$gen_msg_id | notmuch_search_sanitize > OUTPUT
    cat <<EOF | sed s'/^[ \t]*//' > EXPECTED
    thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; add msg in pre-new (inbox unread)
    thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; add msg in new (inbox unread)
EOF
    test_expect_equal_file EXPECTED OUTPUT

    rm -rf ${HOOK_DIR}
done
test_done
