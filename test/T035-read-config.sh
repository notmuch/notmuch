#!/usr/bin/env bash
test_description='Various options for reading configuration'
. $(dirname "$0")/test-lib.sh || exit 1

backup_config () {
    local test_name=$(basename $0 .sh)
    cp ${NOTMUCH_CONFIG} notmuch-config-backup.${test_name}
}

xdg_config () {
    local dir
    local profile=${1:-default}
    if [[ $profile != default ]]; then
	export NOTMUCH_PROFILE=$profile
    fi
    backup_config
    dir="${HOME}/.config/notmuch/${profile}"
    rm -rf $dir
    mkdir -p $dir
    CONFIG_PATH=$dir/config
    mv ${NOTMUCH_CONFIG} ${CONFIG_PATH}
    unset NOTMUCH_CONFIG
}

restore_config () {
    local test_name=$(basename $0 .sh)
    export NOTMUCH_CONFIG="${TMP_DIRECTORY}/notmuch-config"
    unset CONFIG_PATH
    unset NOTMUCH_PROFILE
    cp notmuch-config-backup.${test_name} ${NOTMUCH_CONFIG}
}

add_email_corpus

test_begin_subtest "count with saved query from config file"
backup_config
query_name="test${RANDOM}"
notmuch count query:$query_name > OUTPUT
printf "\n[query]\n${query_name} = tag:inbox\n" >> notmuch-config
notmuch count query:$query_name >> OUTPUT
cat <<EOF > EXPECTED
0
52
EOF
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "count with saved query from config file (xdg)"
query_name="test${RANDOM}"
xdg_config
notmuch count query:$query_name > OUTPUT
printf "\n[query]\n${query_name} = tag:inbox\n" >> ${CONFIG_PATH}
notmuch count query:$query_name >> OUTPUT
cat <<EOF > EXPECTED
0
52
EOF
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "count with saved query from config file (xdg+profile)"
query_name="test${RANDOM}"
xdg_config work
notmuch count query:$query_name > OUTPUT
printf "\n[query]\n${query_name} = tag:inbox\n" >> ${CONFIG_PATH}
notmuch count query:$query_name >> OUTPUT
cat <<EOF > EXPECTED
0
52
EOF
restore_config
test_expect_equal_file EXPECTED OUTPUT


test_done
