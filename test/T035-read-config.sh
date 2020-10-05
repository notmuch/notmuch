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

cat <<EOF > EXPECTED
Before:
#notmuch-dump batch-tag:3 tags

After:
#notmuch-dump batch-tag:3 tags
+attachment +inbox +signed +unread -- id:20091118005829.GB25380@dottiness.seas.harvard.edu
+attachment +inbox +signed +unread -- id:20091118010116.GC25380@dottiness.seas.harvard.edu
+inbox +signed +unread -- id:20091117190054.GU3165@dottiness.seas.harvard.edu
+inbox +signed +unread -- id:20091117203301.GV3165@dottiness.seas.harvard.edu
+inbox +signed +unread -- id:20091118002059.067214ed@hikari
+inbox +signed +unread -- id:20091118005040.GA25380@dottiness.seas.harvard.edu
+inbox +signed +unread -- id:87iqd9rn3l.fsf@vertex.dottedmag
EOF

test_begin_subtest "dump with saved query from config file"
backup_config
query_name="test${RANDOM}"
CONFIG_PATH=notmuch-config
printf "Before:\n" > OUTPUT
notmuch dump --include=tags query:$query_name | sort >> OUTPUT
printf "\nAfter:\n" >> OUTPUT
printf "\n[query]\n${query_name} = tag:signed\n" >> ${CONFIG_PATH}
notmuch dump --include=tags query:$query_name | sort >> OUTPUT
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "dump with saved query from config file (xdg)"
backup_config
query_name="test${RANDOM}"
xdg_config
printf "Before:\n" > OUTPUT
notmuch dump --include=tags query:$query_name | sort >> OUTPUT
printf "\nAfter:\n" >> OUTPUT
printf "\n[query]\n${query_name} = tag:signed\n" >> ${CONFIG_PATH}
notmuch dump --include=tags query:$query_name | sort >> OUTPUT
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "dump with saved query from config file (xdg+profile)"
backup_config
query_name="test${RANDOM}"
xdg_config work
printf "Before:\n" > OUTPUT
notmuch dump --include=tags query:$query_name | sort >> OUTPUT
printf "\nAfter:\n" >> OUTPUT
printf "\n[query]\n${query_name} = tag:signed\n" >> ${CONFIG_PATH}
notmuch dump --include=tags query:$query_name | sort >> OUTPUT
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "restore with xdg config"
backup_config
notmuch dump '*' > EXPECTED
notmuch tag -inbox '*'
xdg_config
notmuch restore --input=EXPECTED
notmuch dump > OUTPUT
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "restore with xdg+profile config"
backup_config
notmuch dump '*' > EXPECTED
notmuch tag -inbox '*'
xdg_config work
notmuch restore --input=EXPECTED
notmuch dump > OUTPUT
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_done
