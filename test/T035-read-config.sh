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

test_begin_subtest "Insert message with custom new.tags (xdg)"
backup_config
xdg_config
tag=test${RANDOM}
notmuch --config=${CONFIG_PATH} config set new.tags $tag
generate_message \
    "[subject]=\"insert-subject\"" \
    "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" \
    "[body]=\"insert-message\""
mkdir -p ${MAIL_DIR}/{cur,new,tmp}
notmuch insert < "$gen_msg_filename"
notmuch dump id:$gen_msg_id > OUTPUT
cat <<EOF > EXPECTED
#notmuch-dump batch-tag:3 config,properties,tags
+$tag -- id:$gen_msg_id
EOF
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Insert message with custom new.tags (xdg+profile)"
backup_config
tag=test${RANDOM}
xdg_config $tag
notmuch --config=${CONFIG_PATH} config set new.tags $tag
generate_message \
    "[subject]=\"insert-subject\"" \
    "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" \
    "[body]=\"insert-message\""
mkdir -p ${MAIL_DIR}/{cur,new,tmp}
notmuch insert < "$gen_msg_filename"
notmuch dump id:$gen_msg_id > OUTPUT
cat <<EOF > EXPECTED
#notmuch-dump batch-tag:3 config,properties,tags
+$tag -- id:$gen_msg_id
EOF
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "reindex with saved query from config file"
backup_config
query_name="test${RANDOM}"
count1=$(notmuch count --lastmod '*' | cut -f3)
printf "\n[query]\n${query_name} = tag:inbox\n" >> notmuch-config
notmuch reindex query:$query_name
count2=$(notmuch count --lastmod '*' | cut -f3)
restore_config
test_expect_success "test '$count2 -gt $count1'"

test_begin_subtest "reindex with saved query from config file (xdg)"
query_name="test${RANDOM}"
count1=$(notmuch count --lastmod '*' | cut -f3)
xdg_config
printf "\n[query]\n${query_name} = tag:inbox\n" >> ${CONFIG_PATH}
notmuch reindex query:$query_name
count2=$(notmuch count --lastmod '*' | cut -f3)
restore_config
test_expect_success "test '$count2 -gt $count1'"

test_begin_subtest "reindex with saved query from config file (xdg+profile)"
query_name="test${RANDOM}"
count1=$(notmuch count --lastmod '*' | cut -f3)
xdg_config $query_name
printf "\n[query]\n${query_name} = tag:inbox\n" >> ${CONFIG_PATH}
notmuch reindex query:$query_name
count2=$(notmuch count --lastmod '*' | cut -f3)
restore_config
test_expect_success "test '$count2 -gt $count1'"



add_message '[from]="Sender <sender@example.com>"' \
	     [to]=test_suite@notmuchmail.org \
	    '[cc]="Other Parties <cc@example.com>"' \
	     [subject]=notmuch-reply-test \
	    '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
	    '[body]="reply with CC"'

cat <<EOF > EXPECTED
Before:
After:
From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>
Cc: Other Parties <cc@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> reply with CC
EOF

test_begin_subtest "reply with saved query from config file"
backup_config
query_name="test${RANDOM}"
printf "Before:\n" > OUTPUT
notmuch reply query:$query_name 2>&1 >> OUTPUT
printf "\n[query]\n${query_name} = id:${gen_msg_id}\n" >> notmuch-config
printf "After:\n" >> OUTPUT
notmuch reply query:$query_name >> OUTPUT
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "reply with saved query from config file (xdg)"
backup_config
query_name="test${RANDOM}"
xdg_config
printf "Before:\n" > OUTPUT
notmuch reply query:$query_name 2>&1 >> OUTPUT
printf "\n[query]\n${query_name} = id:${gen_msg_id}\n" >> ${CONFIG_PATH}
printf "After:\n" >> OUTPUT
notmuch reply query:$query_name >> OUTPUT
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "reply with saved query from config file (xdg+profile)"
backup_config
query_name="test${RANDOM}"
xdg_config $query_name
printf "Before:\n" > OUTPUT
notmuch reply query:$query_name 2>&1 >> OUTPUT
printf "\n[query]\n${query_name} = id:${gen_msg_id}\n" >> ${CONFIG_PATH}
printf "After:\n" >> OUTPUT
notmuch reply query:$query_name >> OUTPUT
restore_config
test_expect_equal_file EXPECTED OUTPUT

backup_database
test_begin_subtest "search with alternate config"
notmuch tag -- +foobar17 '*'
cp notmuch-config alt-config
notmuch --config=alt-config config set search.exclude_tags foobar17
output=$(notmuch --config=alt-config count '*')
test_expect_equal "$output" "0"
restore_database

cat <<EOF > EXPECTED
Before:
After:
thread:XXX   2009-11-18 [1/2] Carl Worth| Alex Botero-Lowry; [notmuch] [PATCH] Error out if no query is supplied to search instead of going into an infinite loop (attachment inbox unread)
thread:XXX   2009-11-18 [1/2] Carl Worth| Ingmar Vanhassel; [notmuch] [PATCH] Typsos (inbox unread)
thread:XXX   2009-11-18 [1/3] Carl Worth| Adrian Perez de Castro, Keith Packard; [notmuch] Introducing myself (inbox signed unread)
thread:XXX   2009-11-18 [1/3] Carl Worth| Israel Herraiz, Keith Packard; [notmuch] New to the list (inbox unread)
thread:XXX   2009-11-18 [1/3] Carl Worth| Jan Janak; [notmuch] What a great idea! (inbox unread)
thread:XXX   2009-11-18 [1/2] Carl Worth| Jan Janak; [notmuch] [PATCH] Older versions of install do not support -C. (inbox unread)
thread:XXX   2009-11-18 [1/3(4)] Carl Worth| Aron Griffis, Keith Packard; [notmuch] archive (inbox unread)
thread:XXX   2009-11-18 [1/2] Carl Worth| Keith Packard; [notmuch] [PATCH] Make notmuch-show 'X' (and 'x') commands remove inbox (and unread) tags (inbox unread)
thread:XXX   2009-11-18 [1/7] Carl Worth| Lars Kellogg-Stedman, Mikhail Gusarov, Keith Packard; [notmuch] Working with Maildir storage? (inbox signed unread)
thread:XXX   2009-11-18 [2/5] Carl Worth| Mikhail Gusarov, Keith Packard; [notmuch] [PATCH 1/2] Close message file after parsing message headers (inbox unread)
thread:XXX   2009-11-17 [1/2] Carl Worth| Alex Botero-Lowry; [notmuch] preliminary FreeBSD support (attachment inbox unread)
EOF

test_begin_subtest "search with saved query from config file"
query_name="test${RANDOM}"
backup_config
printf "Before:\n" > OUTPUT
notmuch search query:$query_name 2>&1 | notmuch_search_sanitize >> OUTPUT
printf "\n[query]\n${query_name} = from:cworth\n" >> notmuch-config
printf "After:\n" >> OUTPUT
notmuch search query:$query_name 2>&1 | notmuch_search_sanitize >> OUTPUT
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "search with saved query from config file (xdg)"
query_name="test${RANDOM}"
xdg_config
printf "Before:\n" > OUTPUT
notmuch search query:$query_name 2>&1 | notmuch_search_sanitize >> OUTPUT
printf "\n[query]\n${query_name} = from:cworth\n" >> ${CONFIG_PATH}
printf "After:\n" >> OUTPUT
notmuch search query:$query_name 2>&1 | notmuch_search_sanitize >> OUTPUT
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "search with saved query from config file (xdg + profile)"
query_name="test${RANDOM}"
xdg_config $query_name
printf "Before:\n" > OUTPUT
notmuch search query:$query_name 2>&1 | notmuch_search_sanitize >> OUTPUT
printf "\n[query]\n${query_name} = from:cworth\n" >> ${CONFIG_PATH}
printf "After:\n" >> OUTPUT
notmuch search query:$query_name 2>&1 | notmuch_search_sanitize >> OUTPUT
restore_config
test_expect_equal_file EXPECTED OUTPUT

cat <<EOF > EXPECTED
Before:
After:
Alex Botero-Lowry <alex.boterolowry@gmail.com>
Alexander Botero-Lowry <alex.boterolowry@gmail.com>
François Boulogne <boulogne.f@gmail.com>
Jjgod Jiang <gzjjgod@gmail.com>
EOF

test_begin_subtest "address: saved query from config file"
backup_config
query_name="test${RANDOM}"
printf "Before:\n" > OUTPUT
notmuch address --deduplicate=no --output=sender query:$query_name 2>&1 | sort >> OUTPUT
printf "\n[query]\n${query_name} = from:gmail.com\n" >> notmuch-config
printf "After:\n" >> OUTPUT
notmuch address --output=sender query:$query_name | sort >> OUTPUT
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "address: saved query from config file (xdg)"
query_name="test${RANDOM}"
xdg_config
printf "Before:\n" > OUTPUT
notmuch address --deduplicate=no --output=sender query:$query_name 2>&1 | sort >> OUTPUT
printf "\n[query]\n${query_name} = from:gmail.com\n" >> ${CONFIG_PATH}
printf "After:\n" >> OUTPUT
notmuch address --output=sender query:$query_name | sort >> OUTPUT
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "address: saved query from config file (xdg+profile)"
query_name="test${RANDOM}"
xdg_config $query_name
printf "Before:\n" > OUTPUT
notmuch address --deduplicate=no --output=sender query:$query_name 2>&1 | sort >> OUTPUT
printf "\n[query]\n${query_name} = from:gmail.com\n" >> ${CONFIG_PATH}
printf "After:\n" >> OUTPUT
notmuch address --output=sender query:$query_name | sort >> OUTPUT
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "show with alternate config"
backup_database
cp notmuch-config alt-config
notmuch --config=alt-config config set search.exclude_tags foobar17
notmuch tag -- +foobar17 '*'
output=$(notmuch --config=alt-config show '*' && echo OK)
restore_database
test_expect_equal "$output" "OK"

test_begin_subtest "show with alternate config (xdg)"
backup_database
notmuch tag -- +foobar17 '*'
xdg_config
notmuch --config=${CONFIG_PATH} config set search.exclude_tags foobar17
output=$(notmuch show '*' && echo OK)
restore_database
restore_config
test_expect_equal "$output" "OK"

test_begin_subtest "show with alternate config (xdg+profile)"
backup_database
notmuch tag -- +foobar17 '*'
xdg_config foobar17
notmuch --config=${CONFIG_PATH} config set search.exclude_tags foobar17
output=$(notmuch show '*' && echo OK)
restore_database
restore_config
test_expect_equal "$output" "OK"

# reset to known state
add_email_corpus

test_begin_subtest "tag with saved query from config file"
backup_config
query_name="test${RANDOM}"
tag_name="tag${RANDOM}"
notmuch count query:$query_name > OUTPUT
printf "\n[query]\n${query_name} = tag:inbox\n" >> notmuch-config
notmuch tag +$tag_name -- query:${query_name}
notmuch count tag:$tag_name >> OUTPUT
cat <<EOF > EXPECTED
0
52
EOF
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "tag with saved query from config file (xdg)"
xdg_config
query_name="test${RANDOM}"
tag_name="tag${RANDOM}"
notmuch count query:$query_name > OUTPUT
printf "\n[query]\n${query_name} = tag:inbox\n" >> ${CONFIG_PATH}
notmuch tag +$tag_name -- query:${query_name}
notmuch count tag:$tag_name >> OUTPUT
cat <<EOF > EXPECTED
0
52
EOF
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "tag with saved query from config file (xdg+profile)"
query_name="test${RANDOM}"
xdg_config ${query_name}
tag_name="tag${RANDOM}"
notmuch count query:$query_name > OUTPUT
printf "\n[query]\n${query_name} = tag:inbox\n" >> ${CONFIG_PATH}
notmuch tag +$tag_name -- query:${query_name}
notmuch count tag:$tag_name >> OUTPUT
cat <<EOF > EXPECTED
0
52
EOF
restore_config
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "running compact (xdg)"
xdg_config
notmuch compact
output=$(notmuch count '*')
restore_config
test_expect_equal "52" "$output"

test_begin_subtest "running compact (xdg + profile)"
xdg_config ${RANDOM}
notmuch compact
output=$(notmuch count '*')
restore_config
test_expect_equal "52" "$output"

test_begin_subtest "run notmuch-new (xdg)"
xdg_config
generate_message
output=$(NOTMUCH_NEW --debug)
restore_config
test_expect_equal "$output" "Added 1 new message to the database."

test_begin_subtest "run notmuch-new (xdg + profile)"
xdg_config ${RANDOM}
generate_message
output=$(NOTMUCH_NEW --debug)
restore_config
test_expect_equal "$output" "Added 1 new message to the database."

test_done
