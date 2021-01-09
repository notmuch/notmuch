#!/usr/bin/env bash
test_description='Configuration of mail-root and database path'
. $(dirname "$0")/test-lib.sh || exit 1

backup_config () {
    local test_name=$(basename $0 .sh)
    cp ${NOTMUCH_CONFIG} notmuch-config-backup.${test_name}
}

restore_config () {
    local test_name=$(basename $0 .sh)
    export NOTMUCH_CONFIG="${TMP_DIRECTORY}/notmuch-config"
    unset CONFIG_PATH
    unset DATABASE_PATH
    unset NOTMUCH_PROFILE
    cp notmuch-config-backup.${test_name} ${NOTMUCH_CONFIG}
}

split_config () {
    local dir
    backup_config
    dir="$TMP_DIRECTORY/database.$test_count"
    rm -rf $dir
    mkdir $dir
    notmuch config set database.path $dir
    notmuch config set database.mail_root $MAIL_DIR
    DATABASE_PATH=$dir
}

symlink_config () {
    local dir
    backup_config
    dir="$TMP_DIRECTORY/link.$test_count"
    ln -s $MAIL_DIR $dir
    notmuch config set database.path $dir
    notmuch config set database.mail_root $MAIL_DIR
    unset DATABASE_PATH
}

for config in traditional split symlink; do
    # start each set of tests with a known set of messages
    add_email_corpus

    case $config in
	traditional)
	    backup_config
	    ;;
	split)
	    split_config
	    mv mail/.notmuch/xapian $DATABASE_PATH
	    ;;
	symlink)
	    symlink_config
	    ;;
    esac

    test_begin_subtest "count ($config)"
    output=$(notmuch count '*')
    test_expect_equal "$output" '52'

    test_begin_subtest "count+tag ($config)"
    tag="tag${RANDOM}"
    notmuch tag +$tag '*'
    output=$(notmuch count tag:$tag)
    notmuch tag -$tag '*'
    test_expect_equal "$output" '52'

    test_begin_subtest "address ($config)"
    notmuch address --deduplicate=no --sort=newest-first --output=sender --output=recipients path:foo >OUTPUT
    cat <<EOF >EXPECTED
Carl Worth <cworth@cworth.org>
notmuch@notmuchmail.org
EOF
    test_expect_equal_file EXPECTED OUTPUT

    test_begin_subtest "dump ($config)"
    notmuch dump is:attachment and is:signed | sort > OUTPUT
    cat <<EOF > EXPECTED
#notmuch-dump batch-tag:3 config,properties,tags
+attachment +inbox +signed +unread -- id:20091118005829.GB25380@dottiness.seas.harvard.edu
+attachment +inbox +signed +unread -- id:20091118010116.GC25380@dottiness.seas.harvard.edu
EOF
    test_expect_equal_file EXPECTED OUTPUT

    test_begin_subtest "dump + tag + restore ($config)"
    notmuch dump '*' > EXPECTED
    notmuch tag -inbox '*'
    notmuch restore < EXPECTED
    notmuch dump > OUTPUT
    test_expect_equal_file EXPECTED OUTPUT

    test_begin_subtest "reindex ($config)"
    notmuch search --output=messages '*' > EXPECTED
    notmuch reindex '*'
    notmuch search --output=messages '*' > OUTPUT
    test_expect_equal_file EXPECTED OUTPUT

    test_begin_subtest "use existing database ($config)"
    output=$(notmuch new)
    test_expect_equal "$output" 'No new mail.'

    test_begin_subtest "create database ($config)"
    rm -rf $DATABASE_PATH/{.notmuch,}/xapian
    notmuch new
    output=$(notmuch count '*')
    test_expect_equal "$output" '52'

    test_begin_subtest "detect new files ($config)"
    generate_message
    generate_message
    notmuch new
    output=$(notmuch count '*')
    test_expect_equal "$output" '54'

    test_begin_subtest "Show a raw message ($config)"
    add_message
    notmuch show --format=raw id:$gen_msg_id > OUTPUT
    test_expect_equal_file $gen_msg_filename OUTPUT
    rm -f $gen_msg_filename

    test_begin_subtest "reply ($config)"
    add_message '[from]="Sender <sender@example.com>"' \
		[to]=test_suite@notmuchmail.org \
		[subject]=notmuch-reply-test \
		'[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
		'[body]="basic reply test"'
    notmuch reply id:${gen_msg_id} 2>&1 > OUTPUT
    cat <<EOF > EXPECTED
From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> basic reply test
EOF
    test_expect_equal_file EXPECTED OUTPUT
    restore_config
done

test_done
