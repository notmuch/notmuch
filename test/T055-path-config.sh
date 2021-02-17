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

    restore_config
done

test_done
