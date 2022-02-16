#!/usr/bin/env bash
test_description='Configuration of mail-root and database path'
. $(dirname "$0")/test-lib.sh || exit 1

test_require_external_prereq xapian-metdata

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
    unset XAPIAN_PATH
    unset MAILDIR
    rm -f "$HOME/mail"
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
    XAPIAN_PATH="$dir/xapian"
}

symlink_config () {
    local dir
    backup_config
    dir="$TMP_DIRECTORY/link.$test_count"
    ln -s $MAIL_DIR $dir
    notmuch config set database.path $dir
    notmuch config set database.mail_root $MAIL_DIR
    XAPIAN_PATH="$MAIL_DIR/.notmuch/xapian"
    unset DATABASE_PATH
}


home_mail_config () {
    local dir
    backup_config
    dir="${HOME}/mail"
    ln -s $MAIL_DIR $dir
    notmuch config set database.path
    notmuch config set database.mail_root
    XAPIAN_PATH="$MAIL_DIR/.notmuch/xapian"
    unset DATABASE_PATH
}

maildir_env_config () {
    local dir
    backup_config
    dir="${HOME}/env_points_here"
    ln -s $MAIL_DIR $dir
    export MAILDIR=$dir
    notmuch config set database.path
    notmuch config set database.mail_root
    XAPIAN_PATH="${MAIL_DIR}/.notmuch/xapian"
    unset DATABASE_PATH
}

xdg_config () {
    local dir
    local profile=${1:-default}

    if [[ $profile != default ]]; then
	export NOTMUCH_PROFILE=$profile
    fi

    backup_config
    DATABASE_PATH="${HOME}/.local/share/notmuch/${profile}"
    rm -rf $DATABASE_PATH
    mkdir -p $DATABASE_PATH

    config_dir="${HOME}/.config/notmuch/${profile}"
    mkdir -p ${config_dir}
    CONFIG_PATH=$config_dir/config
    mv ${NOTMUCH_CONFIG} $CONFIG_PATH
    unset NOTMUCH_CONFIG

    XAPIAN_PATH="${DATABASE_PATH}/xapian"
    notmuch --config=${CONFIG_PATH} config set database.mail_root ${TMP_DIRECTORY}/mail
    notmuch --config=${CONFIG_PATH} config set database.path
}

for config in traditional split XDG XDG+profile symlink home_mail maildir_env; do
    #start each set of tests with an known set of messages
    add_email_corpus

    case $config in
	traditional)
	    backup_config
	    XAPIAN_PATH="$MAIL_DIR/.notmuch/xapian"
	    ;;
	split)
	    split_config
	    mv mail/.notmuch/xapian $DATABASE_PATH
	    ;;
	XDG)
	    xdg_config
	    mv mail/.notmuch/xapian $DATABASE_PATH
	    ;;
	XDG+profile)
	    xdg_config ${RANDOM}
	    mv mail/.notmuch/xapian $DATABASE_PATH
	    ;;
	symlink)
	    symlink_config
	    ;;
	home_mail)
	    home_mail_config
	    ;;
	maildir_env)
	    maildir_env_config
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
    test_expect_equal_file_nonempty EXPECTED OUTPUT

    test_begin_subtest "reindex ($config)"
    notmuch search --output=messages '*' > EXPECTED
    notmuch reindex '*'
    notmuch search --output=messages '*' > OUTPUT
    test_expect_equal_file_nonempty EXPECTED OUTPUT

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
    test_expect_equal_file_nonempty $gen_msg_filename OUTPUT
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

    test_begin_subtest "insert+search ($config)"
    generate_message \
	"[subject]=\"insert-subject\"" \
	"[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" \
	"[body]=\"insert-message\""
    mkdir -p "$MAIL_DIR"/{cur,new,tmp}
    notmuch insert < "$gen_msg_filename"
    cur_msg_filename=$(notmuch search --output=files "subject:insert-subject")
    test_expect_equal_file_nonempty "$cur_msg_filename" "$gen_msg_filename"

    test_begin_subtest "compact+search ($config)"
    notmuch search --output=messages '*' | sort > EXPECTED
    notmuch compact
    notmuch search --output=messages '*' | sort > OUTPUT
    test_expect_equal_file_nonempty EXPECTED OUTPUT

    test_begin_subtest "upgrade backup ($config)"
    features=$(xapian-metadata get $XAPIAN_PATH features | grep -v "^relative directory paths")
    xapian-metadata set $XAPIAN_PATH features "$features"
    output=$(notmuch new | grep Welcome)
    test_expect_equal \
	"$output" \
	"Welcome to a new version of notmuch! Your database will now be upgraded."

    test_begin_subtest "notmuch +config -database suggests notmuch new ($config)"
    mv "$XAPIAN_PATH" "${XAPIAN_PATH}.bak"
    notmuch > OUTPUT
cat <<EOF > EXPECTED
Notmuch is configured, but no database was found.
You probably want to run "notmuch new" now to create a database.

Note that the first run of "notmuch new" can take a very long time
and that the resulting database will use roughly the same amount of
storage space as the email being indexed.

EOF
    mv "${XAPIAN_PATH}.bak" "$XAPIAN_PATH"

   test_expect_equal_file EXPECTED OUTPUT

   test_begin_subtest "Set config value ($config)"
   name=${RANDOM}
   value=${RANDOM}
   notmuch config set test${test_count}.${name} ${value}
   output=$(notmuch config get test${test_count}.${name})
   notmuch config set test${test_count}.${name}
   output2=$(notmuch config get test${test_count}.${name})
   test_expect_equal "${output}+${output2}" "${value}+"

   test_begin_subtest "Set config value in database ($config)"
   name=${RANDOM}
   value=${RANDOM}
   notmuch config set --database test${test_count}.${name} ${value}
   output=$(notmuch config get test${test_count}.${name})
   notmuch config set --database test${test_count}.${name}
   output2=$(notmuch config get test${test_count}.${name})
   test_expect_equal "${output}+${output2}" "${value}+"

   test_begin_subtest "Config list ($config)"
   notmuch config list | notmuch_config_sanitize | \
       sed -e "s/^database.backup_dir=.*$/database.backup_dir/"  \
	   -e "s/^database.hook_dir=.*$/database.hook_dir/" \
	   -e "s/^database.path=.*$/database.path/"  \
	   -e "s,^database.mail_root=CWD/home/mail,database.mail_root=MAIL_DIR," \
	   -e "s,^database.mail_root=CWD/home/env_points_here,database.mail_root=MAIL_DIR," \
	   > OUTPUT
   cat <<EOF > EXPECTED
built_with.compact=something
built_with.field_processor=something
built_with.retry_lock=something
built_with.sexp_queries=something
database.autocommit=8000
database.backup_dir
database.hook_dir
database.mail_root=MAIL_DIR
database.path
maildir.synchronize_flags=true
new.ignore=
new.tags=unread;inbox
search.exclude_tags=
user.name=Notmuch Test Suite
user.other_email=test_suite_other@notmuchmail.org;test_suite@otherdomain.org
user.primary_email=test_suite@notmuchmail.org
EOF
   test_expect_equal_file EXPECTED OUTPUT

   case $config in
       XDG*)
	   test_begin_subtest "Set shadowed config value in database ($config)"
	   name=${RANDOM}
	   value=${RANDOM}
	   key=test${test_count}.${name}
	   notmuch config set --database ${key}  ${value}
	   notmuch config set ${key} shadow${value}
	   output=$(notmuch --config='' config get ${key})
	   notmuch config set --database ${key}
	   output2=$(notmuch --config='' config get ${key})
	   notmuch config set ${key}
	   test_expect_equal "${output}+${output2}" "${value}+"
	   ;&
       split)
	   test_begin_subtest "'to' header does not crash (python-cffi) ($config)"
	   echo 'notmuch@notmuchmail.org' > EXPECTED
	   test_python <<EOF
from notmuch2 import Database
db=Database(config=Database.CONFIG.SEARCH)
m=db.find('20091117232137.GA7669@griffis1.net')
to=m.header('To')
print(to)
EOF
	   test_expect_equal_file EXPECTED OUTPUT
	   ;;
       *)
	   backup_database
	   test_begin_subtest ".notmuch without xapian/ handled gracefully ($config)"
	   rm -r $XAPIAN_PATH
	   test_expect_success "notmuch new"
	   restore_database
	   ;;
   esac

   case $config in
       split|XDG*)
   esac
   restore_config
   rm -rf home/.local
   rm -rf home/.config
done

test_done
