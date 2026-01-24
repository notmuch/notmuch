#!/usr/bin/env bash
test_description='git-remote-notmuch'
. $(dirname "$0")/test-lib.sh || exit 1

notmuch_sanitize_git() {
    sed 's/^committer \(.*\) \(<[^>]*>\) [1-9][0-9]* [-+][0-9]*/committer \1 \2 TIMESTAMP TIMEZONE/'
}

add_email_corpus

mkdir repo

git_tmp=$(mktemp -d gitXXXXXXXX)

run_helper () {
    env -u NOTMUCH_CONFIG GIT_DIR=${git_tmp} git-remote-notmuch dummy-alias "?config=${NOTMUCH_CONFIG}"
}

backup_state () {
    backup_database
    rm -rf repo.bak
    cp -a repo repo.bak
}

restore_state () {
    restore_database
    rm -rf repo
    mv repo.bak repo
}

export GIT_AUTHOR_NAME="Notmuch Test Suite"
export GIT_AUTHOR_EMAIL="notmuch@example.com"
export GIT_COMMITTER_NAME="Notmuch Test Suite"
export GIT_COMMITTER_EMAIL="notmuch@example.com"
export GIT_REMOTE_NM_DEBUG="sd"
export GIT_REMOTE_NM_LOG=grn-log.txt
EXPECTED=$NOTMUCH_SRCDIR/test/git-remote.expected-output
MAKE_EXPORT_PY=$NOTMUCH_SRCDIR/test/make-export.py

TAG_FILE="_notmuch_metadata/87/b1/4EFC743A.3060609@april.org/tags"

test_begin_subtest 'capabilities'
echo capabilities | run_helper > OUTPUT
cat <<EOF > EXPECTED
import
export
refspec refs/heads/*:refs/notmuch/*

EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'list'
echo list | run_helper > OUTPUT
cat <<EOF > EXPECTED
? refs/heads/master

EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'import writes lastmod file'
echo import | run_helper dummy-alias dummy-url > /dev/null
lastmod=$(notmuch count --lastmod '*' | cut -f2-)
test_expect_equal "${lastmod}" "$(cat < ${git_tmp}/notmuch/lastmod)"

# note that this test must not be the first time import is run,
# because it depends on the lastmod file
test_begin_subtest 'import produces expected output'
echo import | run_helper | notmuch_sanitize_git > OUTPUT
test_expect_equal_file $EXPECTED/default.import OUTPUT

test_begin_subtest "clone notmuch://"
test_expect_success "git clone notmuch:// $(mktemp -d cloneXXXXXX)"

test_begin_subtest "clone notmuch://?config=notmuch-config"
test_expect_success "git clone notmuch://?config=notmuch-config $(mktemp -d cloneXXXXXX)"

test_begin_subtest "clone notmuch://?profile=default"
test_expect_success "git clone notmuch://?profile=default $(mktemp -d cloneXXXXXX)"

test_begin_subtest "clone notmuch://?config=notmuch-config&profile=default"
test_expect_success "git clone notmuch://?config=notmuch-config\&profile=default $(mktemp -d cloneXXXXXX)"

test_begin_subtest 'clone notmuch://`pwd`/mail'
test_expect_success "env -u NOTMUCH_CONFIG git clone notmuch://`pwd`/mail $(mktemp -d cloneXXXXXX)"

test_begin_subtest 'clone notmuch://`pwd`/mail/?config=`pwd`/notmuch-config'
test_expect_success "env -u NOTMUCH_CONFIG git clone notmuch://`pwd`/mail?config=`pwd`/notmuch-config $(mktemp -d cloneXXXXXX)"

test_begin_subtest 'clone notmuch://.../mail/?config=.../notmuch-config&profile=default'
test_expect_success "env -u NOTMUCH_CONFIG git clone notmuch://`pwd`/mail/?config=`pwd`/notmuch-config\&profile=default $(mktemp -d clone XXX)"

test_begin_subtest 'clone notmuch://?path=.../mail/&config=.../notmuch-config&profile=default'
test_expect_success "env -u NOTMUCH_CONFIG git clone notmuch://?path=`pwd`/mail\&config=notmuch-config\&profile=default $(mktemp -d cloneXXXXXX)"

test_begin_subtest "clone notmuch::"
test_expect_success "git clone notmuch:: $(mktemp -d cloneXXXXXX)"

test_begin_subtest 'clone notmuch::`pwd`/mail'
test_expect_success "env -u NOTMUCH_CONFIG git clone notmuch::`pwd`/mail $(mktemp -d cloneXXXXXX)"

test_begin_subtest 'clone notmuch::`pwd`/mail?config=`pwd`/notmuch-config'
test_expect_success "env -u NOTMUCH_CONFIG git clone notmuch::`pwd`/mail?config=`pwd`/notmuch-config $(mktemp -d cloneXXXXXX)"

test_begin_subtest "clone has every message"
git clone notmuch:: repo
find repo -name tags -type f | sed -e s,repo/_notmuch_metadata/../../,id:, -e s,/tags$,, | sort > OUTPUT
notmuch search --output=messages '*' | sort > EXPECTED
test_expect_equal_file EXPECTED OUTPUT

backup_state
test_begin_subtest "pull get new tag"
notmuch tag +zznew -- id:4EFC743A.3060609@april.org
git -C repo pull
cat<<EOF >EXPECTED
inbox
unread
zznew
EOF
test_expect_equal_file EXPECTED repo/$TAG_FILE
restore_state

backup_state
test_begin_subtest "push empty commit"
git -C repo pull
notmuch dump | sort > EXPECTED
git -C repo pull
git -C repo push
notmuch dump | sort > OUTPUT
test_expect_equal_file EXPECTED OUTPUT
restore_state

backup_state
test_begin_subtest "pull sees deletion"
notmuch tag -unread -- id:4EFC743A.3060609@april.org
git -C repo pull
cat<<EOF >EXPECTED
inbox
EOF
test_expect_equal_file EXPECTED repo/$TAG_FILE
restore_state

backup_state
test_begin_subtest 'export runs'
run_helper <<EOF | notmuch_sanitize_git > OUTPUT
export
blob
mark :1
data 10
tag1
tag2

commit refs/heads/master
mark :2
author Notmuch Test Suite <notmuch@example.com> 1234 +0000
committer Notmuch Test Suite <notmuch@example.com> 1234 +0000
data 8
ignored
M 100644 :1 $TAG_FILE

done

EOF
cat <<EOF > EXPECTED
ok refs/heads/master

EOF
test_expect_equal_file EXPECTED OUTPUT

# this test depends on the previous one
test_begin_subtest 'export modifies database'
notmuch dump id:4EFC743A.3060609@april.org | tail -n 1 > OUTPUT
cat <<EOF > EXPECTED
+tag1 +tag2 -- id:4EFC743A.3060609@april.org
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_state

backup_state
test_begin_subtest 'restore via export'
notmuch dump > BEFORE
python3 $MAKE_EXPORT_PY > export.in
notmuch tag +transient -- id:4EFC743A.3060609@april.org
run_helper < export.in > OUTPUT
notmuch dump id:4EFC743A.3060609@april.org | tail -n 1 > OUTPUT
cat <<EOF > EXPECTED
+inbox +unread -- id:4EFC743A.3060609@april.org
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_state

backup_state
test_begin_subtest "push updates database"
cat<<EOF >repo/$TAG_FILE
tag1
tag2
EOF
git -C repo add $TAG_FILE
git -C repo commit -m 'testing push'
git -C repo push origin master
notmuch dump id:4EFC743A.3060609@april.org | tail -n 1 > OUTPUT
cat <<EOF > EXPECTED
+tag1 +tag2 -- id:4EFC743A.3060609@april.org
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_state

backup_state
test_begin_subtest "adding tag via repo"
cat<<EOF >repo/$TAG_FILE
tag1
tag2
tag3
EOF
git -C repo add $TAG_FILE
git -C repo commit -m 'testing push'
git -C repo push origin master
notmuch dump id:4EFC743A.3060609@april.org | tail -n 1 > OUTPUT
cat <<EOF > EXPECTED
+tag1 +tag2 +tag3 -- id:4EFC743A.3060609@april.org
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_state

backup_state
test_begin_subtest "non-prefixed file ignored on push"
cat<<EOF >repo/dummy
this is outside the notmuch metadata prefix
EOF
git -C repo add dummy
git -C repo commit -m 'testing prefix'
test_expect_code 0 "git -C repo push origin master"
restore_state

backup_state
test_begin_subtest "non-prefixed file ignored on pull"
cat<<EOF >repo/dummy
this is outside the notmuch metadata prefix
EOF
cp repo/dummy EXPECTED
git -C repo add dummy
git -C repo commit -m 'testing prefix'
git -C repo push origin master
git -C repo pull origin master
test_expect_equal_file EXPECTED repo/dummy
restore_state

backup_state
test_begin_subtest "push of non-main ref ignored"
notmuch dump > EXPECTED
git -C repo switch -c chaos
git -C repo rm -r _notmuch_metadata
git -C repo commit -m "delete all the things"
git -C repo push origin chaos:chaos
notmuch dump > OUTPUT
test_expect_equal_file EXPECTED OUTPUT
restore_state

backup_state
test_begin_subtest "removing all tags via repo"
cat<<EOF >repo/$TAG_FILE
EOF
git -C repo add $TAG_FILE
git -C repo commit -m 'testing push'
git -C repo push origin master
notmuch dump id:4EFC743A.3060609@april.org | tail -n 1 > OUTPUT
cat <<EOF > EXPECTED
 -- id:4EFC743A.3060609@april.org
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_state

backup_state
test_begin_subtest "removing message via repo"
parent=$(dirname $TAG_FILE)
# future proof this for when e.g. properties are stored
git -C repo rm -r $parent
git -C repo commit -m 'testing deletion'
git -C repo push origin master
notmuch dump id:4EFC743A.3060609@april.org | tail -n 1 > OUTPUT
cat <<EOF > EXPECTED
#notmuch-dump batch-tag:3 config,properties,tags
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_state

backup_state
test_begin_subtest "not removing later messages"
add_message '[subject]="first new message"'
git -C repo pull
add_message '[subject]="second new message"'
git -C repo pull
notmuch dump | sort > EXPECTED
git clone repo cloned_repo
rm -rf ${MAIL_DIR}/.notmuch
notmuch new --full-scan
git -C cloned_repo remote add database notmuch::
notmuch config set git.fail_on_missing false
git -C cloned_repo push database master
notmuch config set git.fail_on_missing true
notmuch dump | sort > OUTPUT
test_expect_equal_file_nonempty EXPECTED OUTPUT
restore_state

backup_state
test_begin_subtest 'by default, missing messages are an error during export'
test_subtest_known_broken
sed s/4EFC743A.3060609@april.org/missing-message@example.com/ < export.in > missing.in
test_expect_code 1 "run_helper < missing.in"
restore_state

backup_state
test_begin_subtest 'when configured, missing messages are ignored'
notmuch config set git.fail_on_missing false
test_expect_code 0 "run_helper < missing.in"
notmuch config set git.fail_on_missing true
restore_state

test_done
