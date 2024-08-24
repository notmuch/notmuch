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
export GIT_REMOTE_NM_DEBUG="s"
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
test_begin_subtest "pull sees deletion"
notmuch tag -unread -- id:4EFC743A.3060609@april.org
git -C repo pull
cat<<EOF >EXPECTED
inbox
EOF
test_expect_equal_file EXPECTED repo/$TAG_FILE
restore_state

test_done
