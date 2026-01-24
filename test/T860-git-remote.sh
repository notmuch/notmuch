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

test_done
