#!/usr/bin/env bash
test_description='"notmuch git" to save and restore tags'
. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus

git config --global user.email notmuch@example.org
git config --global user.name  "Notmuch Test Suite"

test_begin_subtest "init"
test_expect_success "notmuch git -p '' -C remote.git init"

test_begin_subtest "clone"
test_expect_success "notmuch git -p '' -C tags.git clone remote.git"

test_begin_subtest "commit"
notmuch git -C tags.git -p '' commit
git -C tags.git ls-tree -r --name-only HEAD | xargs dirname | sort -u | sed s,tags/,id:, > OUTPUT
notmuch search --output=messages '*' | sort > EXPECTED
test_expect_equal_file_nonempty EXPECTED OUTPUT

test_begin_subtest "checkout"
notmuch dump > BEFORE
notmuch tag -inbox '*'
notmuch git -C tags.git -p '' checkout
notmuch dump > AFTER
test_expect_equal_file_nonempty BEFORE AFTER

test_begin_subtest "archive"
notmuch git -C tags.git -p '' archive | tar tf - | \
    grep 20091117190054.GU3165@dottiness.seas.harvard.edu | sort > OUTPUT
cat <<EOF > EXPECTED
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/inbox
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/signed
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/unread
EOF
notmuch git -C tags.git -p '' checkout
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "status"
notmuch tag +test id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C tags.git -p '' status > OUTPUT
cat <<EOF > EXPECTED
A	20091117190054.GU3165@dottiness.seas.harvard.edu	test
EOF
notmuch git -C tags.git -p '' checkout
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "fetch"
notmuch tag +test2 id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C remote.git -p '' commit
notmuch tag -test2 id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C tags.git -p '' fetch
notmuch git -C tags.git -p '' status > OUTPUT
cat <<EOF > EXPECTED
 a	20091117190054.GU3165@dottiness.seas.harvard.edu	test2
EOF
notmuch git -C tags.git -p '' checkout
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "merge"
notmuch git -C tags.git -p '' merge
notmuch dump id:20091117190054.GU3165@dottiness.seas.harvard.edu | grep -v '^#' > OUTPUT
cat <<EOF > EXPECTED
+inbox +signed +test2 +unread -- id:20091117190054.GU3165@dottiness.seas.harvard.edu
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "push"
notmuch tag +test3 id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C tags.git -p '' commit
notmuch tag -test3 id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C tags.git -p '' push
notmuch git -C remote.git -p '' checkout
notmuch dump id:20091117190054.GU3165@dottiness.seas.harvard.edu | grep -v '^#' > OUTPUT
cat <<EOF > EXPECTED
+inbox +signed +test2 +test3 +unread -- id:20091117190054.GU3165@dottiness.seas.harvard.edu
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "environment passed through when run as 'notmuch git'"
env NOTMUCH_GIT_DIR=foo NOTMUCH_GIT_PREFIX=bar NOTMUCH_PROFILE=default notmuch git -C tags.git -p '' -ldebug status |& \
    grep '^env ' | notmuch_dir_sanitize > OUTPUT
cat <<EOF > EXPECTED
env NOTMUCH_GIT_DIR = foo
env NOTMUCH_GIT_PREFIX = bar
env NOTMUCH_PROFILE = default
env NOTMUCH_CONFIG = CWD/notmuch-config
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
