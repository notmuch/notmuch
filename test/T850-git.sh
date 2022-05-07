#!/usr/bin/env bash
test_description='"notmuch git" to save and restore tags'
. $(dirname "$0")/test-lib.sh || exit 1

if [ $NOTMUCH_HAVE_SFSEXP -ne 1 ]; then
    printf "Skipping due to missing sfsexp library\n"
    test_done
fi

add_email_corpus

git config --global user.email notmuch@example.org
git config --global user.name  "Notmuch Test Suite"

test_begin_subtest "init"
test_expect_success "notmuch git -p '' -C remote.git init"

test_begin_subtest "clone"
test_expect_success "notmuch git -p '' -C tags.git clone remote.git"

test_begin_subtest "commit"
notmuch git -C tags.git commit
git -C tags.git ls-tree -r --name-only HEAD | xargs dirname | sort -u | sed s,tags/,id:, > OUTPUT
notmuch search --output=messages '*' | sort > EXPECTED
test_expect_equal_file_nonempty EXPECTED OUTPUT

test_begin_subtest "commit, with quoted tag"
notmuch git -C clone2.git clone tags.git
git -C clone2.git ls-tree -r --name-only HEAD | grep /inbox > BEFORE
notmuch tag '+"quoted tag"' '*'
notmuch git -C clone2.git commit
notmuch tag '-"quoted tag"' '*'
git -C clone2.git ls-tree -r --name-only HEAD | grep /inbox > AFTER
test_expect_equal_file_nonempty BEFORE AFTER

test_begin_subtest "commit (incremental)"
notmuch tag +test id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C tags.git commit
git -C tags.git ls-tree -r --name-only HEAD |
    grep 20091117190054 | sort > OUTPUT
echo "--------------------------------------------------" >> OUTPUT
notmuch tag -test id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C tags.git commit
git -C tags.git ls-tree -r --name-only HEAD |
    grep 20091117190054 | sort >> OUTPUT
cat <<EOF > EXPECTED
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/inbox
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/signed
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/test
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/unread
--------------------------------------------------
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/inbox
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/signed
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/unread
EOF
test_expect_equal_file_nonempty EXPECTED OUTPUT

test_begin_subtest "commit (change prefix)"
notmuch tag +test::one id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C tags.git -p 'test::' commit
git -C tags.git ls-tree -r --name-only HEAD |
    grep 20091117190054 | sort > OUTPUT
echo "--------------------------------------------------" >> OUTPUT
notmuch tag -test::one id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C tags.git commit
git -C tags.git ls-tree -r --name-only HEAD |
    grep 20091117190054 | sort >> OUTPUT
cat <<EOF > EXPECTED
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/one
--------------------------------------------------
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/inbox
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/signed
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/unread
EOF
test_expect_equal_file_nonempty EXPECTED OUTPUT

test_begin_subtest "checkout"
notmuch dump > BEFORE
notmuch tag -inbox '*'
notmuch git -C tags.git checkout
notmuch dump > AFTER
test_expect_equal_file_nonempty BEFORE AFTER

test_begin_subtest "archive"
notmuch git -C tags.git archive | tar tf - | \
    grep 20091117190054.GU3165@dottiness.seas.harvard.edu | sort > OUTPUT
cat <<EOF > EXPECTED
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/inbox
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/signed
tags/20091117190054.GU3165@dottiness.seas.harvard.edu/unread
EOF
notmuch git -C tags.git checkout
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "status"
notmuch tag +test id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C tags.git status > OUTPUT
cat <<EOF > EXPECTED
A	20091117190054.GU3165@dottiness.seas.harvard.edu	test
EOF
notmuch git -C tags.git checkout
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "fetch"
notmuch tag +test2 id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C remote.git commit
notmuch tag -test2 id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C tags.git fetch
notmuch git -C tags.git status > OUTPUT
cat <<EOF > EXPECTED
 a	20091117190054.GU3165@dottiness.seas.harvard.edu	test2
EOF
notmuch git -C tags.git checkout
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "merge"
notmuch git -C tags.git merge
notmuch dump id:20091117190054.GU3165@dottiness.seas.harvard.edu | grep -v '^#' > OUTPUT
cat <<EOF > EXPECTED
+inbox +signed +test2 +unread -- id:20091117190054.GU3165@dottiness.seas.harvard.edu
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "push"
notmuch tag +test3 id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C tags.git commit
notmuch tag -test3 id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C tags.git push
notmuch git -C remote.git checkout
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

test_begin_subtest "--nmbug argument sets defaults"
notmuch git -ldebug --nmbug status |& grep '^\(prefix\|repository\)' | notmuch_dir_sanitize > OUTPUT
cat <<EOF > EXPECTED
prefix = notmuch::
repository = CWD/home/.nmbug
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "invoke as nmbug sets defaults"
"$NOTMUCH_BUILDDIR"/nmbug -ldebug status |& grep '^\(prefix\|repository\)' | notmuch_dir_sanitize > OUTPUT
cat <<EOF > EXPECTED
prefix = notmuch::
repository = CWD/home/.nmbug
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "env variable NOTMUCH_GIT_DIR works when invoked as nmbug"
NOTMUCH_GIT_DIR=`pwd`/foo "$NOTMUCH_BUILDDIR"/nmbug -ldebug status |& grep '^repository' | notmuch_dir_sanitize > OUTPUT
cat <<EOF > EXPECTED
repository = CWD/foo
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "env variable NOTMUCH_GIT_DIR works when invoked as 'notmuch git'"
NOTMUCH_GIT_DIR=`pwd`/remote.git notmuch git -ldebug status |& grep '^repository' | notmuch_dir_sanitize > OUTPUT
cat <<EOF > EXPECTED
repository = CWD/remote.git
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "env variable NOTMUCH_GIT_PREFIX works when invoked as nmbug"
NOTMUCH_GIT_PREFIX=foo:: "$NOTMUCH_BUILDDIR"/nmbug -ldebug status |& grep '^prefix' | notmuch_dir_sanitize > OUTPUT
cat <<EOF > EXPECTED
prefix = foo::
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "env variable NOTMUCH_GIT_PREFIX works when invoked as 'notmuch git'"
NOTMUCH_GIT_PREFIX=env:: notmuch git -ldebug status |& grep '^prefix' | notmuch_dir_sanitize > OUTPUT
cat <<EOF > EXPECTED
prefix = env::
EOF
test_expect_equal_file EXPECTED OUTPUT


test_begin_subtest "init, xdg default location"
repo=home/.local/share/notmuch/default/git
notmuch git -ldebug init |& grep '^repository' | notmuch_dir_sanitize > OUTPUT
git -C $repo rev-parse --absolute-git-dir | notmuch_dir_sanitize >> OUTPUT
cat <<EOF > EXPECTED
repository = CWD/$repo
CWD/$repo
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "init, xdg default location, with profile"
repo=home/.local/share/notmuch/work/git
NOTMUCH_PROFILE=work notmuch git -ldebug init |& grep '^repository' | notmuch_dir_sanitize > OUTPUT
git -C $repo rev-parse --absolute-git-dir | notmuch_dir_sanitize >> OUTPUT
cat <<EOF > EXPECTED
repository = CWD/$repo
CWD/$repo
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
