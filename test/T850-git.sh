#!/usr/bin/env bash
test_description='"notmuch git" to save and restore tags'
. $(dirname "$0")/test-lib.sh || exit 1

if [ $NOTMUCH_HAVE_SFSEXP -ne 1 ]; then
    printf "Skipping due to missing sfsexp library\n"
    test_done
fi

# be very careful using backup_database / restore_database in this
# file, as they fool the cache invalidation checks in notmuch-git.

add_email_corpus

git config --global user.email notmuch@example.org
git config --global user.name  "Notmuch Test Suite"

test_begin_subtest "init"
test_expect_success "notmuch git -p '' -C remote.git init"

test_begin_subtest "init (git.path)"
notmuch config set git.path configured.git
notmuch git init
notmuch config set git.path
output=$(git -C configured.git rev-parse --is-bare-repository)
test_expect_equal "$output" "true"

test_begin_subtest "clone"
test_expect_success "notmuch git -p '' -C tags.git clone remote.git"

test_begin_subtest "initial commit needs force"
test_expect_code 1 "notmuch git -C tags.git commit"

test_begin_subtest "committing new prefix requires force"
notmuch git -C force-prefix.git init
notmuch tag +new-prefix::foo id:20091117190054.GU3165@dottiness.seas.harvard.edu
test_expect_code 1 "notmuch git -l debug -p 'new-prefix::' -C force-prefix.git commit"
notmuch tag -new-prefix::foo id:20091117190054.GU3165@dottiness.seas.harvard.edu

test_begin_subtest "committing new prefix works with force"
notmuch tag +new-prefix::foo id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -l debug -p 'new-prefix::' -C force-prefix.git commit --force
git -C force-prefix.git ls-tree -r --name-only HEAD |  notmuch_git_sanitize | xargs dirname | sort -u > OUTPUT
notmuch tag -new-prefix::foo id:20091117190054.GU3165@dottiness.seas.harvard.edu
cat <<EOF>EXPECTED
20091117190054.GU3165@dottiness.seas.harvard.edu
EOF
test_expect_equal_file_nonempty EXPECTED OUTPUT

test_begin_subtest "checkout new prefix requires force"
test_expect_code 1 "notmuch git -l debug -p 'new-prefix::' -C force-prefix.git checkout"

test_begin_subtest "checkout new prefix works with force"
notmuch dump > BEFORE
notmuch git -l debug -p 'new-prefix::' -C force-prefix.git checkout --force
notmuch dump --include=tags id:20091117190054.GU3165@dottiness.seas.harvard.edu | grep -v '^#' > OUTPUT
notmuch restore < BEFORE
cat <<EOF > EXPECTED
+inbox +new-prefix%3a%3afoo +signed +unread -- id:20091117190054.GU3165@dottiness.seas.harvard.edu
EOF
test_expect_equal_file_nonempty EXPECTED OUTPUT

test_begin_subtest "commit"
notmuch git -C tags.git commit --force
git -C tags.git ls-tree -r --name-only HEAD | notmuch_git_sanitize | xargs dirname | sort -u > OUTPUT
notmuch search --output=messages '*' | sed s/^id:// | sort > EXPECTED
test_expect_equal_file_nonempty EXPECTED OUTPUT

test_begin_subtest "commit --force succeeds"
notmuch git -C force.git init
test_expect_success "notmuch git -C force.git commit --force"

test_begin_subtest "changing git.safe_fraction succeeds"
notmuch config set git.safe_fraction 1
notmuch git -C force2.git init
test_expect_success "notmuch git -C force2.git commit"
notmuch config set git.safe_fraction

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
git -C tags.git ls-tree -r --name-only HEAD | notmuch_git_sanitize | \
    grep 20091117190054 | sort > OUTPUT
echo "--------------------------------------------------" >> OUTPUT
notmuch tag -test id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C tags.git commit
git -C tags.git ls-tree -r --name-only HEAD | notmuch_git_sanitize | \
    grep 20091117190054 | sort >> OUTPUT
cat <<EOF > EXPECTED
20091117190054.GU3165@dottiness.seas.harvard.edu/inbox
20091117190054.GU3165@dottiness.seas.harvard.edu/signed
20091117190054.GU3165@dottiness.seas.harvard.edu/test
20091117190054.GU3165@dottiness.seas.harvard.edu/unread
--------------------------------------------------
20091117190054.GU3165@dottiness.seas.harvard.edu/inbox
20091117190054.GU3165@dottiness.seas.harvard.edu/signed
20091117190054.GU3165@dottiness.seas.harvard.edu/unread
EOF
test_expect_equal_file_nonempty EXPECTED OUTPUT

test_begin_subtest "commit (change prefix)"
notmuch tag +test::one id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C tags.git -p 'test::' commit --force
git -C tags.git ls-tree -r --name-only HEAD |
    grep 20091117190054 | notmuch_git_sanitize | sort > OUTPUT
echo "--------------------------------------------------" >> OUTPUT
notmuch tag -test::one id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C tags.git commit --force
git -C tags.git ls-tree -r --name-only HEAD | notmuch_git_sanitize | \
    grep 20091117190054 | sort >> OUTPUT
cat <<EOF > EXPECTED
20091117190054.GU3165@dottiness.seas.harvard.edu/one
--------------------------------------------------
20091117190054.GU3165@dottiness.seas.harvard.edu/inbox
20091117190054.GU3165@dottiness.seas.harvard.edu/signed
20091117190054.GU3165@dottiness.seas.harvard.edu/unread
EOF
test_expect_equal_file_nonempty EXPECTED OUTPUT

backup_database
test_begin_subtest "large checkout needs --force"
notmuch tag -inbox '*'
test_expect_code 1 "notmuch git -C tags.git checkout"
restore_database

test_begin_subtest "checkout (git.safe_fraction)"
notmuch git -C force3.git clone tags.git
notmuch dump > BEFORE
notmuch tag -inbox '*'
notmuch config set git.safe_fraction 1
notmuch git -C force3.git checkout
notmuch config set git.safe_fraction
notmuch dump > AFTER
test_expect_equal_file_nonempty BEFORE AFTER

test_begin_subtest "checkout"
notmuch dump > BEFORE
notmuch tag -inbox '*'
notmuch git -C tags.git checkout --force
notmuch dump > AFTER
test_expect_equal_file_nonempty BEFORE AFTER

test_begin_subtest "archive"
notmuch git -C tags.git archive | tar tf - | \
    grep 20091117190054.GU3165@dottiness.seas.harvard.edu | notmuch_git_sanitize | sort > OUTPUT
cat <<EOF > EXPECTED
20091117190054.GU3165@dottiness.seas.harvard.edu/
20091117190054.GU3165@dottiness.seas.harvard.edu/inbox
20091117190054.GU3165@dottiness.seas.harvard.edu/signed
20091117190054.GU3165@dottiness.seas.harvard.edu/unread
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

test_begin_subtest "status (global config argument)"
cp notmuch-config notmuch-config.new
notmuch --config=notmuch-config.new config set git.path tags.git
notmuch tag +test id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch --config=./notmuch-config.new git status > OUTPUT
cat <<EOF > EXPECTED
A	20091117190054.GU3165@dottiness.seas.harvard.edu	test
EOF
notmuch --config=notmuch-config.new git checkout
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "fetch"
notmuch tag +test2 id:20091117190054.GU3165@dottiness.seas.harvard.edu
notmuch git -C remote.git commit --force
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


test_begin_subtest "env variable NOTMUCH_GIT_DIR overrides config when invoked as 'nmbug'"
notmuch config set git.path `pwd`/bar
NOTMUCH_GIT_DIR=`pwd`/remote.git  "$NOTMUCH_BUILDDIR"/nmbug -ldebug status |& grep '^repository' | notmuch_dir_sanitize > OUTPUT
notmuch config set git.path
cat <<EOF > EXPECTED
repository = CWD/remote.git
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "env variable NOTMUCH_GIT_DIR overrides config when invoked as 'notmuch git'"
notmuch config set git.path `pwd`/bar
NOTMUCH_GIT_DIR=`pwd`/remote.git notmuch git -ldebug status |& grep '^repository' | notmuch_dir_sanitize > OUTPUT
notmuch config set git.path
cat <<EOF > EXPECTED
repository = CWD/remote.git
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "env variable NOTMUCH_GIT_PREFIX works when invoked as 'nmbug'"
NOTMUCH_GIT_PREFIX=env:: "$NOTMUCH_BUILDDIR"/nmbug -ldebug status |& grep '^prefix' | notmuch_dir_sanitize > OUTPUT
cat <<EOF > EXPECTED
prefix = env::
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "env variable NOTMUCH_GIT_PREFIX works when invoked as nmbug"
NOTMUCH_GIT_PREFIX=foo:: "$NOTMUCH_BUILDDIR"/nmbug -ldebug status |& grep '^prefix' | notmuch_dir_sanitize > OUTPUT
cat <<EOF > EXPECTED
prefix = foo::
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "env variable NOTMUCH_GIT_PREFIX overrides config when invoked as 'nmbug'"
notmuch config set git.tag_prefix config::
NOTMUCH_GIT_PREFIX=env:: "$NOTMUCH_BUILDDIR"/nmbug -ldebug status |& grep '^prefix' | notmuch_dir_sanitize > OUTPUT
notmuch config set git.path
cat <<EOF > EXPECTED
prefix = env::
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "env variable NOTMUCH_GIT_PREFIX overrides config when invoked as 'notmuch git'"
notmuch config set git.tag_prefix config::
NOTMUCH_GIT_PREFIX=env:: notmuch git -ldebug status |& grep '^prefix' | notmuch_dir_sanitize > OUTPUT
notmuch config set git.path
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

test_begin_subtest "init, configured location"
repo=configured-tags
notmuch config set git.path `pwd`/$repo
notmuch git -ldebug init |& grep '^repository' | notmuch_dir_sanitize > OUTPUT
notmuch config set git.path
git -C $repo rev-parse --absolute-git-dir | notmuch_dir_sanitize >> OUTPUT
cat <<EOF > EXPECTED
repository = CWD/$repo
CWD/$repo
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "configured tag prefix"
notmuch config set git.tag_prefix test::
notmuch git -ldebug status |& grep '^prefix' > OUTPUT
notmuch config set git.tag_prefix
cat <<EOF > EXPECTED
prefix = test::
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "default version is 1"
notmuch git -l debug -C default-version.git init
output=$(git -C default-version.git cat-file blob HEAD:FORMAT)
test_expect_equal "${output}" 1

test_begin_subtest "illegal version"
test_expect_code 1 "notmuch git -l debug -C default-version.git init --format-version=42"

hash=("" "8d/c3/") # for use in synthetic repo contents.
for ver in {0..1}; do
    test_begin_subtest "init version=${ver}"
    notmuch git -C version-${ver}.git  -p "test${ver}::" init --format-version=${ver}
    output=$(git -C version-${ver}.git ls-tree -r --name-only HEAD)
    expected=("" "FORMAT")
    test_expect_equal "${output}" "${expected[${ver}]}"

    test_begin_subtest "initial commit version=${ver}"
    notmuch tag "+test${ver}::a" "+test${ver}::b" id:20091117190054.GU3165@dottiness.seas.harvard.edu
    notmuch git -C version-${ver}.git -p "test${ver}::" commit --force
    git -C version-${ver}.git ls-tree -r --name-only HEAD | grep -v FORMAT > OUTPUT
cat <<EOF > EXPECTED
tags/${hash[${ver}]}20091117190054.GU3165@dottiness.seas.harvard.edu/a
tags/${hash[${ver}]}20091117190054.GU3165@dottiness.seas.harvard.edu/b
EOF
    test_expect_equal_file_nonempty EXPECTED OUTPUT

    test_begin_subtest "second commit repo=${ver}"
    notmuch tag "+test${ver}::c" "+test${ver}::d" id:20091117190054.GU3165@dottiness.seas.harvard.edu
    notmuch git -C version-${ver}.git  -p "test${ver}::" commit --force
    git -C version-${ver}.git ls-tree -r --name-only HEAD | grep -v FORMAT > OUTPUT
cat <<EOF > EXPECTED
tags/${hash[$ver]}20091117190054.GU3165@dottiness.seas.harvard.edu/a
tags/${hash[$ver]}20091117190054.GU3165@dottiness.seas.harvard.edu/b
tags/${hash[$ver]}20091117190054.GU3165@dottiness.seas.harvard.edu/c
tags/${hash[$ver]}20091117190054.GU3165@dottiness.seas.harvard.edu/d
EOF
    test_expect_equal_file_nonempty EXPECTED OUTPUT

    test_begin_subtest "checkout repo=${ver} "
    notmuch dump > BEFORE
    notmuch tag -test::${ver}::a '*'
    notmuch git -C version-${ver}.git  -p "test${ver}::" checkout --force
    notmuch dump > AFTER
    test_expect_equal_file_nonempty BEFORE AFTER
done

test_done
