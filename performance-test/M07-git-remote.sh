#!/usr/bin/env bash

test_description='git remote helper'

. $(dirname "$0")/perf-test-lib.sh || exit 1

mkdir repo
export GIT_DIR=`pwd`/repo
MAKE_EXPORT_PY=$NOTMUCH_SRCDIR/test/make-export.py

memory_start

echo "import refs/heads/master" > import.in

memory_run "import" "git-remote-notmuch origin notmuch:// >import.out <import.in"

python3 $MAKE_EXPORT_PY > export.in
memory_run "export" "git-remote-notmuch origin notmuch:// >export.out <export.in"

memory_done
