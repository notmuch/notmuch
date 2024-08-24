#!/usr/bin/env bash

test_description='git remote helper'

. $(dirname "$0")/perf-test-lib.sh || exit 1

mkdir repo
export GIT_DIR=`pwd`/repo

memory_start

echo "import refs/heads/master" > import.in

memory_run "import" "git-remote-notmuch origin notmuch:// >import.out <import.in"

memory_done
