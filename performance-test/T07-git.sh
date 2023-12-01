#!/usr/bin/env bash

test_description='notmuch-git'

. $(dirname "$0")/perf-test-lib.sh || exit 1

time_start

time_run 'init' "notmuch git init"

time_run 'commit --force' "notmuch git commit --force"
time_run 'commit' "notmuch git -l error commit"
time_run 'commit' "notmuch git -l error commit"

time_run 'checkout' "notmuch git checkout"

time_run 'tag -inbox' "notmuch tag -inbox '*'"

time_run 'checkout --force' "notmuch git checkout --force"



time_done
