#!/usr/bin/env bash

test_description='git-remote-notmuch'

. $(dirname "$0")/perf-test-lib.sh || exit 1

time_start

time_run 'clone --bare' "git clone --quiet --bare -b master notmuch::default default.git"
time_run 'clone' "git clone --quiet -b master notmuch:// repo"

time_done
