#!/usr/bin/env bash

test_description='reindexing'

. $(dirname "$0")/perf-test-lib.sh || exit 1

time_start

time_run 'reindex *' "notmuch reindex '*'"
time_run 'reindex *' "notmuch reindex '*'"
time_run 'reindex *' "notmuch reindex '*'"

time_done
