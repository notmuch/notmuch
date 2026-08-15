#!/usr/bin/env bash

test_description='attachment content indexing'

. $(dirname "$0")/perf-test-lib.sh || exit 1

time_start

notmuch config set index.as_text ".*"

time_run 'reindex w/o filter' "notmuch reindex tag:attachment"
time_run 'reindex w/o filter' "notmuch reindex tag:attachment"

notmuch config set index.filter "/bin/true"

time_run 'reindex with filter' "notmuch reindex tag:attachment"

time_done
