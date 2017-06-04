#!/bin/bash

test_description='tagging'

. ./perf-test-lib.sh || exit 1

time_start

time_run 'reindex *' "notmuch reindex '*'"
time_run 'reindex *' "notmuch reindex '*'"
time_run 'reindex *' "notmuch reindex '*'"

time_done
