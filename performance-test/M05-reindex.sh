#!/usr/bin/env bash

test_description='reindex'

. $(dirname "$0")/perf-test-lib.sh || exit 1

memory_start

memory_run 'reindex *' "notmuch reindex '*'"

memory_done
