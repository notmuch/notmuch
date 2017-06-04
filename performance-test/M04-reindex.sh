#!/bin/bash

test_description='reindex'

. ./perf-test-lib.sh || exit 1

memory_start

memory_run 'reindex *' "notmuch reindex '*'"

memory_done
