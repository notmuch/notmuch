#!/bin/bash

test_description='notmuch new'

. ./perf-test-lib.sh || exit 1

# ensure initial 'notmuch new' is run by memory_start
uncache_database

memory_start

# run 'notmuch new' a second time, to test different code paths
memory_run "notmuch new" "notmuch new"

memory_done
