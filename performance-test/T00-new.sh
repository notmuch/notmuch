#!/bin/bash

test_description='notmuch new'

. ./perf-test-lib.sh || exit 1

uncache_database

time_start

for i in $(seq 2 6); do
    time_run "notmuch new #$i" 'notmuch new'
done

time_done
