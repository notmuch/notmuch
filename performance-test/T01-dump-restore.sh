#!/bin/bash

test_description='dump and restore'

. ./perf-test-lib.sh || exit 1

time_start

time_run 'load nmbug tags' 'notmuch restore --accumulate < corpus.tags/nmbug.sup-dump'
time_run 'dump *' 'notmuch dump > tags.out'
time_run 'restore *' 'notmuch restore < tags.out'

time_done
