#!/bin/bash

test_description='thread subqueries'

. $(dirname "$0")/perf-test-lib.sh || exit 1

time_start

time_run "search thread:{} ..." "notmuch search thread:{date:2010} and thread:{from:linus}"
time_run "search thread:{} ..." "notmuch search thread:{date:2010} and thread:{from:linus}"
time_run "search thread:{} ..." "notmuch search thread:{date:2010} and thread:{from:linus}"

time_done
