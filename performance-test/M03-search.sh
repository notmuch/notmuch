#!/bin/bash

test_description='search'

. ./perf-test-lib.sh || exit 1

memory_start

memory_run 'search *' "notmuch search '*' 1>/dev/null"
memory_run 'search --format=json *' "notmuch search --format=json '*' 1>/dev/null"
memory_run 'search --format=sexp *' "notmuch search --format=sexp '*' 1>/dev/null"

memory_done
