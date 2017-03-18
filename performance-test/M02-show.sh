#!/bin/bash

test_description='show'

. ./perf-test-lib.sh || exit 1

memory_start

memory_run 'show *' "notmuch show '*' 1>/dev/null"
memory_run 'show --format=json *' "notmuch show --format=json '*' 1>/dev/null"
memory_run 'show --format=sexp *' "notmuch show --format=sexp '*' 1>/dev/null"

memory_done
