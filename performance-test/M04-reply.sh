#!/bin/bash

test_description='search'

. ./perf-test-lib.sh || exit 1

memory_start

for id in $(notmuch search --output=messages '*' | shuf -n 5); do
    memory_run "reply $id" "notmuch reply \"$id\" 1>/dev/null"
    memory_run "reply --format=json $id" "notmuch reply --format=json \"$id\" 1>/dev/null"
    memory_run "reply --format=sexp $id" "notmuch reply --format=sexp \"$id\" 1>/dev/null"
done

memory_done
