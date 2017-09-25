#!/bin/bash

test_description='search'

. $(dirname "$0")/perf-test-lib.sh || exit 1

memory_start

mkdir -p "$MAIL_DIR"/{cur,new,tmp}

for count in {1..20}; do
    generate_message "[file]=\"insert-$count\"" "[dir]='tmp/'"
    memory_run "insert $count" "notmuch insert < $gen_msg_filename"
done

memory_done
