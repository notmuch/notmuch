#!/bin/bash

test_description='dump and restore'

. ./perf-test-lib.sh || exit 1

memory_start

memory_run 'load nmbug tags' 'notmuch restore --accumulate --input=corpus.tags/nmbug.sup-dump'
memory_run 'dump *' 'notmuch dump --output=tags.sup'
memory_run 'restore *' 'notmuch restore --input=tags.sup'
memory_run 'dump --format=batch-tag *' 'notmuch dump --format=batch-tag --output=tags.bt'
memory_run 'restore --format=batch-tag *' 'notmuch restore --format=batch-tag --input=tags.bt'

memory_done
