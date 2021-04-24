#!/usr/bin/env bash

test_description='reindexing'

. $(dirname "$0")/perf-test-lib.sh || exit 1

time_start

time_run 'reindex *' "notmuch reindex '*'"
time_run 'reindex *' "notmuch reindex '*'"
time_run 'reindex *' "notmuch reindex '*'"

manifest=$(mktemp manifestXXXXXX)

find mail -type f ! -path 'mail/.notmuch/*' | sed -n '1~4 p' > $manifest
# arithmetic context is to eat extra whitespace on e.g. some BSDs
count=$((`wc -l < $manifest`))

xargs tar uf backup.tar < $manifest

perl -nle 'rename $_, "$_.renamed"' $manifest

time_run "reindex ($count mv)" "notmuch reindex '*'"

perl -nle 'rename "$_.renamed", $_' $manifest

time_run "reindex ($count mv back)" "notmuch reindex '*'"

perl -nle 'unlink $_; unlink $_.copy' $manifest

time_run "reindex ($count rm)" "notmuch reindex '*'"

tar xf backup.tar

time_run "reindex ($count restore)" "notmuch reindex '*'"

perl -nle 'link $_, "$_.copy"' $manifest

time_run "reindex ($count cp)" "notmuch reindex '*'"

time_done
