#!/usr/bin/env bash

test_description='notmuch new'

. $(dirname "$0")/perf-test-lib.sh || exit 1

uncache_database

time_start

for i in $(seq 2 6); do
    time_run "notmuch new #$i" 'notmuch new'
done

manifest=$(mktemp manifestXXXXXX)

find mail -type f ! -path 'mail/.notmuch/*' | sed -n '1~4 p' > $manifest
# arithmetic context is to eat extra whitespace on e.g. some BSDs
count=$((`wc -l < $manifest`))

perl -nle 'rename $_, "$_.renamed"' $manifest

time_run "new ($count mv)" 'notmuch new'

perl -nle 'rename "$_.renamed", $_' $manifest

time_run "new ($count mv back)" 'notmuch new'

xargs tar cf backup.tar < $manifest

perl -nle 'unlink $_; unlink $_.copy' $manifest

time_run "new ($count rm)" 'notmuch new'

tar xf backup.tar

time_run "new ($count restore)" 'notmuch new'

perl -nle 'link $_, "$_.copy"' $manifest

time_run "new ($count cp)" 'notmuch new'

time_done
