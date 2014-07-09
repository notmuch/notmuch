#!/usr/bin/env bash
test_description="threading when messages received out of order"
. ./test-lib.sh

# Generate all single-root four message thread structures.  We'll use
# this for multiple tests below.
THREADS=$(python ${TEST_DIRECTORY}/gen-threads.py 4)
nthreads=$(wc -l <<< "$THREADS")

test_begin_subtest "Messages with one parent get linked in all delivery orders"
# In the first variant, this delivers messages that reference only
# their immediate parent.  Hence, we should only expect threads to be
# fully joined at the end.
for ((n = 0; n < 4; n++)); do
    # Deliver the n'th message of every thread
    thread=0
    while read -a parents; do
        parent=${parents[$n]}
        generate_message \
            [id]=m$n@t$thread [in-reply-to]="\<m$parent@t$thread\>" \
            [subject]=p$thread [from]=m$n
        thread=$((thread + 1))
    done <<< "$THREADS"
    notmuch new > /dev/null
done
output=$(notmuch search --sort=newest-first '*' | notmuch_search_sanitize)
expected=$(for ((i = 0; i < $nthreads; i++)); do
        echo "thread:XXX   2001-01-05 [4/4] m3, m2, m1, m0; p$i (inbox unread)"
    done)
test_expect_equal "$output" "$expected"

test_begin_subtest "Messages with all parents get linked in all delivery orders"
test_subtest_known_broken
# Here we do the same thing as the previous test, but each message
# references all of its parents.  Since every message references the
# root of the thread, each thread should always be fully joined.  This
# is currently broken because of the bug detailed in
# id:8738h7kv2q.fsf@qmul.ac.uk.
rm ${MAIL_DIR}/*
notmuch new > /dev/null
output=""
expected=""
for ((n = 0; n < 4; n++)); do
    # Deliver the n'th message of every thread
    thread=0
    while read -a parents; do
        references=""
        parent=${parents[$n]}
        while [[ $parent != None ]]; do
            references="<m$parent@t$thread> $references"
            parent=${parents[$parent]}
        done

        generate_message \
            [id]=m$n@t$thread [references]="'$references'" \
            [subject]=p$thread [from]=m$n
        thread=$((thread + 1))
    done <<< "$THREADS"
    notmuch new > /dev/null

    output="$output
$(notmuch search --sort=newest-first '*' | notmuch_search_sanitize)"

    # Construct expected output
    template="thread:XXX   2001-01-05 [$((n+1))/$((n+1))]"
    for ((m = n; m > 0; m--)); do
        template="$template m$m,"
    done
    expected="$expected
$(for ((i = 0; i < $nthreads; i++)); do
        echo "$template m0; p$i (inbox unread)"
    done)"
done
test_expect_equal "$output" "$expected"

test_done
