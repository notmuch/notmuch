#!/usr/bin/env bash
#
# Copyright (c) 2016 Daniel Kahn Gillmor
#

test_description='thread breakage during reindexing'

# notmuch uses ghost documents to track messages we have seen references
# to but have never seen.  Regardless of the order of delivery, message
# deletion, and reindexing, the list of ghost messages for a given
# stored corpus should not vary, so that threads can be reassmebled
# cleanly.
#
# In practice, we accept a small amount of variation (and therefore
# traffic pattern metadata leakage to be stored in the index) for the
# sake of efficiency.
#
# This test also embeds some subtests to ensure that indexing actually
# works properly and attempted fixes to threading issues do not break
# the expected contents of the index.

. ./test-lib.sh || exit 1

message_a() {
    mkdir -p ${MAIL_DIR}/cur
    cat > ${MAIL_DIR}/cur/a <<EOF
Subject: First message
Message-ID: <a@example.net>
From: Alice <alice@example.net>
To: Bob <bob@example.net>
Date: Thu, 31 Mar 2016 20:10:00 -0400

This is the first message in the thread.
Apple
EOF
}

message_b() {
    mkdir -p ${MAIL_DIR}/cur
    cat > ${MAIL_DIR}/cur/b <<EOF
Subject: Second message
Message-ID: <b@example.net>
In-Reply-To: <a@example.net>
References: <a@example.net>
From: Bob <bob@example.net>
To: Alice <alice@example.net>
Date: Thu, 31 Mar 2016 20:15:00 -0400

This is the second message in the thread.
Banana
EOF
}


test_content_count() {
    test_begin_subtest "${3:-looking for $2 instance of '$1'}"
    count=$(notmuch count --output=threads "$1")
    test_expect_equal "$count" "$2"
}

test_thread_count() {
    test_begin_subtest "${2:-Expecting $1 thread(s)}"
    count=$(notmuch count --output=threads)
    test_expect_equal "$count" "$1"
}

test_ghost_count() {
    test_begin_subtest "${2:-Expecting $1 ghosts(s)}"
    ghosts=$(../ghost-report ${MAIL_DIR}/.notmuch/xapian)
    test_expect_equal "$ghosts" "$1"
}

notmuch new >/dev/null

test_thread_count 0 'There should be no threads initially'
test_ghost_count 0 'There should be no ghosts initially'

message_a
notmuch new >/dev/null
test_thread_count 1 'One message in: one thread'
test_content_count apple 1
test_content_count banana 0
test_ghost_count 0

message_b
notmuch new >/dev/null
test_thread_count 1 'Second message in the same thread: one thread'
test_content_count apple 1
test_content_count banana 1
test_ghost_count 0

rm -f ${MAIL_DIR}/cur/a
notmuch new >/dev/null
test_thread_count 1 'First message removed: still only one thread'
test_content_count apple 0
test_content_count banana 1
test_ghost_count 1 'should be one ghost after first message removed'

message_a
notmuch new >/dev/null
test_thread_count 1 'First message reappears: should return to the same thread'
test_content_count apple 1
test_content_count banana 1
test_ghost_count 0

rm -f ${MAIL_DIR}/cur/b
notmuch new >/dev/null
test_thread_count 1 'Removing second message: still only one thread'
test_content_count apple 1
test_content_count banana 0
test_begin_subtest 'No ghosts should remain after deletion of second message'
# this is known to fail; we are leaking ghost messages deliberately
test_subtest_known_broken
ghosts=$(../ghost-report ${MAIL_DIR}/.notmuch/xapian)
test_expect_equal "$ghosts" "0"

rm -f ${MAIL_DIR}/cur/a
notmuch new >/dev/null
test_thread_count 0 'All messages gone: no threads'
test_content_count apple 0
test_content_count banana 0
test_ghost_count 0 'No ghosts should remain after full thread deletion'

test_done
