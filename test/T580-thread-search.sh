#!/usr/bin/env bash
#
# Copyright (c) 2015 David Bremner
#

test_description='test of searching by thread-id'

. ./test-lib.sh || exit 1

add_email_corpus

test_begin_subtest "Every message is found in exactly one thread"

count=0
success=0
for id in $(notmuch search --output=messages '*'); do
    count=$((count +1))
    matches=$((`notmuch search --output=threads "$id" | wc -l`))
    if [ "$matches" = 1 ]; then
	success=$((success + 1))
    fi
done

test_expect_equal "$count" "$success"

test_begin_subtest "roundtripping message-ids via thread-ids"

count=0
success=0
for id in $(notmuch search --output=messages '*'); do
    count=$((count +1))
    thread=$(notmuch search --output=threads "$id")
    matched=$(notmuch search --output=messages "$thread" | grep "$id")
    if [ "$matched" = "$id" ]; then
	success=$((success + 1))
    fi
done

test_expect_equal "$count" "$success"


test_done
