#!/usr/bin/env bash
test_description='"notmuch search, count and show" with excludes in several variations'
. ./test-lib.sh || exit 1

# Generates a thread consisting of a top level message and 'length'
# replies. The subject of the top message 'subject: top message"
# and the subject of the nth reply in the thread is "subject: reply n"
generate_thread ()
{
    local subject="$1"
    local length="$2"
    generate_message '[subject]="'"${subject}: top message"'"' '[body]="'"body of top message"'"'
    parent_id=$gen_msg_id
    gen_thread_msg_id[0]=$gen_msg_id
    for i in `seq 1 $length`
    do
	generate_message '[subject]="'"${subject}: reply $i"'"' \
	                 "[in-reply-to]=\<$parent_id\>" \
	                 '[body]="'"body of reply $i"'"'
	gen_thread_msg_id[$i]=$gen_msg_id
	parent_id=$gen_msg_id
    done
    notmuch new > /dev/null
    # We cannot retrieve the thread_id until after we have run notmuch new.
    gen_thread_id=`notmuch search --output=threads id:${gen_thread_msg_id[0]}`
}

#############################################
# These are the original search exclude tests.

test_begin_subtest "Search, exclude \"deleted\" messages from search"
notmuch config set search.exclude_tags deleted
generate_message '[subject]="Not deleted"'
not_deleted_id=$gen_msg_id
generate_message '[subject]="Deleted"'
notmuch new > /dev/null
notmuch tag +deleted id:$gen_msg_id
deleted_id=$gen_msg_id
output=$(notmuch search subject:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Not deleted (inbox unread)"

test_begin_subtest "Search, exclude \"deleted\" messages from message search"
output=$(notmuch search --output=messages subject:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "id:$not_deleted_id"

test_begin_subtest "Search, exclude \"deleted\" messages from message search --exclude=false"
output=$(notmuch search --exclude=false --output=messages subject:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "id:$not_deleted_id
id:$deleted_id"

test_begin_subtest "Search, exclude \"deleted\" messages from message search (non-existent exclude-tag)"
notmuch config set search.exclude_tags deleted non_existent_tag
output=$(notmuch search --output=messages subject:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "id:$not_deleted_id"
notmuch config set search.exclude_tags deleted

test_begin_subtest "Search, exclude \"deleted\" messages from search, overridden"
output=$(notmuch search subject:deleted and tag:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Deleted (deleted inbox unread)"

test_begin_subtest "Search, exclude \"deleted\" messages from threads"
add_message '[subject]="Not deleted reply"' '[in-reply-to]="<$gen_msg_id>"'
output=$(notmuch search subject:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Not deleted (inbox unread)
thread:XXX   2001-01-05 [1/2] Notmuch Test Suite; Not deleted reply (deleted inbox unread)"

test_begin_subtest "Search, don't exclude \"deleted\" messages when --exclude=flag specified"
output=$(notmuch search --exclude=flag subject:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Not deleted (inbox unread)
thread:XXX   2001-01-05 [1/2] Notmuch Test Suite; Deleted (deleted inbox unread)"

test_begin_subtest "Search, don't exclude \"deleted\" messages from search if not configured"
notmuch config set search.exclude_tags
output=$(notmuch search subject:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Not deleted (inbox unread)
thread:XXX   2001-01-05 [2/2] Notmuch Test Suite; Deleted (deleted inbox unread)"


########################################################
# We construct some threads for the tests. We use the tag "test" to
# indicate which messages we will search for.

# A thread of deleted messages; test matches one of them.
generate_thread "All messages excluded: single match" 5
notmuch tag +deleted $gen_thread_id
notmuch tag +test id:${gen_thread_msg_id[2]}

# A thread of deleted messages; test matches two of them.
generate_thread "All messages excluded: double match" 5
notmuch tag +deleted $gen_thread_id
notmuch tag +test id:${gen_thread_msg_id[2]}
notmuch tag +test id:${gen_thread_msg_id[4]}

# A thread some messages deleted; test only matches a deleted message.
generate_thread "Some messages excluded: single excluded match" 5
notmuch tag +deleted +test id:${gen_thread_msg_id[3]}

# A thread some messages deleted; test only matches a non-deleted message.
generate_thread "Some messages excluded: single non-excluded match" 5
notmuch tag +deleted id:${gen_thread_msg_id[2]}
notmuch tag +test id:${gen_thread_msg_id[4]}

# A thread no messages deleted; test matches a message.
generate_thread "No messages excluded: single match" 5
notmuch tag +test id:${gen_thread_msg_id[3]}

# Temporarily remove excludes to get list of matching messages
notmuch config set search.exclude_tags
matching_message_ids=( `notmuch search --output=messages tag:test` )
notmuch config set search.exclude_tags deleted

#########################################
# Notmuch search tests

test_begin_subtest "Search, default exclusion (thread summary)"
output=$(notmuch search tag:test | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; Some messages excluded: single non-excluded match: reply 4 (deleted inbox test unread)
thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; No messages excluded: single match: reply 3 (inbox test unread)"

test_begin_subtest "Search, default exclusion (messages)"
output=$(notmuch search --output=messages tag:test | notmuch_search_sanitize)
test_expect_equal "$output" "${matching_message_ids[4]}
${matching_message_ids[5]}"

test_begin_subtest "Search, exclude=true (thread summary)"
output=$(notmuch search --exclude=true tag:test | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; Some messages excluded: single non-excluded match: reply 4 (deleted inbox test unread)
thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; No messages excluded: single match: reply 3 (inbox test unread)"

test_begin_subtest "Search, exclude=true (messages)"
output=$(notmuch search --exclude=true --output=messages tag:test | notmuch_search_sanitize)
test_expect_equal "$output" "${matching_message_ids[4]}
${matching_message_ids[5]}"

test_begin_subtest "Search, exclude=false (thread summary)"
output=$(notmuch search --exclude=false tag:test | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; All messages excluded: single match: reply 2 (deleted inbox test unread)
thread:XXX   2001-01-05 [2/6] Notmuch Test Suite; All messages excluded: double match: reply 2 (deleted inbox test unread)
thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; Some messages excluded: single excluded match: reply 3 (deleted inbox test unread)
thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; Some messages excluded: single non-excluded match: reply 4 (deleted inbox test unread)
thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; No messages excluded: single match: reply 3 (inbox test unread)"

test_begin_subtest "Search, exclude=false (messages)"
output=$(notmuch search --exclude=false --output=messages tag:test | notmuch_search_sanitize)
test_expect_equal "$output" "${matching_message_ids[0]}
${matching_message_ids[1]}
${matching_message_ids[2]}
${matching_message_ids[3]}
${matching_message_ids[4]}
${matching_message_ids[5]}"

test_begin_subtest "Search, exclude=flag (thread summary)"
output=$(notmuch search --exclude=flag tag:test | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [0/6] Notmuch Test Suite; All messages excluded: single match: reply 2 (deleted inbox test unread)
thread:XXX   2001-01-05 [0/6] Notmuch Test Suite; All messages excluded: double match: reply 2 (deleted inbox test unread)
thread:XXX   2001-01-05 [0/6] Notmuch Test Suite; Some messages excluded: single excluded match: reply 3 (deleted inbox test unread)
thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; Some messages excluded: single non-excluded match: reply 4 (deleted inbox test unread)
thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; No messages excluded: single match: reply 3 (inbox test unread)"

test_begin_subtest "Search, exclude=flag (messages)"
output=$(notmuch search --exclude=flag --output=messages tag:test | notmuch_search_sanitize)
test_expect_equal "$output" "${matching_message_ids[0]}
${matching_message_ids[1]}
${matching_message_ids[2]}
${matching_message_ids[3]}
${matching_message_ids[4]}
${matching_message_ids[5]}"

test_begin_subtest "Search, exclude=all (thread summary)"
output=$(notmuch search --exclude=all tag:test | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/5] Notmuch Test Suite; Some messages excluded: single non-excluded match: reply 4 (inbox test unread)
thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; No messages excluded: single match: reply 3 (inbox test unread)"

test_begin_subtest "Search, exclude=all (messages)"
output=$(notmuch search --exclude=all --output=messages tag:test | notmuch_search_sanitize)
test_expect_equal "$output" "${matching_message_ids[4]}
${matching_message_ids[5]}"

test_begin_subtest "Search, default exclusion: tag in query (thread summary)"
output=$(notmuch search tag:test and tag:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; All messages excluded: single match: reply 2 (deleted inbox test unread)
thread:XXX   2001-01-05 [2/6] Notmuch Test Suite; All messages excluded: double match: reply 2 (deleted inbox test unread)
thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; Some messages excluded: single excluded match: reply 3 (deleted inbox test unread)"

test_begin_subtest "Search, default exclusion: tag in query (messages)"
output=$(notmuch search --output=messages tag:test and tag:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "${matching_message_ids[0]}
${matching_message_ids[1]}
${matching_message_ids[2]}
${matching_message_ids[3]}"

test_begin_subtest "Search, exclude=true: tag in query (thread summary)"
output=$(notmuch search --exclude=true tag:test and tag:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; All messages excluded: single match: reply 2 (deleted inbox test unread)
thread:XXX   2001-01-05 [2/6] Notmuch Test Suite; All messages excluded: double match: reply 2 (deleted inbox test unread)
thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; Some messages excluded: single excluded match: reply 3 (deleted inbox test unread)"

test_begin_subtest "Search, exclude=true: tag in query (messages)"
output=$(notmuch search --exclude=true --output=messages tag:test and tag:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "${matching_message_ids[0]}
${matching_message_ids[1]}
${matching_message_ids[2]}
${matching_message_ids[3]}"

test_begin_subtest "Search, exclude=false: tag in query (thread summary)"
output=$(notmuch search --exclude=false tag:test and tag:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; All messages excluded: single match: reply 2 (deleted inbox test unread)
thread:XXX   2001-01-05 [2/6] Notmuch Test Suite; All messages excluded: double match: reply 2 (deleted inbox test unread)
thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; Some messages excluded: single excluded match: reply 3 (deleted inbox test unread)"

test_begin_subtest "Search, exclude=false: tag in query (messages)"
output=$(notmuch search --exclude=false --output=messages tag:test and tag:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "${matching_message_ids[0]}
${matching_message_ids[1]}
${matching_message_ids[2]}
${matching_message_ids[3]}"

test_begin_subtest "Search, exclude=flag: tag in query (thread summary)"
output=$(notmuch search --exclude=flag tag:test and tag:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; All messages excluded: single match: reply 2 (deleted inbox test unread)
thread:XXX   2001-01-05 [2/6] Notmuch Test Suite; All messages excluded: double match: reply 2 (deleted inbox test unread)
thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; Some messages excluded: single excluded match: reply 3 (deleted inbox test unread)"

test_begin_subtest "Search, exclude=flag: tag in query (messages)"
output=$(notmuch search --exclude=flag --output=messages tag:test and tag:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "${matching_message_ids[0]}
${matching_message_ids[1]}
${matching_message_ids[2]}
${matching_message_ids[3]}"

test_begin_subtest "Search, exclude=all: tag in query (thread summary)"
output=$(notmuch search --exclude=all tag:test and tag:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; All messages excluded: single match: reply 2 (deleted inbox test unread)
thread:XXX   2001-01-05 [2/6] Notmuch Test Suite; All messages excluded: double match: reply 2 (deleted inbox test unread)
thread:XXX   2001-01-05 [1/6] Notmuch Test Suite; Some messages excluded: single excluded match: reply 3 (deleted inbox test unread)"

test_begin_subtest "Search, exclude=all: tag in query (messages)"
output=$(notmuch search --exclude=all --output=messages tag:test and tag:deleted | notmuch_search_sanitize)
test_expect_equal "$output" "${matching_message_ids[0]}
${matching_message_ids[1]}
${matching_message_ids[2]}
${matching_message_ids[3]}"

#########################################################
# Notmuch count tests

test_begin_subtest "Count, default exclusion (messages)"
output=$(notmuch count tag:test)
test_expect_equal "$output" "2"

test_begin_subtest "Count, default exclusion (threads)"
output=$(notmuch count --output=threads tag:test)
test_expect_equal "$output" "2"

test_begin_subtest "Count, exclude=true (messages)"
output=$(notmuch count --exclude=true tag:test)
test_expect_equal "$output" "2"

test_begin_subtest "Count, exclude=true (threads)"
output=$(notmuch count --output=threads --exclude=true tag:test)
test_expect_equal "$output" "2"

test_begin_subtest "Count, exclude=false (messages)"
output=$(notmuch count --exclude=false tag:test)
test_expect_equal "$output" "6"

test_begin_subtest "Count, exclude=false (threads)"
output=$(notmuch count --output=threads --exclude=false tag:test)
test_expect_equal "$output" "5"

test_begin_subtest "Count, default exclusion: tag in query (messages)"
output=$(notmuch count tag:test and tag:deleted)
test_expect_equal "$output" "4"

test_begin_subtest "Count, default exclusion: tag in query (threads)"
output=$(notmuch count --output=threads tag:test and tag:deleted)
test_expect_equal "$output" "3"

test_begin_subtest "Count, exclude=true: tag in query (messages)"
output=$(notmuch count --exclude=true tag:test and tag:deleted)
test_expect_equal "$output" "4"

test_begin_subtest "Count, exclude=true: tag in query (threads)"
output=$(notmuch count --output=threads --exclude=true tag:test and tag:deleted)
test_expect_equal "$output" "3"

test_begin_subtest "Count, exclude=false: tag in query (messages)"
output=$(notmuch count --exclude=false tag:test and tag:deleted)
test_expect_equal "$output" "4"

test_begin_subtest "Count, exclude=false: tag in query (threads)"
output=$(notmuch count --output=threads --exclude=false tag:test and tag:deleted)
test_expect_equal "$output" "3"

#############################################################
# Show tests

test_begin_subtest "Show, default exclusion"
output=$(notmuch show tag:test | notmuch_show_sanitize_all | egrep "Subject:|message{")
test_expect_equal "$output" "message{ id:XXXXX depth:0 match:1 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 4
message{ id:XXXXX depth:0 match:1 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 3"

test_begin_subtest "Show, default exclusion (entire-thread)"
output=$(notmuch show --entire-thread tag:test | notmuch_show_sanitize_all | egrep "Subject:|message{")
test_expect_equal "$output" "message{ id:XXXXX depth:0 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: top message
message{ id:XXXXX depth:1 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 1
message{ id:XXXXX depth:2 match:0 excluded:1 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 2
message{ id:XXXXX depth:3 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 3
message{ id:XXXXX depth:4 match:1 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 4
message{ id:XXXXX depth:5 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 5
message{ id:XXXXX depth:0 match:0 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: top message
message{ id:XXXXX depth:1 match:0 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 1
message{ id:XXXXX depth:2 match:0 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 2
message{ id:XXXXX depth:3 match:1 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 3
message{ id:XXXXX depth:4 match:0 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 4
message{ id:XXXXX depth:5 match:0 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 5"

test_begin_subtest "Show, exclude=true"
output=$(notmuch show --exclude=true tag:test | notmuch_show_sanitize_all | egrep "Subject:|message{")
test_expect_equal "$output" "message{ id:XXXXX depth:0 match:1 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 4
message{ id:XXXXX depth:0 match:1 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 3"

test_begin_subtest "Show, exclude=true (entire-thread)"
output=$(notmuch show --entire-thread --exclude=true tag:test | notmuch_show_sanitize_all | egrep "Subject:|message{")
test_expect_equal "$output" "message{ id:XXXXX depth:0 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: top message
message{ id:XXXXX depth:1 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 1
message{ id:XXXXX depth:2 match:0 excluded:1 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 2
message{ id:XXXXX depth:3 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 3
message{ id:XXXXX depth:4 match:1 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 4
message{ id:XXXXX depth:5 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 5
message{ id:XXXXX depth:0 match:0 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: top message
message{ id:XXXXX depth:1 match:0 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 1
message{ id:XXXXX depth:2 match:0 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 2
message{ id:XXXXX depth:3 match:1 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 3
message{ id:XXXXX depth:4 match:0 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 4
message{ id:XXXXX depth:5 match:0 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 5"

test_begin_subtest "Show, exclude=false"
output=$(notmuch show --exclude=false tag:test | notmuch_show_sanitize_all  | egrep "Subject:|message{")
test_expect_equal "$output" "message{ id:XXXXX depth:0 match:1 excluded:1 filename:XXXXX
Subject: All messages excluded: single match: reply 2
message{ id:XXXXX depth:0 match:1 excluded:1 filename:XXXXX
Subject: All messages excluded: double match: reply 2
message{ id:XXXXX depth:1 match:1 excluded:1 filename:XXXXX
Subject: All messages excluded: double match: reply 4
message{ id:XXXXX depth:0 match:1 excluded:1 filename:XXXXX
Subject: Some messages excluded: single excluded match: reply 3
message{ id:XXXXX depth:0 match:1 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 4
message{ id:XXXXX depth:0 match:1 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 3"

test_begin_subtest "Show, exclude=false (entire-thread)"
output=$(notmuch show --entire-thread --exclude=false tag:test | notmuch_show_sanitize_all | egrep "Subject:|message{")
test_expect_equal "$output" "message{ id:XXXXX depth:0 match:0 excluded:1 filename:XXXXX
Subject: All messages excluded: single match: top message
message{ id:XXXXX depth:1 match:0 excluded:1 filename:XXXXX
Subject: All messages excluded: single match: reply 1
message{ id:XXXXX depth:2 match:1 excluded:1 filename:XXXXX
Subject: All messages excluded: single match: reply 2
message{ id:XXXXX depth:3 match:0 excluded:1 filename:XXXXX
Subject: All messages excluded: single match: reply 3
message{ id:XXXXX depth:4 match:0 excluded:1 filename:XXXXX
Subject: All messages excluded: single match: reply 4
message{ id:XXXXX depth:5 match:0 excluded:1 filename:XXXXX
Subject: All messages excluded: single match: reply 5
message{ id:XXXXX depth:0 match:0 excluded:1 filename:XXXXX
Subject: All messages excluded: double match: top message
message{ id:XXXXX depth:1 match:0 excluded:1 filename:XXXXX
Subject: All messages excluded: double match: reply 1
message{ id:XXXXX depth:2 match:1 excluded:1 filename:XXXXX
Subject: All messages excluded: double match: reply 2
message{ id:XXXXX depth:3 match:0 excluded:1 filename:XXXXX
Subject: All messages excluded: double match: reply 3
message{ id:XXXXX depth:4 match:1 excluded:1 filename:XXXXX
Subject: All messages excluded: double match: reply 4
message{ id:XXXXX depth:5 match:0 excluded:1 filename:XXXXX
Subject: All messages excluded: double match: reply 5
message{ id:XXXXX depth:0 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single excluded match: top message
message{ id:XXXXX depth:1 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single excluded match: reply 1
message{ id:XXXXX depth:2 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single excluded match: reply 2
message{ id:XXXXX depth:3 match:1 excluded:1 filename:XXXXX
Subject: Some messages excluded: single excluded match: reply 3
message{ id:XXXXX depth:4 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single excluded match: reply 4
message{ id:XXXXX depth:5 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single excluded match: reply 5
message{ id:XXXXX depth:0 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: top message
message{ id:XXXXX depth:1 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 1
message{ id:XXXXX depth:2 match:0 excluded:1 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 2
message{ id:XXXXX depth:3 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 3
message{ id:XXXXX depth:4 match:1 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 4
message{ id:XXXXX depth:5 match:0 excluded:0 filename:XXXXX
Subject: Some messages excluded: single non-excluded match: reply 5
message{ id:XXXXX depth:0 match:0 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: top message
message{ id:XXXXX depth:1 match:0 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 1
message{ id:XXXXX depth:2 match:0 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 2
message{ id:XXXXX depth:3 match:1 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 3
message{ id:XXXXX depth:4 match:0 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 4
message{ id:XXXXX depth:5 match:0 excluded:0 filename:XXXXX
Subject: No messages excluded: single match: reply 5"


test_done
