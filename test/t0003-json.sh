#!/bin/bash
test_description="--format=json output"
. ./test-lib.sh

test_begin_subtest "Show message: json"
add_message "[subject]=\"json-show-subject\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[body]=\"json-show-message\""
output=$($NOTMUCH show --format=json "json-show-message")
test_expect_equal "$output" "[[[{\"id\": \"${gen_msg_id}\", \"match\": true, \"filename\": \"${gen_msg_filename}\", \"timestamp\": 946728000, \"date_relative\": \"2000-01-01\", \"tags\": [\"inbox\",\"unread\"], \"headers\": {\"Subject\": \"json-show-subject\", \"From\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"To\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"Cc\": \"\", \"Bcc\": \"\", \"Date\": \"Sat, 01 Jan 2000 12:00:00 -0000\"}, \"body\": [{\"id\": 1, \"content-type\": \"text/plain\", \"content\": \"json-show-message\n\"}]}, []]]]"

test_begin_subtest "Search message: json"
add_message "[subject]=\"json-search-subject\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[body]=\"json-search-message\""
output=$($NOTMUCH search --format=json "json-search-message" | notmuch_search_sanitize)
test_expect_equal "$output" "[{\"thread\": \"XXX\",
\"timestamp\": 946728000,
\"matched\": 1,
\"total\": 1,
\"authors\": \"Notmuch Test Suite\",
\"subject\": \"json-search-subject\",
\"tags\": [\"inbox\", \"unread\"]}]"

test_begin_subtest "Search by subject (utf-8):"
add_message [subject]=utf8-sübjéct "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\""
output=$($NOTMUCH search subject:utf8-sübjéct | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-sübjéct (inbox unread)"

test_begin_subtest "Show message: json, utf-8"
add_message "[subject]=\"json-show-utf8-body-sübjéct\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[body]=\"jsön-show-méssage\""
output=$($NOTMUCH show --format=json "jsön-show-méssage")
test_expect_equal "$output" "[[[{\"id\": \"${gen_msg_id}\", \"match\": true, \"filename\": \"${gen_msg_filename}\", \"timestamp\": 946728000, \"date_relative\": \"2000-01-01\", \"tags\": [\"inbox\",\"unread\"], \"headers\": {\"Subject\": \"json-show-utf8-body-sübjéct\", \"From\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"To\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"Cc\": \"\", \"Bcc\": \"\", \"Date\": \"Sat, 01 Jan 2000 12:00:00 -0000\"}, \"body\": [{\"id\": 1, \"content-type\": \"text/plain\", \"content\": \"jsön-show-méssage\n\"}]}, []]]]"

test_begin_subtest "Search message: json, utf-8"
add_message "[subject]=\"json-search-utf8-body-sübjéct\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[body]=\"jsön-search-méssage\""
output=$($NOTMUCH search --format=json "jsön-search-méssage" | notmuch_search_sanitize)
test_expect_equal "$output" "[{\"thread\": \"XXX\",
\"timestamp\": 946728000,
\"matched\": 1,
\"total\": 1,
\"authors\": \"Notmuch Test Suite\",
\"subject\": \"json-search-utf8-body-sübjéct\",
\"tags\": [\"inbox\", \"unread\"]}]"

test_done
