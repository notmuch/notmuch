#!/usr/bin/env bash
test_description="--format=text output"
. ./test-lib.sh || exit 1

test_begin_subtest "Show message: text"
add_message "[subject]=\"text-show-subject\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[body]=\"text-show-message\""
output=$(notmuch show --format=text "text-show-message" | notmuch_show_sanitize_all)
test_expect_equal "$output" "\
message{ id:XXXXX depth:0 match:1 excluded:0 filename:XXXXX
header{
Notmuch Test Suite <test_suite@notmuchmail.org> (2000-01-01) (inbox unread)
Subject: text-show-subject
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: Sat, 01 Jan 2000 12:00:00 +0000
header}
body{
part{ ID: 1, Content-type: text/plain
text-show-message
part}
body}
message}"

test_begin_subtest "Search message: text"
add_message "[subject]=\"text-search-subject\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[body]=\"text-search-message\""
output=$(notmuch search --format=text "text-search-message" | notmuch_search_sanitize)
test_expect_equal "$output" \
"thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; text-search-subject (inbox unread)"

test_begin_subtest "Show message: text, utf-8"
add_message "[subject]=\"text-show-utf8-body-sübjéct\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[body]=\"tëxt-show-méssage\""
output=$(notmuch show --format=text "tëxt-show-méssage" | notmuch_show_sanitize_all)
test_expect_equal "$output" "\
message{ id:XXXXX depth:0 match:1 excluded:0 filename:XXXXX
header{
Notmuch Test Suite <test_suite@notmuchmail.org> (2000-01-01) (inbox unread)
Subject: text-show-utf8-body-sübjéct
From: Notmuch Test Suite <test_suite@notmuchmail.org>
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: Sat, 01 Jan 2000 12:00:00 +0000
header}
body{
part{ ID: 1, Content-type: text/plain
tëxt-show-méssage
part}
body}
message}"

test_begin_subtest "Search message: text, utf-8"
add_message "[subject]=\"text-search-utf8-body-sübjéct\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[body]=\"tëxt-search-méssage\""
output=$(notmuch search --format=text "tëxt-search-méssage" | notmuch_search_sanitize)
test_expect_equal "$output" \
"thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; text-search-utf8-body-sübjéct (inbox unread)"

add_email_corpus

test_begin_subtest "Search message tags: text0"
cat <<EOF > EXPECTED
attachment inbox signed unread
EOF
notmuch search --format=text0 --output=tags '*' | xargs -0 | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

# Use tr(1) to convert --output=text0 to --output=text for
# comparison. Also translate newlines to spaces to fail with more
# noise if they are present as delimiters instead of null
# characters. This assumes there are no newlines in the data.
test_begin_subtest "Compare text vs. text0 for threads"
notmuch search --format=text --output=threads '*' | notmuch_search_sanitize > EXPECTED
notmuch search --format=text0 --output=threads '*' | tr "\n\0" " \n" | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Compare text vs. text0 for messages"
notmuch search --format=text --output=messages '*' | notmuch_search_sanitize > EXPECTED
notmuch search --format=text0 --output=messages '*' | tr "\n\0" " \n" | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Compare text vs. text0 for files"
notmuch search --format=text --output=files '*' | notmuch_search_sanitize > EXPECTED
notmuch search --format=text0 --output=files '*' | tr "\n\0" " \n" | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Compare text vs. text0 for tags"
notmuch search --format=text --output=tags '*' | notmuch_search_sanitize > EXPECTED
notmuch search --format=text0 --output=tags '*' | tr "\n\0" " \n" | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_done
