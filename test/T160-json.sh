#!/usr/bin/env bash
test_description="--format=json output"
. ./test-lib.sh || exit 1

test_begin_subtest "Show message: json"
add_message "[subject]=\"json-show-subject\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[bcc]=\"test_suite+bcc@notmuchmail.org\"" "[reply-to]=\"test_suite+replyto@notmuchmail.org\"" "[body]=\"json-show-message\""
output=$(notmuch show --format=json "json-show-message")
test_expect_equal_json "$output" "[[[{\"id\": \"${gen_msg_id}\", \"match\": true, \"excluded\": false, \"filename\": [\"${gen_msg_filename}\"], \"timestamp\": 946728000, \"date_relative\": \"2000-01-01\", \"tags\": [\"inbox\",\"unread\"], \"headers\": {\"Subject\": \"json-show-subject\", \"From\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"To\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"Bcc\": \"test_suite+bcc@notmuchmail.org\", \"Reply-To\": \"test_suite+replyto@notmuchmail.org\", \"Date\": \"Sat, 01 Jan 2000 12:00:00 +0000\"}, \"body\": [{\"id\": 1, \"content-type\": \"text/plain\", \"content\": \"json-show-message\n\"}]}, []]]]"

# This should be the same output as above.
test_begin_subtest "Show message: json --body=true"
output=$(notmuch show --format=json --body=true "json-show-message")
test_expect_equal_json "$output" "[[[{\"id\": \"${gen_msg_id}\", \"match\": true, \"excluded\": false, \"filename\": [\"${gen_msg_filename}\"], \"timestamp\": 946728000, \"date_relative\": \"2000-01-01\", \"tags\": [\"inbox\",\"unread\"], \"headers\": {\"Subject\": \"json-show-subject\", \"From\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"To\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"Bcc\": \"test_suite+bcc@notmuchmail.org\", \"Reply-To\": \"test_suite+replyto@notmuchmail.org\", \"Date\": \"Sat, 01 Jan 2000 12:00:00 +0000\"}, \"body\": [{\"id\": 1, \"content-type\": \"text/plain\", \"content\": \"json-show-message\n\"}]}, []]]]"

test_begin_subtest "Show message: json --body=false"
output=$(notmuch show --format=json --body=false "json-show-message")
test_expect_equal_json "$output" "[[[{\"id\": \"${gen_msg_id}\", \"match\": true, \"excluded\": false, \"filename\": [\"${gen_msg_filename}\"], \"timestamp\": 946728000, \"date_relative\": \"2000-01-01\", \"tags\": [\"inbox\",\"unread\"], \"headers\": {\"Subject\": \"json-show-subject\", \"From\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"To\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"Bcc\": \"test_suite+bcc@notmuchmail.org\", \"Reply-To\": \"test_suite+replyto@notmuchmail.org\", \"Date\": \"Sat, 01 Jan 2000 12:00:00 +0000\"}}, []]]]"

test_begin_subtest "Search message: json"
add_message "[subject]=\"json-search-subject\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[body]=\"json-search-message\""
output=$(notmuch search --format=json "json-search-message" | notmuch_search_sanitize)
test_expect_equal_json "$output" "[{\"thread\": \"XXX\",
 \"timestamp\": 946728000,
 \"date_relative\": \"2000-01-01\",
 \"matched\": 1,
 \"total\": 1,
 \"authors\": \"Notmuch Test Suite\",
 \"subject\": \"json-search-subject\",
 \"query\": [\"id:$gen_msg_id\", null],
 \"tags\": [\"inbox\",
 \"unread\"]}]"

test_begin_subtest "Show message: json, utf-8"
add_message "[subject]=\"json-show-utf8-body-sübjéct\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[body]=\"jsön-show-méssage\""
output=$(notmuch show --format=json "jsön-show-méssage")
test_expect_equal_json "$output" "[[[{\"id\": \"${gen_msg_id}\", \"match\": true, \"excluded\": false, \"filename\": [\"${gen_msg_filename}\"], \"timestamp\": 946728000, \"date_relative\": \"2000-01-01\", \"tags\": [\"inbox\",\"unread\"], \"headers\": {\"Subject\": \"json-show-utf8-body-sübjéct\", \"From\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"To\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"Date\": \"Sat, 01 Jan 2000 12:00:00 +0000\"}, \"body\": [{\"id\": 1, \"content-type\": \"text/plain\", \"content\": \"jsön-show-méssage\n\"}]}, []]]]"

test_begin_subtest "Show message: json, inline attachment filename"
subject='json-show-inline-attachment-filename'
id="json-show-inline-attachment-filename@notmuchmail.org"
emacs_fcc_message \
    "$subject" \
    'This is a test message with inline attachment with a filename' \
    "(mml-attach-file \"$TEST_DIRECTORY/README\" nil nil \"inline\")
     (message-goto-eoh)
     (insert \"Message-ID: <$id>\n\")"
output=$(notmuch show --format=json "id:$id")
filename=$(notmuch search --output=files "id:$id")
# Get length of README after base64-encoding, minus additional newline.
attachment_length=$(( $(base64 $TEST_DIRECTORY/README | wc -c) - 1 ))
test_expect_equal_json "$output" "[[[{\"id\": \"$id\", \"match\": true, \"excluded\": false, \"filename\": [\"$filename\"], \"timestamp\": 946728000, \"date_relative\": \"2000-01-01\", \"tags\": [\"inbox\"], \"headers\": {\"Subject\": \"$subject\", \"From\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"To\": \"test_suite@notmuchmail.org\", \"Date\": \"Sat, 01 Jan 2000 12:00:00 +0000\"}, \"body\": [{\"id\": 1, \"content-type\": \"multipart/mixed\", \"content\": [{\"id\": 2, \"content-type\": \"text/plain\", \"content\": \"This is a test message with inline attachment with a filename\"}, {\"id\": 3, \"content-type\": \"application/octet-stream\", \"content-length\": $attachment_length, \"content-transfer-encoding\": \"base64\", \"content-disposition\": \"inline\", \"filename\": \"README\"}]}]}, []]]]"

test_begin_subtest "Search message: json, utf-8"
add_message "[subject]=\"json-search-utf8-body-sübjéct\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[body]=\"jsön-search-méssage\""
output=$(notmuch search --format=json "jsön-search-méssage" | notmuch_search_sanitize)
test_expect_equal_json "$output" "[{\"thread\": \"XXX\",
 \"timestamp\": 946728000,
 \"date_relative\": \"2000-01-01\",
 \"matched\": 1,
 \"total\": 1,
 \"authors\": \"Notmuch Test Suite\",
 \"subject\": \"json-search-utf8-body-sübjéct\",
 \"query\": [\"id:$gen_msg_id\", null],
 \"tags\": [\"inbox\",
 \"unread\"]}]"

test_begin_subtest "Format version: too low"
test_expect_code 20 "notmuch search --format-version=0 \\*"

test_begin_subtest "Format version: too high"
test_expect_code 21 "notmuch search --format-version=999 \\*"

test_begin_subtest "Show message: multiple filenames"
add_message "[id]=message-id@example.com [filename]=copy1"
add_message "[id]=message-id@example.com [filename]=copy2"
cat <<EOF > EXPECTED
[
    [
        [
            {
                "date_relative": "2001-01-05",
                "excluded": false,
                "filename": [
                    "${MAIL_DIR}/copy1",
                    "${MAIL_DIR}/copy2"
                ],
                "headers": {
                    "Date": "Fri, 05 Jan 2001 15:43:52 +0000",
                    "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
                    "Subject": "Show message: multiple filenames",
                    "To": "Notmuch Test Suite <test_suite@notmuchmail.org>"
                },
                "id": "message-id@example.com",
                "match": true,
                "tags": [
                    "inbox",
                    "unread"
                ],
                "timestamp": 978709432
            },
            []
        ]
    ]
]
EOF
output=$(notmuch show --format=json --body=false id:message-id@example.com)
test_expect_equal_json "$output" "$(cat EXPECTED)"

test_begin_subtest "Show message: multiple filenames, format version 2"
cat <<EOF > EXPECTED
[
    [
        [
            {
                "date_relative": "2001-01-05",
                "excluded": false,
                "filename": "${MAIL_DIR}/copy1",
                "headers": {
                    "Date": "Fri, 05 Jan 2001 15:43:52 +0000",
                    "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
                    "Subject": "Show message: multiple filenames",
                    "To": "Notmuch Test Suite <test_suite@notmuchmail.org>"
                },
                "id": "message-id@example.com",
                "match": true,
                "tags": [
                    "inbox",
                    "unread"
                ],
                "timestamp": 978709432
            },
            []
        ]
    ]
]
EOF
output=$(notmuch show --format=json --body=false --format-version=2 id:message-id@example.com)
test_expect_equal_json "$output" "$(cat EXPECTED)"

test_done
