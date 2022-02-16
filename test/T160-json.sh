#!/usr/bin/env bash
test_description="--format=json output"
. $(dirname "$0")/test-lib.sh || exit 1
.  $NOTMUCH_SRCDIR/test/test-lib-emacs.sh || exit 1

test_begin_subtest "Show message: json"
add_message "[subject]=\"json-show-subject\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[bcc]=\"test_suite+bcc@notmuchmail.org\"" "[reply-to]=\"test_suite+replyto@notmuchmail.org\"" "[body]=\"json-show-message\""
output=$(notmuch show --format=json "json-show-message")
test_expect_equal_json "$output" "[[[{\"id\": \"${gen_msg_id}\", \"crypto\": {}, \"match\": true, \"excluded\": false, \"filename\": [\"${gen_msg_filename}\"], \"timestamp\": 946728000, \"date_relative\": \"2000-01-01\", \"tags\": [\"inbox\",\"unread\"], \"headers\": {\"Subject\": \"json-show-subject\", \"From\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"To\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"Bcc\": \"test_suite+bcc@notmuchmail.org\", \"Reply-To\": \"test_suite+replyto@notmuchmail.org\", \"Date\": \"Sat, 01 Jan 2000 12:00:00 +0000\"}, \"body\": [{\"id\": 1, \"content-type\": \"text/plain\", \"content\": \"json-show-message\n\"}]}, []]]]"

# This should be the same output as above.
test_begin_subtest "Show message: json --body=true"
output=$(notmuch show --format=json --body=true "json-show-message")
test_expect_equal_json "$output" "[[[{\"id\": \"${gen_msg_id}\",  \"crypto\": {}, \"match\": true, \"excluded\": false, \"filename\": [\"${gen_msg_filename}\"], \"timestamp\": 946728000, \"date_relative\": \"2000-01-01\", \"tags\": [\"inbox\",\"unread\"], \"headers\": {\"Subject\": \"json-show-subject\", \"From\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"To\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"Bcc\": \"test_suite+bcc@notmuchmail.org\", \"Reply-To\": \"test_suite+replyto@notmuchmail.org\", \"Date\": \"Sat, 01 Jan 2000 12:00:00 +0000\"}, \"body\": [{\"id\": 1, \"content-type\": \"text/plain\", \"content\": \"json-show-message\n\"}]}, []]]]"

test_begin_subtest "Show message: json --body=false"
output=$(notmuch show --format=json --body=false "json-show-message")
test_expect_equal_json "$output" "[[[{\"id\": \"${gen_msg_id}\",  \"crypto\": {}, \"match\": true, \"excluded\": false, \"filename\": [\"${gen_msg_filename}\"], \"timestamp\": 946728000, \"date_relative\": \"2000-01-01\", \"tags\": [\"inbox\",\"unread\"], \"headers\": {\"Subject\": \"json-show-subject\", \"From\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"To\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"Bcc\": \"test_suite+bcc@notmuchmail.org\", \"Reply-To\": \"test_suite+replyto@notmuchmail.org\", \"Date\": \"Sat, 01 Jan 2000 12:00:00 +0000\"}}, []]]]"

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
test_expect_equal_json "$output" "[[[{\"id\": \"${gen_msg_id}\",  \"crypto\": {}, \"match\": true, \"excluded\": false, \"filename\": [\"${gen_msg_filename}\"], \"timestamp\": 946728000, \"date_relative\": \"2000-01-01\", \"tags\": [\"inbox\",\"unread\"], \"headers\": {\"Subject\": \"json-show-utf8-body-sübjéct\", \"From\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"To\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"Date\": \"Sat, 01 Jan 2000 12:00:00 +0000\"}, \"body\": [{\"id\": 1, \"content-type\": \"text/plain\", \"content\": \"jsön-show-méssage\n\"}]}, []]]]"

test_begin_subtest "Show message: json, inline attachment filename"
subject='json-show-inline-attachment-filename'
id="json-show-inline-attachment-filename@notmuchmail.org"
emacs_fcc_message \
    "$subject" \
    'This is a test message with inline attachment with a filename' \
    "(mml-attach-file \"$NOTMUCH_SRCDIR/test/README\" nil nil \"inline\")
     (message-goto-eoh)
     (insert \"Message-ID: <$id>\n\")"
output=$(notmuch show --format=json "id:$id")
filename=$(notmuch search --output=files "id:$id")
# Get length of README after base64-encoding, minus additional newline.
attachment_length=$(( $(base64 $NOTMUCH_SRCDIR/test/README | wc -c) - 1 ))
test_expect_equal_json "$output" "[[[{\"id\": \"$id\",  \"crypto\": {}, \"match\": true, \"excluded\": false, \"filename\": [\"$filename\"], \"timestamp\": 946728000, \"date_relative\": \"2000-01-01\", \"tags\": [\"inbox\"], \"headers\": {\"Subject\": \"$subject\", \"From\": \"Notmuch Test Suite <test_suite@notmuchmail.org>\", \"To\": \"test_suite@notmuchmail.org\", \"Date\": \"Sat, 01 Jan 2000 12:00:00 +0000\"}, \"body\": [{\"id\": 1, \"content-type\": \"multipart/mixed\", \"content\": [{\"id\": 2, \"content-type\": \"text/plain\", \"content\": \"This is a test message with inline attachment with a filename\"}, {\"id\": 3, \"content-type\": \"application/octet-stream\", \"content-length\": $attachment_length, \"content-transfer-encoding\": \"base64\", \"content-disposition\": \"inline\", \"filename\": \"README\"}]}]}, []]]]"

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

test_begin_subtest "Search message: json, 64-bit timestamp"
if [ $NOTMUCH_HAVE_64BIT_TIME_T -ne 1 ]; then
    test_subtest_known_broken
fi
add_message "[subject]=\"json-search-64bit-timestamp-subject\"" "[date]=\"Tue, 01 Jan 2999 12:00:00 -0000\"" "[body]=\"json-search-64bit-timestamp-message\""
output=$(notmuch search --format=json "json-search-64bit-timestamp-message" | notmuch_search_sanitize)
test_expect_equal_json "$output" "[{\"thread\": \"XXX\",
 \"timestamp\": 32472187200,
 \"date_relative\": \"the future\",
 \"matched\": 1,
 \"total\": 1,
 \"authors\": \"Notmuch Test Suite\",
 \"subject\": \"json-search-64bit-timestamp-subject\",
 \"query\": [\"id:$gen_msg_id\", null],
 \"tags\": [\"inbox\",
 \"unread\"]}]"

test_begin_subtest "Format version: too low"
test_expect_code 20 "notmuch search --format-version=0 \\*"

test_begin_subtest "Format version: too high"
test_expect_code 21 "notmuch search --format-version=999 \\*"

test_begin_subtest "Show message: multiple filenames"
add_message '[id]=message-id@example.com [filename]=copy1 [date]="Fri, 05 Jan 2001 15:43:52 +0000"'
add_message '[id]=message-id@example.com [filename]=copy2 [date]="Fri, 05 Jan 2001 15:43:52 +0000"'
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
                "crypto": {},
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

test_begin_subtest "show extra headers"
add_message "[subject]=\"extra-headers\"" "[date]=\"Sat, 01 Jan 2000 12:00:00 -0000\"" "[in-reply-to]=\"<parent@notmuch-test-suite>\"" "[body]=\"extra-headers test\""\
	   "[header]=\"Received: from mail.example.com (mail.example.com [1.1.1.1])
	by mail.notmuchmail.org (some MTA) with ESMTP id 12345678
	for <test_suite_other@notmuchmail.org>; Sat, 10 Apr 2010 07:54:51 -0400 (EDT)\"" \

notmuch config set show.extra_headers "in-reply-to;received"
output=$(notmuch show --format=json --body=false id:${gen_msg_id} | notmuch_json_show_sanitize)
cat <<EOF > EXPECTED
[
    [
        [
            {
                "crypto": {},
                "date_relative": "2000-01-01",
                "excluded": false,
                "filename": [
                    "YYYYY"
                ],
                "headers": {
                    "Date": "Sat, 01 Jan 2000 12:00:00 +0000",
                    "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
                    "In-Reply-To": "<parent@notmuch-test-suite>",
                    "Received": "from mail.example.com (mail.example.com [1.1.1.1])\tby mail.notmuchmail.org (some MTA) with ESMTP id 12345678\tfor <test_suite_other@notmuchmail.org>; Sat, 10 Apr 2010 07:54:51 -0400 (EDT)",
                    "Subject": "extra-headers",
                    "To": "Notmuch Test Suite <test_suite@notmuchmail.org>"
                },
                "id": "XXXXX",
                "match": true,
                "tags": [
                    "inbox",
                    "unread"
                ],
                "timestamp": 946728000
            },
            []
        ]
    ]
]
EOF
test_expect_equal_json "${output}" "$(cat EXPECTED)"

test_done
