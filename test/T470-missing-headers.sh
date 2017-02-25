#!/usr/bin/env bash
test_description='messages with missing headers'
. ./test-lib.sh || exit 1

# Notmuch requires at least one of from, subject, or to or it will
# ignore the file.  Generate two messages so that together they cover
# all possible missing headers.  We also give one of the messages a
# date to ensure stable result ordering.

cat <<EOF > "${MAIL_DIR}/msg-2"
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: Fri, 05 Jan 2001 15:43:57 +0000

Body
EOF

cat <<EOF > "${MAIL_DIR}/msg-1"
From: Notmuch Test Suite <test_suite@notmuchmail.org>

Body
EOF

NOTMUCH_NEW >/dev/null

test_begin_subtest "Search: text"
output=$(notmuch search '*' | notmuch_search_sanitize)
test_expect_equal "$output" "\
thread:XXX   2001-01-05 [1/1] (null);  (inbox unread)
thread:XXX   1970-01-01 [1/1] Notmuch Test Suite;  (inbox unread)"

test_begin_subtest "Search: json"
output=$(notmuch search --format=json '*' | notmuch_search_sanitize)
test_expect_equal_json "$output" '
[
    {
        "authors": "",
        "date_relative": "2001-01-05",
        "matched": 1,
        "subject": "",
        "tags": [
            "inbox",
            "unread"
        ],
        "thread": "XXX",
        "timestamp": 978709437,
        "total": 1,
        "query": ["id:notmuch-sha1-7a6e4eac383ef958fcd3ebf2143db71b8ff01161", null]
    },
    {
        "authors": "Notmuch Test Suite",
        "date_relative": "1970-01-01",
        "matched": 1,
        "subject": "",
        "tags": [
            "inbox",
            "unread"
        ],
        "thread": "XXX",
        "timestamp": 0,
        "total": 1,
        "query": ["id:notmuch-sha1-ca55943aff7a72baf2ab21fa74fab3d632401334", null]
    }
]'

test_begin_subtest "Show: text"
output=$(notmuch show '*' | notmuch_show_sanitize)
test_expect_equal "$output" "\
message{ id:notmuch-sha1-7a6e4eac383ef958fcd3ebf2143db71b8ff01161 depth:0 match:1 excluded:0 filename:/XXX/mail/msg-2
header{
 (2001-01-05) (inbox unread)
Subject: (null)
From: (null)
To: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: Fri, 05 Jan 2001 15:43:57 +0000
header}
body{
part{ ID: 1, Content-type: text/plain
Body
part}
body}
message}
message{ id:notmuch-sha1-ca55943aff7a72baf2ab21fa74fab3d632401334 depth:0 match:1 excluded:0 filename:/XXX/mail/msg-1
header{
Notmuch Test Suite <test_suite@notmuchmail.org> (1970-01-01) (inbox unread)
Subject: (null)
From: Notmuch Test Suite <test_suite@notmuchmail.org>
Date: Thu, 01 Jan 1970 00:00:00 +0000
header}
body{
part{ ID: 1, Content-type: text/plain
Body
part}
body}
message}"

test_begin_subtest "Show: json"
output=$(notmuch show --format=json '*' | notmuch_json_show_sanitize)
expected=$(notmuch_json_show_sanitize <<EOF
[
    [
        [
            {
                "body": [
                    {
                        "content": "Body\n",
                        "content-type": "text/plain",
                        "id": 1
                    }
                ],
                "date_relative": "2001-01-05",
                "excluded": false,
                "filename": ["YYYYY"],
                "headers": {
                    "Date": "Fri, 05 Jan 2001 15:43:57 +0000",
                    "From": "",
                    "Subject": "",
                    "To": "Notmuch Test Suite <test_suite@notmuchmail.org>"
                },
                "id": "XXXXX",
                "match": true,
                "tags": [
                    "inbox",
                    "unread"
                ],
                "timestamp": 978709437
            },
            []
        ]
    ],
    [
        [
            {
                "body": [
                    {
                        "content": "Body\n",
                        "content-type": "text/plain",
                        "id": 1
                    }
                ],
                "date_relative": "1970-01-01",
                "excluded": false,
                "filename": ["YYYYY"],
                "headers": {
                    "Date": "Thu, 01 Jan 1970 00:00:00 +0000",
                    "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
                    "Subject": ""
                },
                "id": "XXXXX",
                "match": true,
                "tags": [
                    "inbox",
                    "unread"
                ],
                "timestamp": 0
            },
            []
        ]
    ]
]
EOF
)
test_expect_equal_json "$output" "$expected"

test_done
