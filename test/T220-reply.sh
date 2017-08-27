#!/usr/bin/env bash
test_description="\"notmuch reply\" in several variations"
. ./test-lib.sh || exit 1

test_begin_subtest "Basic reply"
add_message '[from]="Sender <sender@example.com>"' \
	     [to]=test_suite@notmuchmail.org \
	     [subject]=notmuch-reply-test \
	    '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
	    '[body]="basic reply test"'

output=$(notmuch reply id:${gen_msg_id} 2>&1 && echo OK)
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> basic reply test
OK"

test_begin_subtest "Multiple recipients"
add_message '[from]="Sender <sender@example.com>"' \
	    '[to]="test_suite@notmuchmail.org, Someone Else <someone@example.com>"' \
	     [subject]=notmuch-reply-test \
	    '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
	    '[body]="Multiple recipients"'

output=$(notmuch reply id:${gen_msg_id} 2>&1 && echo OK)
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>, Someone Else <someone@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> Multiple recipients
OK"

test_begin_subtest "Reply with CC"
add_message '[from]="Sender <sender@example.com>"' \
	     [to]=test_suite@notmuchmail.org \
	    '[cc]="Other Parties <cc@example.com>"' \
	     [subject]=notmuch-reply-test \
	    '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
	    '[body]="reply with CC"'

output=$(notmuch reply id:${gen_msg_id} 2>&1 && echo OK)
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>
Cc: Other Parties <cc@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> reply with CC
OK"

test_begin_subtest "Reply from alternate address"
add_message '[from]="Sender <sender@example.com>"' \
	     [to]=test_suite_other@notmuchmail.org \
	     [subject]=notmuch-reply-test \
	    '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
	    '[body]="reply from alternate address"'

output=$(notmuch reply id:${gen_msg_id} 2>&1 && echo OK)
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite_other@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> reply from alternate address
OK"

test_begin_subtest "Reply from address in named group list"
add_message '[from]="Sender <sender@example.com>"' \
            '[to]=group:test_suite@notmuchmail.org,someone@example.com\;' \
             [cc]=test_suite_other@notmuchmail.org \
             [subject]=notmuch-reply-test \
            '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
            '[body]="Reply from address in named group list"'

output=$(notmuch reply id:${gen_msg_id} 2>&1 && echo OK)
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>, someone@example.com
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> Reply from address in named group list
OK"

test_begin_subtest "Support for Reply-To"
add_message '[from]="Sender <sender@example.com>"' \
	     [to]=test_suite@notmuchmail.org \
	     [subject]=notmuch-reply-test \
	    '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
	    '[body]="support for reply-to"' \
	    '[reply-to]="Sender <elsewhere@example.com>"'

output=$(notmuch reply id:${gen_msg_id} 2>&1 && echo OK)
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <elsewhere@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> support for reply-to
OK"

test_begin_subtest "Un-munging Reply-To"
add_message '[from]="Sender <sender@example.com>"' \
	    '[to]="Some List <list@example.com>"' \
	     [subject]=notmuch-reply-test \
	    '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
	    '[body]="Un-munging Reply-To"' \
	    '[reply-to]="Evil Munging List <list@example.com>"'

output=$(notmuch reply id:${gen_msg_id} 2>&1 && echo OK)
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>, Some List <list@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> Un-munging Reply-To
OK"

test_begin_subtest "Un-munging Reply-To With Exact Match"
add_message '[from]="Sender <sender@example.com>"' \
	    '[to]="Some List <list@example.com>"' \
	     [subject]=notmuch-reply-test \
	    '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
	    '[body]="Un-munging Reply-To"' \
	    '[reply-to]="Some List <list@example.com>"'

output=$(notmuch reply id:${gen_msg_id} 2>&1 && echo OK)
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>, Some List <list@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> Un-munging Reply-To
OK"

test_begin_subtest "Un-munging Reply-To With Raw addr-spec"
add_message '[from]="Sender <sender@example.com>"' \
	    '[to]="Some List <list@example.com>"' \
	     [subject]=notmuch-reply-test \
	    '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
	    '[body]="Un-munging Reply-To"' \
	    '[reply-to]="list@example.com"'

output=$(notmuch reply id:${gen_msg_id} 2>&1 && echo OK)
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>, Some List <list@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> Un-munging Reply-To
OK"

test_begin_subtest "Message with header of exactly 200 bytes"
add_message '[subject]="This subject is exactly 200 bytes in length. Other than its length there is not much of note here. Note that the length of 200 bytes includes the Subject: and Re: prefixes with two spaces"' \
	    '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
	    '[body]="200-byte header"'
output=$(notmuch reply id:${gen_msg_id} 2>&1 && echo OK)
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: This subject is exactly 200 bytes in length. Other than its
 length there is not much of note here. Note that the length of 200 bytes
 includes the Subject: and Re: prefixes with two spaces
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Notmuch Test Suite <test_suite@notmuchmail.org> wrote:
> 200-byte header
OK"

test_begin_subtest "From guessing: Envelope-To"
add_message '[from]="Sender <sender@example.com>"' \
	    '[to]="Recipient <recipient@example.com>"' \
	    '[subject]="From guessing"' \
	    '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
	    '[body]="From guessing"' \
	    '[header]="Envelope-To: test_suite_other@notmuchmail.org"'

output=$(notmuch reply id:${gen_msg_id} 2>&1 && echo OK)
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite_other@notmuchmail.org>
Subject: Re: From guessing
To: Sender <sender@example.com>, Recipient <recipient@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> From guessing
OK"

test_begin_subtest "From guessing: X-Original-To"
add_message '[from]="Sender <sender@example.com>"' \
	    '[to]="Recipient <recipient@example.com>"' \
	    '[subject]="From guessing"' \
	    '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
	    '[body]="From guessing"' \
	    '[header]="X-Original-To: test_suite@otherdomain.org"'

output=$(notmuch reply id:${gen_msg_id} 2>&1 && echo OK)
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@otherdomain.org>
Subject: Re: From guessing
To: Sender <sender@example.com>, Recipient <recipient@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> From guessing
OK"

test_begin_subtest "From guessing: Delivered-To"
add_message '[from]="Sender <sender@example.com>"' \
	    '[to]="Recipient <recipient@example.com>"' \
	    '[subject]="From guessing"' \
	    '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
	    '[body]="From guessing"' \
	    '[header]="Delivered-To: test_suite_other@notmuchmail.org"'

output=$(notmuch reply id:${gen_msg_id} 2>&1 && echo OK)
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite_other@notmuchmail.org>
Subject: Re: From guessing
To: Sender <sender@example.com>, Recipient <recipient@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> From guessing
OK"

test_begin_subtest "Reply with RFC 2047-encoded headers"
add_message '[subject]="=?iso-8859-1?q?=e0=df=e7?="' \
	    '[from]="=?utf-8?q?=e2=98=83?= <snowman@example.com>"' \
	    '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
	    '[body]="Encoding"'

# GMime happens to change from Q- to B-encoding.  We canonicalize the
# case of the encoding and charset because different versions of GMime
# capitalize the encoding differently.
output=$( (notmuch reply id:${gen_msg_id} 2>&1 && echo OK) | perl -pe 's/=\?[^?]+\?[bB]\?/lc($&)/ge')
test_expect_equal "$output" "\
From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: =?iso-8859-1?b?4N/n?=
To: =?utf-8?b?4piD?= <snowman@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, ☃ <snowman@example.com> wrote:
> Encoding
OK"

test_begin_subtest "Reply with RFC 2047-encoded headers (JSON)"
output=$(echo '{"answer":' && notmuch reply --format=json id:${gen_msg_id} 2>&1 && echo ', "success": "OK"}')
test_expect_equal_json "$output" '
{  "answer": {
    "original": {
        "body": [
            {
                "content": "Encoding\n",
                "content-type": "text/plain",
                "id": 1
            }
        ],
        "date_relative": "2010-01-05",
        "excluded": false,
        "filename": ["'${MAIL_DIR}'/msg-014"],
        "headers": {
            "Date": "Tue, 05 Jan 2010 15:43:56 +0000",
            "From": "\u2603 <snowman@example.com>",
            "Subject": "\u00e0\u00df\u00e7",
            "To": "Notmuch Test Suite <test_suite@notmuchmail.org>"
        },
        "id": "'${gen_msg_id}'",
        "match": false,
        "tags": [
            "inbox",
            "unread"
        ],
        "timestamp": 1262706236
    },
    "reply-headers": {
        "From": "Notmuch Test Suite <test_suite@notmuchmail.org>",
        "In-reply-to": "<'${gen_msg_id}'>",
        "References": "<'${gen_msg_id}'>",
        "Subject": "Re: \u00e0\u00df\u00e7",
        "To": "\u2603 <snowman@example.com>"
    }
  },
  "success": "OK"
}'

test_begin_subtest "Reply to a message with multiple Cc headers"
add_email_corpus broken
output=$(notmuch reply id:multiple-cc@example.org 2>&1 && echo OK)
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: wowsers!
To: Alice <alice@example.org>, Daniel <daniel@example.org>
Cc: Bob <bob@example.org>, Charles <charles@example.org>
In-Reply-To: <multiple-cc@example.org>
References: <multiple-cc@example.org>

On Thu, 16 Jun 2016 22:14:41 -0400, Alice <alice@example.org> wrote:
> Note the Cc: and cc: headers.
OK"

test_done
