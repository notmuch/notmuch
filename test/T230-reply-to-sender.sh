#!/usr/bin/env bash
test_description="\"notmuch reply --reply-to=sender\" in several variations"
. ./test-lib.sh || exit 1

test_begin_subtest "Basic reply-to-sender"
add_message '[from]="Sender <sender@example.com>"' \
             [to]=test_suite@notmuchmail.org \
             [subject]=notmuch-reply-test \
            '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
            '[body]="basic reply-to-sender test"'

output=$(notmuch reply --reply-to=sender id:${gen_msg_id})
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> basic reply-to-sender test"

test_begin_subtest "From Us, Basic reply to message"
add_message '[from]="Notmuch Test Suite <test_suite@notmuchmail.org>"' \
            '[to]="Recipient <recipient@example.com>"' \
             [subject]=notmuch-reply-test \
            '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
            '[body]="basic reply-to-from-us test"'

output=$(notmuch reply --reply-to=sender id:${gen_msg_id})
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Recipient <recipient@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Notmuch Test Suite <test_suite@notmuchmail.org> wrote:
> basic reply-to-from-us test"

test_begin_subtest "Multiple recipients"
add_message '[from]="Sender <sender@example.com>"' \
            '[to]="test_suite@notmuchmail.org, Someone Else <someone@example.com>"' \
             [subject]=notmuch-reply-test \
            '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
            '[body]="Multiple recipients"'

output=$(notmuch reply  --reply-to=sender  id:${gen_msg_id})
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> Multiple recipients"

test_begin_subtest "From Us, Multiple TO recipients"
add_message '[from]="Notmuch Test Suite <test_suite@notmuchmail.org>"' \
            '[to]="Recipient <recipient@example.com>, Someone Else <someone@example.com>"' \
             [subject]=notmuch-reply-test \
            '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
            '[body]="From Us, Multiple TO recipients"'

output=$(notmuch reply  --reply-to=sender  id:${gen_msg_id})
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Recipient <recipient@example.com>, Someone Else <someone@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Notmuch Test Suite <test_suite@notmuchmail.org> wrote:
> From Us, Multiple TO recipients"

test_begin_subtest "Reply with CC"
add_message '[from]="Sender <sender@example.com>"' \
             [to]=test_suite@notmuchmail.org \
            '[cc]="Other Parties <cc@example.com>"' \
             [subject]=notmuch-reply-test \
            '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
            '[body]="reply with CC"'

output=$(notmuch reply  --reply-to=sender id:${gen_msg_id})
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> reply with CC"

test_begin_subtest "From Us, Reply with CC"
add_message '[from]="Notmuch Test Suite <test_suite@notmuchmail.org>"' \
            '[to]="Recipient <recipient@example.com>"' \
            '[cc]="Other Parties <cc@example.com>"' \
             [subject]=notmuch-reply-test \
            '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
            '[body]="reply with CC"'

output=$(notmuch reply  --reply-to=sender id:${gen_msg_id})
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Recipient <recipient@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Notmuch Test Suite <test_suite@notmuchmail.org> wrote:
> reply with CC"

test_begin_subtest "From Us, Reply no TO but with CC"
add_message '[from]="Notmuch Test Suite <test_suite@notmuchmail.org>"' \
            '[cc]="Other Parties <cc@example.com>"' \
             [subject]=notmuch-reply-test \
            '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
            '[body]="reply with CC"'

output=$(notmuch reply  --reply-to=sender id:${gen_msg_id})
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
Cc: Other Parties <cc@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Notmuch Test Suite <test_suite@notmuchmail.org> wrote:
> reply with CC"

test_begin_subtest "Reply from alternate address"
add_message '[from]="Sender <sender@example.com>"' \
             [to]=test_suite_other@notmuchmail.org \
             [subject]=notmuch-reply-test \
            '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
            '[body]="reply from alternate address"'

output=$(notmuch reply  --reply-to=sender id:${gen_msg_id})
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite_other@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> reply from alternate address"

test_begin_subtest "Support for Reply-To"
add_message '[from]="Sender <sender@example.com>"' \
             [to]=test_suite@notmuchmail.org \
             [subject]=notmuch-reply-test \
            '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
            '[body]="support for reply-to"' \
            '[reply-to]="Sender <elsewhere@example.com>"'

output=$(notmuch reply  --reply-to=sender id:${gen_msg_id})
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <elsewhere@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> support for reply-to"

test_begin_subtest "Support for Reply-To with multiple recipients"
add_message '[from]="Sender <sender@example.com>"' \
            '[to]="test_suite@notmuchmail.org, Someone Else <someone@example.com>"' \
             [subject]=notmuch-reply-test \
            '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
            '[body]="support for reply-to with multiple recipients"' \
            '[reply-to]="Sender <elsewhere@example.com>"'

output=$(notmuch reply  --reply-to=sender id:${gen_msg_id})
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <elsewhere@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> support for reply-to with multiple recipients"

test_begin_subtest "Un-munging Reply-To"
add_message '[from]="Sender <sender@example.com>"' \
            '[to]="Some List <list@example.com>"' \
             [subject]=notmuch-reply-test \
            '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
            '[body]="Un-munging Reply-To"' \
            '[reply-to]="Evil Munging List <list@example.com>"'

output=$(notmuch reply  --reply-to=sender id:${gen_msg_id})
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Sender <sender@example.com> wrote:
> Un-munging Reply-To"

test_begin_subtest "Message with header of exactly 200 bytes"
add_message '[subject]="This subject is exactly 200 bytes in length. Other than its length there is not much of note here. Note that the length of 200 bytes includes the Subject: and Re: prefixes with two spaces"' \
            '[date]="Tue, 05 Jan 2010 15:43:56 -0000"' \
            '[body]="200-byte header"'
output=$(notmuch reply  --reply-to=sender id:${gen_msg_id})
test_expect_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: This subject is exactly 200 bytes in length. Other than its
 length there is not much of note here. Note that the length of 200 bytes
 includes the Subject: and Re: prefixes with two spaces
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0000, Notmuch Test Suite <test_suite@notmuchmail.org> wrote:
> 200-byte header"
test_done
