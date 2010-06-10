#!/bin/bash
test_description="\"notmuch reply\" in several variations"
. ./test-lib.sh

test_expect_success "Basic reply" '
add_message "[from]=\"Sender <sender@example.com>\"" \
             [to]=test_suite@notmuchmail.org \
             [subject]=notmuch-reply-test \
            "[date]=\"Tue, 05 Jan 2010 15:43:56 -0800\"" \
            "[body]=\"basic reply test\"" &&

output=$($NOTMUCH reply id:${gen_msg_id}) &&
pass_if_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>
Bcc: test_suite@notmuchmail.org
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0800, Sender <sender@example.com> wrote:
> basic reply test"

'
test_expect_success "Multiple recipients" '
add_message "[from]=\"Sender <sender@example.com>\"" \
            "[to]=\"test_suite@notmuchmail.org, Someone Else <someone@example.com>\"" \
             [subject]=notmuch-reply-test \
            "[date]=\"Tue, 05 Jan 2010 15:43:56 -0800\"" \
            "[body]=\"Multiple recipients\"" &&

output=$($NOTMUCH reply id:${gen_msg_id}) &&
pass_if_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>, Someone Else <someone@example.com>
Bcc: test_suite@notmuchmail.org
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0800, Sender <sender@example.com> wrote:
> Multiple recipients"

'
test_expect_success "Reply with CC" '
add_message "[from]=\"Sender <sender@example.com>\"" \
             [to]=test_suite@notmuchmail.org \
            "[cc]=\"Other Parties <cc@example.com>\"" \
             [subject]=notmuch-reply-test \
            "[date]=\"Tue, 05 Jan 2010 15:43:56 -0800\"" \
            "[body]=\"reply with CC\"" &&

output=$($NOTMUCH reply id:${gen_msg_id}) &&
pass_if_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>
Cc: Other Parties <cc@example.com>
Bcc: test_suite@notmuchmail.org
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0800, Sender <sender@example.com> wrote:
> reply with CC"

'
test_expect_success "Reply from alternate address" '
add_message "[from]=\"Sender <sender@example.com>\"" \
             [to]=test_suite_other@notmuchmail.org \
             [subject]=notmuch-reply-test \
            "[date]=\"Tue, 05 Jan 2010 15:43:56 -0800\"" \
            "[body]=\"reply from alternate address\"" &&

output=$($NOTMUCH reply id:${gen_msg_id}) &&
pass_if_equal "$output" "From: Notmuch Test Suite <test_suite_other@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>
Bcc: test_suite@notmuchmail.org
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0800, Sender <sender@example.com> wrote:
> reply from alternate address"

'
test_expect_success "Support for Reply-To" '
add_message "[from]=\"Sender <sender@example.com>\"" \
             [to]=test_suite@notmuchmail.org \
             [subject]=notmuch-reply-test \
            "[date]=\"Tue, 05 Jan 2010 15:43:56 -0800\"" \
            "[body]=\"support for reply-to\"" \
            "[reply-to]=\"Sender <elsewhere@example.com>\"" &&

output=$($NOTMUCH reply id:${gen_msg_id}) &&
pass_if_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <elsewhere@example.com>
Bcc: test_suite@notmuchmail.org
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0800, Sender <sender@example.com> wrote:
> support for reply-to"

'
test_expect_success "Un-munging Reply-To" '
add_message "[from]=\"Sender <sender@example.com>\"" \
            "[to]=\"Some List <list@example.com>\"" \
             [subject]=notmuch-reply-test \
            "[date]=\"Tue, 05 Jan 2010 15:43:56 -0800\"" \
            "[body]=\"Un-munging Reply-To\"" \
            "[reply-to]=\"Evil Munging List <list@example.com>\"" &&

output=$($NOTMUCH reply id:${gen_msg_id}) &&
pass_if_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: notmuch-reply-test
To: Sender <sender@example.com>, Some List <list@example.com>
Bcc: test_suite@notmuchmail.org
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0800, Sender <sender@example.com> wrote:
> Un-munging Reply-To"
'

test_expect_success "Message with header of exactly 200 bytes" '
add_message "[subject]=\"This subject is exactly 200 bytes in length. Other than its length there is not much of note here. Note that the length of 200 bytes includes the Subject: and Re: prefixes with two spaces\"" \
            "[date]=\"Tue, 05 Jan 2010 15:43:56 -0800\"" \
            "[body]=\"200-byte header\"" &&
output=$($NOTMUCH reply id:${gen_msg_id}) &&
pass_if_equal "$output" "From: Notmuch Test Suite <test_suite@notmuchmail.org>
Subject: Re: This subject is exactly 200 bytes in length. Other than its length there is not much of note here. Note that the length of 200 bytes includes the Subject: and Re: prefixes with two spaces
Bcc: test_suite@notmuchmail.org
In-Reply-To: <${gen_msg_id}>
References: <${gen_msg_id}>

On Tue, 05 Jan 2010 15:43:56 -0800, Notmuch Test Suite <test_suite@notmuchmail.org> wrote:
> 200-byte header"
'
test_done
