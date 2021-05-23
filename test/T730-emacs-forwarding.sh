#!/usr/bin/env bash

test_description="emacs forwarding"
. $(dirname "$0")/test-lib.sh || exit 1
. $NOTMUCH_SRCDIR/test/test-lib-emacs.sh || exit 1

test_require_emacs

test_begin_subtest "Forward setting the correct references header"
# Check that, when forwarding a message, the new message has
# a References-header pointing to the original (forwarded) message.

message_id='OriginalMessage@notmuchmail.org'
add_message \
    [id]="$message_id" \
    '[from]="user@example.com"' \
    '[subject]="This is the original message"' \
    '[body]="Dummy text."'

test_emacs_expect_t "
  (let ((message-send-mail-function (lambda () t)))
    (notmuch-show \"id:$message_id\")
    (notmuch-show-forward-message)
    (save-restriction
      (message-narrow-to-headers)
      (message-remove-header \"Fcc\")
      (message-remove-header \"To\")
      (message-add-header \"To: nobody@example.com\"))

    (notmuch-mua-send)
    (notmuch-test-expect-equal
        (message-field-value \"References\") \"<$message_id>\"))
"

test_begin_subtest "Forwarding adding the forwarded tag"
# Check that sending the forwarding message in the previous
# subtest did add the forwarded-tag to the message that was forwarded.

test_expect_equal "$(notmuch search --output=tags id:$message_id | sort)" \
"forwarded
inbox
unread"

test_done
