#!/usr/bin/env bash
test_description="date:since..until queries"
. ./test-lib.sh || exit 1

add_email_corpus

test_begin_subtest "Absolute date range"
output=$(notmuch search date:2010-12-16..12/16/2010 | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2010-12-16 [1/1] Olivier Berger; Essai accentué (inbox unread)"

test_begin_subtest "Absolute date range with 'same' operator"
output=$(notmuch search date:2010-12-16..! | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2010-12-16 [1/1] Olivier Berger; Essai accentué (inbox unread)"

if [ "${NOTMUCH_HAVE_XAPIAN_FIELD_PROCESSOR}" = "1" ]; then
    test_begin_subtest "Absolute date field"
    output=$(notmuch search date:2010-12-16 | notmuch_search_sanitize)
    test_expect_equal "$output" "thread:XXX   2010-12-16 [1/1] Olivier Berger; Essai accentué (inbox unread)"
fi

test_begin_subtest "Absolute time range with TZ"
notmuch search date:18-Nov-2009_02:19:26-0800..2009-11-18_04:49:52-06:00 | notmuch_search_sanitize > OUTPUT
cat <<EOF >EXPECTED
thread:XXX   2009-11-18 [1/3] Carl Worth| Jan Janak; [notmuch] What a great idea! (inbox unread)
thread:XXX   2009-11-18 [1/2] Carl Worth| Jan Janak; [notmuch] [PATCH] Older versions of install do not support -C. (inbox unread)
thread:XXX   2009-11-18 [1/3] Carl Worth| Aron Griffis, Keith Packard; [notmuch] archive (inbox unread)
thread:XXX   2009-11-18 [1/2] Carl Worth| Keith Packard; [notmuch] [PATCH] Make notmuch-show 'X' (and 'x') commands remove inbox (and unread) tags (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
