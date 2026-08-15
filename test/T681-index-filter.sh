#!/usr/bin/env bash
test_description="indexing attachment with a filter"
. $(dirname "$0")/test-lib.sh || exit 1

notmuch config set index.as_text ".*"

cat <<EOF > $MAIL_DIR/attachment-empty.eml
From: example@example.net
To: example@example.com
Subject: zero-size attachment
Date: Sun, 09 Feb 2025 12:33:43 +0000
Message-ID: <177064044971.16863.empty@localhost>
MIME-Version: 1.0
Content-Type: text/plain
Content-Disposition: attachment; filename=foo.txt
Content-Transfer-Encoding: base64

EOF

MSG_FILE_LARGE=${MAIL_DIR}/attachment-large.eml

cat <<EOF > $MSG_FILE_LARGE
From: example@example.net
To: example@example.com
Subject: large attachment
Date: Sun, 09 Feb 2025 12:33:44 +0000
Message-ID: <177064044971.16863.large@localhost>
MIME-Version: 1.0
Content-Type: text/plain
Content-Disposition: attachment; filename=foo.txt
Content-Transfer-Encoding: base64

EOF

{ for i in $(seq 65536); do echo $i; done } | base64 >> $MSG_FILE_LARGE

notmuch new

cat <<EOF > EXPECTED
thread:XXX   2025-02-09 [1/1] example@example.net; large attachment (attachment inbox unread)
thread:XXX   2025-02-09 [1/1] example@example.net; zero-size attachment (attachment inbox unread)
EOF

test_begin_subtest 'input ignored'
notmuch config set index.filter "/bin/sh -c 'echo secretstring'"
notmuch reindex '*'
notmuch search "secretstring" | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'input consumed'
notmuch config set index.filter "/bin/sh -c 'cat - > /dev/null; echo secretstring'"
notmuch reindex '*'
notmuch search "secretstring" | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'interleaved IO'
# this filter interleaves reads of increasingly large weird-sized blocks
# with writes
notmuch config set index.filter '/bin/sh -c "
bs=53;
while true; do
    dd bs=\$bs count=1 2>&1 >/dev/null | grep -q \"^0+0 records in$\" >&2 && break;
    echo \$bs;
    bs=\$((\$bs+48));
done;
echo secretstring;
"'
notmuch reindex '*'
notmuch search "secretstring" | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'exit failure'
notmuch config set index.filter "/bin/sh -c 'echo secretstring; exit 1'"
notmuch reindex '*'
notmuch search "secretstring" | notmuch_search_sanitize > OUTPUT
test_expect_equal_file /dev/null OUTPUT

test_begin_subtest 'exit signal'
notmuch config set index.filter "/bin/sh -c 'echo secretstring; kill -ABRT \$\$'"
notmuch reindex '*'
notmuch search "secretstring" | notmuch_search_sanitize > OUTPUT
test_expect_equal_file /dev/null OUTPUT

test_done
