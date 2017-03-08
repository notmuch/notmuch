#!/usr/bin/env bash
test_description='regular expression searches'
. ./test-lib.sh || exit 1

if [ $NOTMUCH_HAVE_XAPIAN_FIELD_PROCESSOR -eq 0 ]; then
    test_done
fi

add_message '[dir]=bad' '[subject]="To the bone"'
add_message '[dir]=.' '[subject]="Top level"'
add_message '[dir]=bad/news' '[subject]="Bears"'
mkdir -p "${MAIL_DIR}/duplicate/bad/news"
cp "$gen_msg_filename" "${MAIL_DIR}/duplicate/bad/news"

add_message '[dir]=things' '[subject]="These are a few"'
add_message '[dir]=things/favorite' '[subject]="Raindrops, whiskers, kettles"'
add_message '[dir]=things/bad' '[subject]="Bites, stings, sad feelings"'

test_begin_subtest "empty path:// search"
notmuch search 'path:""' > EXPECTED
notmuch search 'path:/^$/' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "empty folder:// search"
notmuch search 'folder:""' > EXPECTED
notmuch search 'folder:/^$/' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "unanchored folder:// specification"
output=$(notmuch search folder:/bad/ | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; To the bone (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Bears (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Bites, stings, sad feelings (inbox unread)"

test_begin_subtest "anchored folder:// search"
output=$(notmuch search 'folder:/^bad$/' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; To the bone (inbox unread)"

test_begin_subtest "unanchored path:// specification"
output=$(notmuch search path:/bad/ | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; To the bone (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Bears (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Bites, stings, sad feelings (inbox unread)"

test_begin_subtest "anchored path:// search"
output=$(notmuch search 'path:/^bad$/' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; To the bone (inbox unread)"

# Use "standard" corpus from here on.
rm -rf $MAIL_DIR
add_email_corpus

notmuch search --output=messages from:cworth > cworth.msg-ids

# these headers will generate no document terms
add_message '[from]="-" [subject]="empty from"'
add_message '[subject]="-"'

test_begin_subtest "null from: search"
notmuch search 'from:""' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2001-01-05 [1/1] -; empty from (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "null subject: search"
notmuch search 'subject:""' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; - (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "xapian wildcard search for from:"
notmuch search --output=messages 'from:cwo*' > OUTPUT
test_expect_equal_file cworth.msg-ids OUTPUT

test_begin_subtest "xapian wildcard search for subject:"
test_expect_equal $(notmuch count 'subject:count*') 1

test_begin_subtest "regexp from search, case sensitive"
notmuch search --output=messages from:/carl/ > OUTPUT
test_expect_equal_file /dev/null OUTPUT

test_begin_subtest "empty regexp or query"
notmuch search --output=messages from:/carl/ or from:/cworth/ > OUTPUT
test_expect_equal_file cworth.msg-ids OUTPUT

test_begin_subtest "non-empty regexp and query"
notmuch search  from:/cworth@cworth.org/ and subject:patch | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2009-11-18 [1/2] Carl Worth| Alex Botero-Lowry; [notmuch] [PATCH] Error out if no query is supplied to search instead of going into an infinite loop (attachment inbox unread)
thread:XXX   2009-11-18 [1/2] Carl Worth| Ingmar Vanhassel; [notmuch] [PATCH] Typsos (inbox unread)
thread:XXX   2009-11-18 [1/2] Carl Worth| Jan Janak; [notmuch] [PATCH] Older versions of install do not support -C. (inbox unread)
thread:XXX   2009-11-18 [1/2] Carl Worth| Keith Packard; [notmuch] [PATCH] Make notmuch-show 'X' (and 'x') commands remove inbox (and unread) tags (inbox unread)
thread:XXX   2009-11-18 [2/5] Carl Worth| Mikhail Gusarov, Keith Packard; [notmuch] [PATCH 1/2] Close message file after parsing message headers (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "regexp from search, duplicate term search"
notmuch search --output=messages from:/cworth/ > OUTPUT
test_expect_equal_file cworth.msg-ids OUTPUT

test_begin_subtest "long enough regexp matches only desired senders"
notmuch search --output=messages 'from:"/C.* Wo/"' > OUTPUT
test_expect_equal_file cworth.msg-ids OUTPUT

test_begin_subtest "shorter regexp matches one more sender"
notmuch search --output=messages 'from:"/C.* W/"' > OUTPUT
{ echo id:1258544095-16616-1-git-send-email-chris@chris-wilson.co.uk; cat cworth.msg-ids; } > EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "regexp subject search, non-ASCII"
notmuch search --output=messages subject:/accentué/ > OUTPUT
echo id:877h1wv7mg.fsf@inf-8657.int-evry.fr > EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "regexp subject search, punctuation"
notmuch search subject:/\'X\'/ | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2009-11-18 [2/2] Keith Packard, Carl Worth; [notmuch] [PATCH] Make notmuch-show 'X' (and 'x') commands remove inbox (and unread) tags (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "regexp subject search, no punctuation"
notmuch search  subject:/X/ | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2009-11-18 [2/2] Keith Packard, Carl Worth; [notmuch] [PATCH] Make notmuch-show 'X' (and 'x') commands remove inbox (and unread) tags (inbox unread)
thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "combine regexp from and subject"
notmuch search  subject:/-C/ and from:/.an.k/ | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2009-11-17 [1/2] Jan Janak| Carl Worth; [notmuch] [PATCH] Older versions of install do not support -C. (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "regexp error reporting"
notmuch search 'from:/unbalanced[/' 1>OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: A Xapian exception occurred
A Xapian exception occurred parsing query: Invalid regular expression
Query string was: from:/unbalanced[/
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "empty mid search"
notmuch search --output=messages mid:yoom > OUTPUT
cp /dev/null EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "non-empty mid regex search"
notmuch search --output=messages mid:/yoom/ > OUTPUT
test_expect_equal_file cworth.msg-ids OUTPUT

test_begin_subtest "combine regexp mid and subject"
notmuch search  subject:/-C/ and mid:/y..m/ | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2009-11-18 [1/2] Carl Worth| Jan Janak; [notmuch] [PATCH] Older versions of install do not support -C. (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "unanchored tag search"
notmuch search tag:signed or tag:inbox > EXPECTED
notmuch search tag:/i/ > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

notmuch tag +testsi '*'
test_begin_subtest "anchored tag search"
notmuch search tag:signed > EXPECTED
notmuch search tag:/^si/ > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_done
