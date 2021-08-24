#!/usr/bin/env bash
test_description='"notmuch search" in several variations'
. $(dirname "$0")/test-lib.sh || exit 1

if [ $NOTMUCH_HAVE_SFSEXP -ne 1 ]; then
    printf "Skipping due to missing sfsexp library\n"
    test_done
fi

add_email_corpus

for query in '()' '(not)' '(and)' '(or ())' '(or (not))' '(or (and))' \
            '(or (and) (or) (not (and)))'; do
    test_begin_subtest "all messages: $query"
    notmuch search '*' > EXPECTED
    notmuch search --query=sexp "$query" > OUTPUT
    test_expect_equal_file EXPECTED OUTPUT
done

for query in '(or)' '(not ())' '(not (not))' '(not (and))' \
                   '(not (or (and) (or) (not (and))))'; do
    test_begin_subtest "no messages: $query"
    notmuch search --query=sexp "$query" > OUTPUT
    test_expect_equal_file /dev/null OUTPUT
done

test_begin_subtest "and of exact terms"
notmuch search --query=sexp '(and "wonderful" "wizard")' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2009-11-18 [1/3] Carl Worth| Jan Janak; [notmuch] What a great idea! (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "or of exact terms"
notmuch search --query=sexp '(or "php" "wizard")' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2010-12-29 [1/1] François Boulogne; [aur-general] Guidelines: cp, mkdir vs install (inbox unread)
thread:XXX   2009-11-18 [1/3] Carl Worth| Jan Janak; [notmuch] What a great idea! (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "single term in body"
notmuch search --query=sexp 'wizard' | notmuch_search_sanitize>OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2009-11-18 [1/3] Carl Worth| Jan Janak; [notmuch] What a great idea! (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "single term in body (case insensitive)"
notmuch search --query=sexp 'Wizard' | notmuch_search_sanitize>OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2009-11-18 [1/3] Carl Worth| Jan Janak; [notmuch] What a great idea! (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "single term in body, stemmed version"
notmuch search arriv > EXPECTED
notmuch search --query=sexp arriv > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "single term in body, unstemmed version"
notmuch search --query=sexp '"arriv"' > OUTPUT
test_expect_equal_file /dev/null OUTPUT

test_begin_subtest "Search by 'subject'"
add_message [subject]=subjectsearchtest '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
output=$(notmuch search --query=sexp '(subject subjectsearchtest)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; subjectsearchtest (inbox unread)"

test_begin_subtest "Search by 'subject' (case insensitive)"
notmuch search tag:inbox and subject:maildir | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp '(subject "Maildir")' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Search by 'subject' (utf-8):"
add_message [subject]=utf8-sübjéct '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
output=$(notmuch search --query=sexp '(subject utf8 sübjéct)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-sübjéct (inbox unread)"

test_begin_subtest "Search by 'subject' (utf-8, and):"
output=$(notmuch search --query=sexp '(subject (and utf8 sübjéct))' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-sübjéct (inbox unread)"

test_begin_subtest "Search by 'subject' (utf-8, and outside):"
output=$(notmuch search --query=sexp '(and (subject utf8) (subject sübjéct))' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-sübjéct (inbox unread)"

test_begin_subtest "Search by 'subject' (utf-8, or):"
notmuch search --query=sexp '(subject (or utf8 subjectsearchtest))' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; subjectsearchtest (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-sübjéct (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Search by 'subject' (utf-8, or outside):"
notmuch search --query=sexp '(or (subject utf8) (subject subjectsearchtest))' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; subjectsearchtest (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-sübjéct (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Search by 'attachment'"
notmuch search attachment:notmuch-help.patch > EXPECTED
notmuch search --query=sexp '(attachment notmuch-help.patch)' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Search by 'body'"
add_message '[subject]="body search"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' [body]=bodysearchtest
output=$(notmuch search --query=sexp '(body bodysearchtest)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; body search (inbox unread)"

test_begin_subtest "Search by 'body' (phrase)"
add_message '[subject]="body search (phrase)"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' '[body]="body search (phrase)"'
add_message '[subject]="negative result"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' '[body]="This phrase should not match the body search"'
output=$(notmuch search --query=sexp '(body "body search phrase")' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; body search (phrase) (inbox unread)"

test_begin_subtest "Search by 'body' (utf-8):"
add_message '[subject]="utf8-message-body-subject"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' '[body]="message body utf8: bödý"'
output=$(notmuch search --query=sexp '(body bödý)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-message-body-subject (inbox unread)"

test_begin_subtest "Search by 'from'"
add_message '[subject]="search by from"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' [from]=searchbyfrom
output=$(notmuch search --query=sexp '(from searchbyfrom)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] searchbyfrom; search by from (inbox unread)"

test_begin_subtest "Search by 'from' (address)"
add_message '[subject]="search by from (address)"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' [from]=searchbyfrom@example.com
output=$(notmuch search --query=sexp '(from searchbyfrom@example.com)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] searchbyfrom@example.com; search by from (address) (inbox unread)"

test_begin_subtest "Search by 'from' (name)"
add_message '[subject]="search by from (name)"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' '[from]="Search By From Name <test@example.com>"'
output=$(notmuch search --query=sexp '(from "Search By From Name")' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Search By From Name; search by from (name) (inbox unread)"

test_begin_subtest "Search by 'from' (name and address)"
output=$(notmuch search --query=sexp '(from "Search By From Name <test@example.com>")' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Search By From Name; search by from (name) (inbox unread)"

add_message '[dir]=bad' '[subject]="To the bone"'
add_message '[dir]=.' '[subject]="Top level"'
add_message '[dir]=bad/news' '[subject]="Bears"'
mkdir -p "${MAIL_DIR}/duplicate/bad/news"
cp "$gen_msg_filename" "${MAIL_DIR}/duplicate/bad/news"

add_message '[dir]=things' '[subject]="These are a few"'
add_message '[dir]=things/favorite' '[subject]="Raindrops, whiskers, kettles"'
add_message '[dir]=things/bad' '[subject]="Bites, stings, sad feelings"'

test_begin_subtest "Search by 'folder' (multiple)"
output=$(notmuch search --query=sexp '(folder bad bad/news things/bad)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; To the bone (inbox unread)
thread:XXX   2001-01-05 [1/1(2)] Notmuch Test Suite; Bears (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Bites, stings, sad feelings (inbox unread)"

test_begin_subtest "Search by 'folder': top level."
notmuch search folder:'""' > EXPECTED
notmuch search --query=sexp '(folder "")'  > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Search by 'id'"
add_message '[subject]="search by id"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
output=$(notmuch search --query=sexp "(id ${gen_msg_id})" | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by id (inbox unread)"

test_begin_subtest "Search by 'id' (or)"
add_message '[subject]="search by id"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
output=$(notmuch search --query=sexp "(id non-existent-mid ${gen_msg_id})" | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by id (inbox unread)"

test_begin_subtest "Search by 'is' (multiple)"
notmuch tag -inbox tag:searchbytag
notmuch search is:inbox AND is:unread | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp '(is inbox unread)' | notmuch_search_sanitize > OUTPUT
notmuch tag +inbox tag:searchbytag
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Search by 'mid'"
add_message '[subject]="search by mid"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
output=$(notmuch search --query=sexp "(mid ${gen_msg_id})" | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by mid (inbox unread)"

test_begin_subtest "Search by 'mid' (or)"
add_message '[subject]="search by mid"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
output=$(notmuch search --query=sexp "(mid non-existent-mid ${gen_msg_id})" | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by mid (inbox unread)"

test_begin_subtest "Search by 'mimetype'"
notmuch search mimetype:text/html > EXPECTED
notmuch search --query=sexp '(mimetype text html)'  > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Search by 'subject' (utf-8, phrase-token):"
output=$(notmuch search --query=sexp '(subject utf8-sübjéct)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-sübjéct (inbox unread)"

test_begin_subtest "Search by 'subject' (utf-8, quoted string):"
output=$(notmuch search --query=sexp '(subject "utf8 sübjéct")' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-sübjéct (inbox unread)"

test_begin_subtest "Search by 'subject' (combine phrase, term):"
output=$(notmuch search --query=sexp '(subject Mac "compatibility issues")' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox unread)"

test_begin_subtest "Search by 'subject' (combine phrase, term 2):"
notmuch search --query=sexp '(subject (or utf8 "compatibility issues"))' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-sübjéct (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-message-body-subject (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Search by 'subject' (combine phrase, term 3):"
notmuch search --query=sexp '(subject issues X/Darwin)' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2009-11-18 [4/4] Jjgod Jiang, Alexander Botero-Lowry; [notmuch] Mac OS X/Darwin compatibility issues (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Search by 'tag'"
add_message '[subject]="search by tag"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
notmuch tag +searchbytag id:${gen_msg_id}
output=$(notmuch search --query=sexp '(tag searchbytag)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by tag (inbox searchbytag unread)"

test_begin_subtest "Search by 'tag' (multiple)"
notmuch tag -inbox tag:searchbytag
notmuch search tag:inbox AND tag:unread | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp '(tag inbox unread)' | notmuch_search_sanitize > OUTPUT
notmuch tag +inbox tag:searchbytag
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Search by 'tag' and 'subject'"
notmuch search tag:inbox and subject:maildir | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp '(and (tag inbox) (subject maildir))' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Search by 'thread'"
add_message '[subject]="search by thread"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"'
thread_id=$(notmuch search id:${gen_msg_id} | sed -e "s/thread:\([a-f0-9]*\).*/\1/")
output=$(notmuch search --query=sexp "(thread ${thread_id})" | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by thread (inbox unread)"

test_begin_subtest "Search by 'to'"
add_message '[subject]="search by to"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' [to]=searchbyto
output=$(notmuch search --query=sexp '(to searchbyto)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (inbox unread)"

test_begin_subtest "Search by 'to' (address)"
add_message '[subject]="search by to (address)"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' [to]=searchbyto@example.com
output=$(notmuch search --query=sexp '(to searchbyto@example.com)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (address) (inbox unread)"

test_begin_subtest "Search by 'to' (name)"
add_message '[subject]="search by to (name)"' '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' '[to]="Search By To Name <test@example.com>"'
output=$(notmuch search --query=sexp '(to "Search By To Name")' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (name) (inbox unread)"

test_begin_subtest "Search by 'to' (name and address)"
output=$(notmuch search --query=sexp '(to "Search By To Name <test@example.com>")' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (name) (inbox unread)"

test_begin_subtest "starts-with, no prefix"
output=$(notmuch search --query=sexp '(starts-with prelim)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2009-11-17 [2/2] Alex Botero-Lowry, Carl Worth; [notmuch] preliminary FreeBSD support (attachment inbox unread)"

test_begin_subtest "starts-with, case-insensitive"
notmuch search --query=sexp '(starts-with FreeB)' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2009-11-18 [3/4] Alexander Botero-Lowry, Jjgod Jiang; [notmuch] Mac OS X/Darwin compatibility issues (inbox unread)
thread:XXX   2009-11-17 [2/2] Alex Botero-Lowry, Carl Worth; [notmuch] preliminary FreeBSD support (attachment inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "starts-with, no prefix, all messages"
notmuch search --query=sexp '(starts-with "")' | notmuch_search_sanitize > OUTPUT
notmuch search '*' | notmuch_search_sanitize > EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "starts-with, attachment"
output=$(notmuch search --query=sexp '(attachment (starts-with not))' | notmuch_search_sanitize)
test_expect_equal "$output" 'thread:XXX   2009-11-18 [2/2] Lars Kellogg-Stedman; [notmuch] "notmuch help" outputs to stderr? (attachment inbox signed unread)'

test_begin_subtest "starts-with, folder"
notmuch search --output=files --query=sexp '(folder (starts-with bad))' | notmuch_dir_sanitize > OUTPUT
cat <<EOF > EXPECTED
MAIL_DIR/bad/msg-010
MAIL_DIR/bad/news/msg-012
MAIL_DIR/duplicate/bad/news/msg-012
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "starts-with, from"
notmuch search --query=sexp '(from (starts-with Mik))' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2009-11-17 [1/1] Mikhail Gusarov; [notmuch] [PATCH] Handle rename of message file (inbox unread)
thread:XXX   2009-11-17 [2/7] Mikhail Gusarov| Lars Kellogg-Stedman, Keith Packard, Carl Worth; [notmuch] Working with Maildir storage? (inbox signed unread)
thread:XXX   2009-11-17 [2/5] Mikhail Gusarov| Carl Worth, Keith Packard; [notmuch] [PATCH 2/2] Include <stdint.h> to get uint32_t in C++ file with gcc 4.4 (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "starts-with, id"
notmuch search --query=sexp --output=messages '(id (starts-with 877))' > OUTPUT
cat <<EOF > EXPECTED
id:877h1wv7mg.fsf@inf-8657.int-evry.fr
id:877htoqdbo.fsf@yoom.home.cworth.org
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "starts-with, is"
output=$(notmuch search --query=sexp '(is (starts-with searchby))' | notmuch_search_sanitize)
test_expect_equal "$output" 'thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by tag (inbox searchbytag unread)'

test_begin_subtest "starts-with, mid"
notmuch search --query=sexp --output=messages '(mid (starts-with 877))' > OUTPUT
cat <<EOF > EXPECTED
id:877h1wv7mg.fsf@inf-8657.int-evry.fr
id:877htoqdbo.fsf@yoom.home.cworth.org
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "starts-with, mimetype"
notmuch search --query=sexp '(mimetype (starts-with sig))' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2009-11-18 [2/2] Lars Kellogg-Stedman; [notmuch] "notmuch help" outputs to stderr? (attachment inbox signed unread)
thread:XXX   2009-11-18 [4/7] Lars Kellogg-Stedman, Mikhail Gusarov| Keith Packard, Carl Worth; [notmuch] Working with Maildir storage? (inbox signed unread)
thread:XXX   2009-11-17 [1/3] Adrian Perez de Castro| Keith Packard, Carl Worth; [notmuch] Introducing myself (inbox signed unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

add_message '[subject]="message with properties"'
notmuch restore <<EOF
#= ${gen_msg_id} foo=bar
EOF

test_begin_subtest "starts-with, property"
notmuch search --query=sexp '(property (starts-with foo=))' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; message with properties (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "starts-with, subject"
notmuch search --query=sexp '(subject (starts-with FreeB))' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2009-11-17 [2/2] Alex Botero-Lowry, Carl Worth; [notmuch] preliminary FreeBSD support (attachment inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "starts-with, tag"
output=$(notmuch search --query=sexp '(tag (starts-with searchby))' | notmuch_search_sanitize)
test_expect_equal "$output" 'thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by tag (inbox searchbytag unread)'

add_message '[subject]="no tags"'
notag_mid=${gen_msg_id}
notmuch tag -unread -inbox id:${notag_mid}

test_begin_subtest "negated starts-with, tag"
output=$(notmuch search --query=sexp '(tag (not (starts-with in)))' | notmuch_search_sanitize)
test_expect_equal "$output" 'thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; no tags ()'

test_begin_subtest "negated starts-with, tag 2"
output=$(notmuch search --query=sexp '(not (tag (starts-with in)))' | notmuch_search_sanitize)
test_expect_equal "$output" 'thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; no tags ()'

test_begin_subtest "negated starts-with, tag 3"
output=$(notmuch search --query=sexp '(not (tag (starts-with "")))' | notmuch_search_sanitize)
test_expect_equal "$output" 'thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; no tags ()'

test_begin_subtest "starts-with, thread"
notmuch search --query=sexp '(thread (starts-with "00"))' > OUTPUT
notmuch search '*' > EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "starts-with, to"
notmuch search --query=sexp '(to (starts-with "search"))' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (address) (inbox unread)
thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; search by to (name) (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "wildcard search for 'is'"
notmuch search not id:${notag_mid} > EXPECTED
notmuch search --query=sexp '(is *)' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "negated wildcard search for 'is'"
notmuch search id:${notag_mid} > EXPECTED
notmuch search --query=sexp '(not (is *))' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "wildcard search for 'property'"
notmuch search property:foo=bar > EXPECTED
notmuch search --query=sexp '(property *)' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "wildcard search for 'tag'"
notmuch search not id:${notag_mid} > EXPECTED
notmuch search --query=sexp '(tag *)' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "negated wildcard search for 'tag'"
notmuch search id:${notag_mid} > EXPECTED
notmuch search --query=sexp '(not (tag *))' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

add_message '[subject]="message with tag \"*\""'
notmuch tag '+*' id:${gen_msg_id}

test_begin_subtest "search for 'tag' \"*\""
output=$(notmuch search --query=sexp --output=messages '(tag "*")')
test_expect_equal "$output" "id:$gen_msg_id"

test_begin_subtest "search for missing / empty to"
add_message [to]="undisclosed-recipients:"
notmuch search --query=sexp '(not (to *))' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; search for missing / empty to (inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Unbalanced parens"
# A code 1 indicates the error was handled (a crash will return e.g. 139).
test_expect_code 1 "notmuch search --query=sexp '('"

test_begin_subtest "Unbalanced parens, error message"
notmuch search --query=sexp '(' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
invalid s-expression: '('
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "unknown prefix"
notmuch search --query=sexp '(foo)' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
unknown prefix 'foo'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "list as prefix"
notmuch search --query=sexp '((foo))' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
unexpected list in field/operation position
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "illegal nesting"
notmuch search --query=sexp '(subject (subject foo))' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
nested field: 'subject' inside 'subject'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "starts-with, no argument"
notmuch search --query=sexp '(starts-with)' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
'starts-with' expects single atom as argument
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "starts-with, list argument"
notmuch search --query=sexp '(starts-with (stuff))' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
'starts-with' expects single atom as argument
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "starts-with, too many arguments"
notmuch search --query=sexp '(starts-with a b c)' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
'starts-with' expects single atom as argument
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "starts-with, illegal field"
notmuch search --query=sexp '(body (starts-with foo))' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
'body' does not support wildcard queries
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "wildcard, illegal field"
notmuch search --query=sexp '(body *)' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
'body' does not support wildcard queries
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
