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

add_message "[body]=thebody-1" "[subject]=kryptonite-1"
add_message "[body]=nothing-to-see-here-1" "[subject]=thebody-1"

test_begin_subtest 'search without body: prefix'
notmuch search thebody > EXPECTED
notmuch search --query=sexp '(and thebody)' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'negated body: prefix'
notmuch search thebody and not body:thebody > EXPECTED
notmuch search --query=sexp '(and (not (body thebody)) thebody)' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'search unprefixed for prefixed term'
notmuch search kryptonite > EXPECTED
notmuch search --query=sexp '(and kryptonite)' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest 'search with body: prefix for term only in subject'
notmuch search body:kryptonite > EXPECTED
notmuch search --query=sexp '(body kryptonite)' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

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

test_begin_subtest "Search by 'folder' with --output=files"
output=$(notmuch search --output=files --query=sexp '(folder bad/news)' | notmuch_search_files_sanitize)
test_expect_equal "$output" "MAIL_DIR/bad/news/msg-XXX
MAIL_DIR/duplicate/bad/news/msg-XXX"

test_begin_subtest "Search by 'folder' with --output=files (trailing /)"
output=$(notmuch search --output=files --query=sexp '(folder bad/news/)' | notmuch_search_files_sanitize)
test_expect_equal "$output" "MAIL_DIR/bad/news/msg-XXX
MAIL_DIR/duplicate/bad/news/msg-XXX"

test_begin_subtest "Search by 'folder' (multiple)"
output=$(notmuch search --query=sexp '(folder bad bad/news things/bad)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; To the bone (inbox unread)
thread:XXX   2001-01-05 [1/1(2)] Notmuch Test Suite; Bears (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Bites, stings, sad feelings (inbox unread)"

test_begin_subtest "Search by 'folder' (multiple, trailing /)"
output=$(notmuch search --query=sexp '(folder bad bad/news/ things/bad)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; To the bone (inbox unread)
thread:XXX   2001-01-05 [1/1(2)] Notmuch Test Suite; Bears (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Bites, stings, sad feelings (inbox unread)"

test_begin_subtest "Search by 'path' with --output=files"
output=$(notmuch search --output=files --query=sexp '(path bad/news)' | notmuch_search_files_sanitize)
test_expect_equal "$output" "MAIL_DIR/bad/news/msg-XXX
MAIL_DIR/duplicate/bad/news/msg-XXX"

test_begin_subtest "Search by 'path' with --output=files (trailing /)"
output=$(notmuch search --output=files --query=sexp '(path bad/news/)' | notmuch_search_files_sanitize)
test_expect_equal "$output" "MAIL_DIR/bad/news/msg-XXX
MAIL_DIR/duplicate/bad/news/msg-XXX"

test_begin_subtest "Search by 'path' specification (multiple)"
output=$(notmuch search --query=sexp '(path bad bad/news things/bad)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; To the bone (inbox unread)
thread:XXX   2001-01-05 [1/1(2)] Notmuch Test Suite; Bears (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Bites, stings, sad feelings (inbox unread)"

test_begin_subtest "Search by 'path' specification (multiple, trailing /)"
output=$(notmuch search --query=sexp '(path bad bad/news/ things/bad)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; To the bone (inbox unread)
thread:XXX   2001-01-05 [1/1(2)] Notmuch Test Suite; Bears (inbox unread)
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Bites, stings, sad feelings (inbox unread)"

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

QUERYSTR="date:2009-11-18..2009-11-18 and tag:unread"
QUERYSTR2="query:test and subject:Maildir"
notmuch config set --database query.test "$QUERYSTR"
notmuch config set query.test2 "$QUERYSTR2"

test_begin_subtest "ill-formed named query search"
notmuch search --query=sexp '(query)' > OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
'query' expects single atom as argument
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "ill-formed named query search 2"
notmuch search --query=sexp '(to (query))' > OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
'query' not supported inside 'to'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "search named query"
notmuch search --query=sexp '(query test)' > OUTPUT
notmuch search $QUERYSTR > EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Search by 'subject' (utf-8, phrase-token):"
output=$(notmuch search --query=sexp '(subject utf8-sübjéct)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-sübjéct (inbox unread)"

test_begin_subtest "search named query with other terms"
notmuch search --query=sexp '(and (query test) (subject Maildir))' > OUTPUT
notmuch search $QUERYSTR and subject:Maildir > EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "search nested named query"
notmuch search --query=sexp '(query test2)' > OUTPUT
notmuch search $QUERYSTR2 > EXPECTED
test_expect_equal_file EXPECTED OUTPUT

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
notmuch search --output=files --query=sexp '(folder (starts-with bad))' | notmuch_search_files_sanitize > OUTPUT
cat <<EOF > EXPECTED
MAIL_DIR/bad/msg-XXX
MAIL_DIR/bad/news/msg-XXX
MAIL_DIR/duplicate/bad/news/msg-XXX
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

test_begin_subtest "Search, exclude \"deleted\" messages from search"
notmuch config set search.exclude_tags deleted
generate_message '[subject]="Not deleted"'
not_deleted_id=$gen_msg_id
generate_message '[subject]="Deleted"'
notmuch new > /dev/null
notmuch tag +deleted id:$gen_msg_id
deleted_id=$gen_msg_id
output=$(notmuch search --query=sexp '(subject deleted)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Not deleted (inbox unread)"

test_begin_subtest "Search, exclude \"deleted\" messages from message search --exclude=false"
output=$(notmuch search --query=sexp --exclude=false --output=messages '(subject deleted)' | notmuch_search_sanitize)
test_expect_equal "$output" "id:$not_deleted_id
id:$deleted_id"

test_begin_subtest "Search, exclude \"deleted\" messages from search, overridden"
notmuch search --query=sexp '(and (subject deleted) (tag deleted))' | notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Deleted (deleted inbox unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Search, exclude \"deleted\" messages from threads"
add_message '[subject]="Not deleted reply"' '[in-reply-to]="<$gen_msg_id>"'
output=$(notmuch search --query=sexp '(subject deleted)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Not deleted (inbox unread)
thread:XXX   2001-01-05 [1/2] Notmuch Test Suite; Not deleted reply (deleted inbox unread)"

test_begin_subtest "Search, don't exclude \"deleted\" messages when --exclude=flag specified"
output=$(notmuch search --query=sexp --exclude=flag '(subject deleted)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Not deleted (inbox unread)
thread:XXX   2001-01-05 [1/2] Notmuch Test Suite; Deleted (deleted inbox unread)"

test_begin_subtest "Search, don't exclude \"deleted\" messages from search if not configured"
notmuch config set search.exclude_tags
output=$(notmuch search --query=sexp '(subject deleted)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2001-01-05 [1/1] Notmuch Test Suite; Not deleted (inbox unread)
thread:XXX   2001-01-05 [2/2] Notmuch Test Suite; Deleted (deleted inbox unread)"

test_begin_subtest "regex at top level"
notmuch search --query=sexp '(rx foo)' >& OUTPUT
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
illegal 'rx' outside field
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "regex in illegal field"
notmuch search --query=sexp '(body (regex foo))' >& OUTPUT
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
'regex' not supported in field 'body'
EOF
test_expect_equal_file EXPECTED OUTPUT

notmuch search --output=messages from:cworth > cworth.msg-ids

test_begin_subtest "regexp 'from' search"
notmuch search --output=messages --query=sexp '(from (rx cworth))' > OUTPUT
test_expect_equal_file cworth.msg-ids OUTPUT

test_begin_subtest "regexp search for 'from' 2"
notmuch search from:/cworth@cworth.org/ and subject:patch | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp '(and (from (rx cworth@cworth.org)) (subject patch))' \
    | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "regexp 'folder' search"
notmuch search 'folder:/^bar$/' | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp '(folder (rx ^bar$))' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "regexp 'id' search"
notmuch search --output=messages --query=sexp '(id (rx yoom))' > OUTPUT
test_expect_equal_file cworth.msg-ids OUTPUT

test_begin_subtest "unanchored 'is' search"
notmuch search tag:signed or tag:inbox > EXPECTED
notmuch search --query=sexp '(is (rx i))' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "anchored 'is' search"
notmuch search tag:signed > EXPECTED
notmuch search --query=sexp '(is (rx ^si))' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "combine regexp mid and subject"
notmuch search subject:/-C/ and mid:/y..m/ | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp '(and (subject (rx -C)) (mid (rx y..m)))' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "regexp 'path' search"
notmuch search 'path:/^bar$/' | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp '(path (rx ^bar$))' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "regexp 'property' search"
notmuch search property:foo=bar > EXPECTED
notmuch search --query=sexp '(property (rx foo=.*))' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "anchored 'tag' search"
notmuch search tag:signed > EXPECTED
notmuch search --query=sexp '(tag (rx ^si))' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "regexp 'thread' search"
notmuch search --output=threads '*' | grep '7$' > EXPECTED
notmuch search --output=threads --query=sexp '(thread (rx 7$))' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Basic query that matches no messages"
count=$(notmuch count from:keithp and to:keithp)
test_expect_equal 0 "$count"

test_begin_subtest "Same query against threads"
notmuch search --query=sexp '(and (thread (of (from keithp))) (thread (matching (to keithp))))' \
    | notmuch_search_sanitize > OUTPUT
cat<<EOF > EXPECTED
thread:XXX   2009-11-18 [7/7] Lars Kellogg-Stedman, Mikhail Gusarov, Keith Packard, Carl Worth; [notmuch] Working with Maildir storage? (inbox signed unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Mix thread and non-threads query"
notmuch search --query=sexp '(and (thread (matching keithp)) (to keithp))' | notmuch_search_sanitize > OUTPUT
cat<<EOF > EXPECTED
thread:XXX   2009-11-18 [1/7] Lars Kellogg-Stedman| Mikhail Gusarov, Keith Packard, Carl Worth; [notmuch] Working with Maildir storage? (inbox signed unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Compound subquery"
notmuch search --query=sexp '(thread (of (from keithp) (subject Maildir)))' | notmuch_search_sanitize > OUTPUT
cat<<EOF > EXPECTED
thread:XXX   2009-11-18 [7/7] Lars Kellogg-Stedman, Mikhail Gusarov, Keith Packard, Carl Worth; [notmuch] Working with Maildir storage? (inbox signed unread)
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "empty subquery"
notmuch search --query=sexp '(thread (of))' 1>OUTPUT 2>&1
notmuch search '*' > EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "illegal expansion"
notmuch search --query=sexp '(id (of ego))' 1>OUTPUT 2>&1
cat<<EOF > EXPECTED
notmuch search: Syntax error in query
'of' unsupported inside 'id'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "(folder (of subquery))"
notmuch search --query=sexp --output=messages '(folder (of (id yun3a4cegoa.fsf@aiko.keithp.com)))' > OUTPUT
cat <<EOF > EXPECTED
id:yun1vjwegii.fsf@aiko.keithp.com
id:yun3a4cegoa.fsf@aiko.keithp.com
id:1258509400-32511-1-git-send-email-stewart@flamingspork.com
id:1258506353-20352-1-git-send-email-stewart@flamingspork.com
id:20091118010116.GC25380@dottiness.seas.harvard.edu
id:20091118005829.GB25380@dottiness.seas.harvard.edu
id:cf0c4d610911171136h1713aa59w9cf9aa31f052ad0a@mail.gmail.com
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "infix query"
notmuch search to:searchbyto | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp '(infix "to:searchbyto")' |  notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "bad infix query 1"
notmuch search --query=sexp '(infix "from:/unbalanced")' 2>&1|  notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
Syntax error in infix query: from:/unbalanced
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "bad infix query 2"
notmuch search --query=sexp '(infix "thread:{unbalanced")' 2>&1|  notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
Syntax error in infix query: thread:{unbalanced
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "bad infix query 3: bad nesting"
notmuch search --query=sexp '(subject (infix "tag:inbox"))' 2>&1|  notmuch_search_sanitize > OUTPUT
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
'infix' not supported inside 'subject'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "infix query that matches no messages"
notmuch search --query=sexp '(and (infix "from:keithp") (infix "to:keithp"))' > OUTPUT
test_expect_equal_file /dev/null OUTPUT

test_begin_subtest "compound infix query"
notmuch search date:2009-11-18..2009-11-18 and tag:unread > EXPECTED
notmuch search --query=sexp  '(infix "date:2009-11-18..2009-11-18 and tag:unread")' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "compound infix query 2"
notmuch search date:2009-11-18..2009-11-18 and tag:unread > EXPECTED
notmuch search --query=sexp  '(and (infix "date:2009-11-18..2009-11-18") (infix "tag:unread"))' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "date query, empty"
notmuch search from:keithp | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp  '(and (date) (from keithp))'| notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "date query, one argument"
notmuch search date:2009-11-18 and from:keithp | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp  '(and (date 2009-11-18) (from keithp))' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "date query, two arguments"
notmuch search date:2009-11-17..2009-11-18 and from:keithp | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp  '(and (date 2009-11-17 2009-11-18) (from keithp))' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "date query, illegal nesting 1"
notmuch search --query=sexp '(to (date))' > OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
nested field: 'date' inside 'to'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "date query, illegal nesting 2"
notmuch search --query=sexp '(to (date 2021-11-18))' > OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
nested field: 'date' inside 'to'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "date query, illegal nesting 3"
notmuch search --query=sexp '(date (to))' > OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
expected atom as first argument of 'date'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "date query, illegal nesting 4"
notmuch search --query=sexp '(date today (to))' > OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
expected atom as second argument of 'date'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "date query, too many arguments"
notmuch search --query=sexp '(date yesterday and tommorow)' > OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
'date' expects maximum of two arguments
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "date query, bad date"
notmuch search --query=sexp '(date "hawaiian-pizza-day")' > OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
Didn't understand date specification 'hawaiian-pizza-day'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "lastmod query, empty"
notmuch search from:keithp | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp  '(and (lastmod) (from keithp))'| notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "lastmod query, one argument"
notmuch tag +4EFC743A.3060609@april.org id:4EFC743A.3060609@april.org
revision=$(notmuch count --lastmod '*' | cut -f3)
notmuch search lastmod:$revision..$revision | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp  "(and (lastmod $revision))" | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "lastmod query, two arguments"
notmuch tag +keithp from:keithp
revision2=$(notmuch count --lastmod '*' | cut -f3)
notmuch search lastmod:$revision..$revision2 | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp  "(and (lastmod $revision $revision2))" | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "lastmod query, illegal nesting 1"
notmuch search --query=sexp '(to (lastmod))' > OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
nested field: 'lastmod' inside 'to'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "lastmod query, bad from revision"
notmuch search --query=sexp '(lastmod apples)' > OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
bad 'from' revision: 'apples'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "lastmod query, bad to revision"
notmuch search --query=sexp '(lastmod 0 apples)' > OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
bad 'to' revision: 'apples'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "lastmod query, illegal nesting 2"
notmuch search --query=sexp '(to (lastmod 2021-11-18))' > OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
nested field: 'lastmod' inside 'to'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "lastmod query, illegal nesting 3"
notmuch search --query=sexp '(lastmod (to))' > OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
expected atom as first argument of 'lastmod'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "lastmod query, illegal nesting 4"
notmuch search --query=sexp '(lastmod today (to))' > OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
expected atom as second argument of 'lastmod'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "lastmod query, too many arguments"
notmuch search --query=sexp '(lastmod yesterday and tommorow)' > OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
'lastmod' expects maximum of two arguments
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "user header (unknown header)"
notmuch search --query=sexp '(FooBar)' >& OUTPUT
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
unknown prefix 'FooBar'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "adding user header"
test_expect_code 0 "notmuch config set index.header.List \"List-Id\""

test_begin_subtest "reindexing"
test_expect_code 0 'notmuch reindex "*"'

test_begin_subtest "wildcard search for user header"
grep -Ril List-Id ${MAIL_DIR} | sort | notmuch_dir_sanitize > EXPECTED
notmuch search --output=files --query=sexp '(List *)' | sort | notmuch_dir_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "wildcard search for user header 2"
grep -Ril List-Id ${MAIL_DIR} | sort | notmuch_dir_sanitize > EXPECTED
notmuch search --output=files --query=sexp '(List (starts-with not))' | sort | notmuch_dir_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "search for user header"
notmuch search List:notmuch | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp '(List notmuch)' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "search for user header (list token)"
notmuch search List:notmuch | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp '(List notmuch.notmuchmail.org)' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "search for user header (quoted string)"
notmuch search 'List:"notmuch notmuchmail org"' | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp '(List "notmuch notmuchmail org")' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "search for user header (atoms)"
notmuch search 'List:"notmuch notmuchmail org"' | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp '(List notmuch notmuchmail org)' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "check saved query name"
test_expect_code 1 "notmuch config set squery.test '(subject utf8-sübjéct)'"

test_begin_subtest "roundtrip saved query (database)"
notmuch config set --database squery.Test '(subject utf8-sübjéct)'
output=$(notmuch config get squery.Test)
test_expect_equal "$output" '(subject utf8-sübjéct)'

test_begin_subtest "roundtrip saved query"
notmuch config set squery.Test '(subject override subject)'
output=$(notmuch config get squery.Test)
test_expect_equal "$output" '(subject override subject)'

test_begin_subtest "unknown saved query"
notmuch search --query=sexp '(Unknown foo bar)' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
unknown prefix 'Unknown'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "syntax error in saved query"
notmuch config set squery.Bad '(Bad'
notmuch search --query=sexp '(Bad foo bar)' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
invalid saved s-expression query: '(Bad'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Saved Search by 'tag' and 'subject'"
notmuch search tag:inbox and subject:maildir | notmuch_search_sanitize > EXPECTED
notmuch config set squery.TagSubject  '(and (tag inbox) (subject maildir))'
notmuch search --query=sexp '(TagSubject)' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Saved Search: illegal nesting"
notmuch config set squery.TagSubject  '(and (tag inbox) (subject maildir))'
notmuch search --query=sexp '(subject (TagSubject))' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
nested field: 'tag' inside 'subject'
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Saved Search: list as prefix"
notmuch config set squery.Bad2  '((and) (tag inbox) (subject maildir))'
notmuch search --query=sexp '(Bad2)' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
unexpected list in field/operation position
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Saved Search: bad parameter syntax"
notmuch config set squery.Bad3  '(macro a b)'
notmuch search --query=sexp '(Bad3)' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
missing (possibly empty) list of arguments to macro
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Saved Search: bad parameter syntax 2"
notmuch config set squery.Bad4  '(macro ((a b)) a)'
notmuch search --query=sexp '(Bad4 1)' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
macro parameters must be unquoted atoms
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Saved Search: bad parameter syntax 3"
notmuch config set squery.Bad5  '(macro (a b) a)'
notmuch search --query=sexp '(Bad5 1)' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
too few arguments to macro
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Saved Search: bad parameter syntax 4"
notmuch search --query=sexp '(Bad5 1 2 3)' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
too many arguments to macro
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Saved Search: macro without body"
notmuch config set squery.Bad3  '(macro (a b))'
notmuch search --query=sexp '(Bad3)' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
missing body of macro
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "macro in query"
notmuch search --query=sexp '(macro (a) (and ,b (subject maildir)))' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
macro definition not permitted here
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "zero argument macro"
notmuch search tag:inbox and subject:maildir | notmuch_search_sanitize > EXPECTED
notmuch config set squery.TagSubject0  '(macro () (and (tag inbox) (subject maildir)))'
notmuch search --query=sexp '(TagSubject0)' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "undefined argument"
notmuch search tag:inbox and subject:maildir | notmuch_search_sanitize > EXPECTED
notmuch config set squery.Bad6  '(macro (a) (and ,b (subject maildir)))'
notmuch search --query=sexp '(Bad6 foo)' >OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
undefined parameter b
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Single argument macro"
notmuch search tag:inbox and subject:maildir | notmuch_search_sanitize > EXPECTED
notmuch config set squery.TagSubject1  '(macro (tagname) (and (tag ,tagname) (subject maildir)))'
notmuch search --query=sexp '(TagSubject1 inbox)' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Single argument macro, list argument"
notmuch search tag:inbox and subject:maildir | notmuch_search_sanitize > EXPECTED
notmuch config set squery.ThingSubject  '(macro (thing) (and ,thing (subject maildir)))'
notmuch search --query=sexp '(ThingSubject (tag inbox))' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "two argument macro"
notmuch search tag:inbox and subject:maildir | notmuch_search_sanitize > EXPECTED
notmuch config set squery.TagSubject2  '(macro (tagname subj) (and (tag ,tagname) (subject ,subj)))'
notmuch search --query=sexp '(TagSubject2 inbox maildir)' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "nested macros (shadowing)"
notmuch search tag:inbox and subject:maildir | notmuch_search_sanitize > EXPECTED
notmuch config set squery.Inner '(macro (x) (subject ,x))'
notmuch config set squery.Outer  '(macro (x y) (and (tag ,x) (Inner ,y)))'
notmuch search --query=sexp '(Outer inbox maildir)' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "nested macros (no dynamic scope)"
notmuch config set squery.Inner2 '(macro (x) (subject ,y))'
notmuch config set squery.Outer2  '(macro (x y) (and (tag ,x) (Inner2 ,y)))'
notmuch search --query=sexp '(Outer2 inbox maildir)' > OUTPUT 2>&1
cat <<EOF > EXPECTED
notmuch search: Syntax error in query
undefined parameter y
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "combine macro and user defined header"
notmuch config set squery.About '(macro (name) (or (subject ,name) (List ,name)))'
notmuch search subject:notmuch or List:notmuch | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp '(About notmuch)' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT


test_begin_subtest "combine macro and user defined header"
notmuch config set squery.About '(macro (name) (or (subject ,name) (List ,name)))'
notmuch search subject:notmuch or List:notmuch | notmuch_search_sanitize > EXPECTED
notmuch search --query=sexp '(About notmuch)' | notmuch_search_sanitize > OUTPUT
test_expect_equal_file EXPECTED OUTPUT


test_done
