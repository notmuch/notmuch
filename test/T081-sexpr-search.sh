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

test_begin_subtest "Search by 'subject' (utf-8, phrase-token):"
test_subtest_known_broken
output=$(notmuch search --query=sexp '(subject utf8-sübjéct)' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-sübjéct (inbox unread)"

test_begin_subtest "Search by 'subject' (utf-8, quoted string):"
test_subtest_known_broken
output=$(notmuch search --query=sexp '(subject "utf8 sübjéct")' | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; utf8-sübjéct (inbox unread)"

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

test_done
