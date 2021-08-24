#!/usr/bin/env bash
test_description='"notmuch search" in several variations'
. $(dirname "$0")/test-lib.sh || exit 1

if [ $NOTMUCH_HAVE_SFSEXP -ne 1 ]; then
    printf "Skipping due to missing sfsexp library\n"
    test_done
fi

add_email_corpus

test_begin_subtest "all messages: ()"
notmuch search '*' > EXPECTED
notmuch search --query=sexp "()" > OUTPUT
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

test_done
