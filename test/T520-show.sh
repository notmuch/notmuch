#!/usr/bin/env bash
test_description='"notmuch show"'

. $(dirname "$0")/test-lib.sh || exit 1

test_query_syntax () {
    test_begin_subtest "sexpr query: $1"
    sexp=$(notmuch show --format=json --query=sexp "$1")
    infix=$(notmuch show --format=json "$2")
    test_expect_equal_json "$sexp" "$infix"
}

add_email_corpus

test_begin_subtest "exit code for show invalid query"
notmuch show foo..
exit_code=$?
test_expect_equal 1 $exit_code

test_begin_subtest "notmuch show --sort=newest-first"
notmuch show --entire-thread=true '*' > EXPECTED
notmuch show --entire-thread=true --sort=newest-first '*' > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch show --sort=oldest-first"
notmuch show --entire-thread=true '*' | grep ^depth:0 > EXPECTED
notmuch show --entire-thread=true --sort=oldest-first '*' | grep ^depth:0 > OLDEST
perl -e 'print reverse<>' OLDEST > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch show --sort for single thread"
QUERY="id:yun1vjwegii.fsf@aiko.keithp.com"
notmuch show --entire-thread=true --sort=newest-first $QUERY > EXPECTED
notmuch show --entire-thread=true --sort=oldest-first $QUERY > OUTPUT
test_expect_equal_file EXPECTED OUTPUT


if [ $NOTMUCH_HAVE_SFSEXP -eq 1 ]; then

    test_query_syntax '(and "wonderful" "wizard")' 'wonderful and wizard'
    test_query_syntax '(or "php" "wizard")' 'php or wizard'
    test_query_syntax 'wizard' 'wizard'
    test_query_syntax 'Wizard' 'Wizard'
    test_query_syntax '(attachment notmuch-help.patch)' 'attachment:notmuch-help.patch'

fi

test_done
