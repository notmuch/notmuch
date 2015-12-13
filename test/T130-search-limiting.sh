#!/usr/bin/env bash
test_description='"notmuch search" --offset and --limit parameters'
. ./test-lib.sh || exit 1

add_email_corpus

for outp in messages threads; do
    test_begin_subtest "${outp}: limit does the right thing"
    notmuch search --output=${outp} "*" | head -n 20 >expected
    notmuch search --output=${outp} --limit=20 "*" >output
    test_expect_equal_file expected output

    test_begin_subtest "${outp}: concatenation of limited searches"
    notmuch search --output=${outp} "*" | head -n 20 >expected
    notmuch search --output=${outp} --limit=10 "*" >output
    notmuch search --output=${outp} --limit=10 --offset=10 "*" >>output
    test_expect_equal_file expected output

    test_begin_subtest "${outp}: limit larger than result set"
    N=`notmuch count --output=${outp} "*"`
    notmuch search --output=${outp} "*" >expected
    notmuch search --output=${outp} --limit=$((1 + ${N})) "*" >output
    test_expect_equal_file expected output

    test_begin_subtest "${outp}: limit = 0"
    test_expect_equal "`notmuch search --output=${outp} --limit=0 "*"`" ""

    test_begin_subtest "${outp}: offset does the right thing"
    # note: tail -n +N is 1-based
    notmuch search --output=${outp} "*" | tail -n +21 >expected
    notmuch search --output=${outp} --offset=20 "*" >output
    test_expect_equal_file expected output

    test_begin_subtest "${outp}: offset = 0"
    notmuch search --output=${outp} "*" >expected
    notmuch search --output=${outp} --offset=0 "*" >output
    test_expect_equal_file expected output

    test_begin_subtest "${outp}: negative offset"
    notmuch search --output=${outp} "*" | tail -n 20 >expected
    notmuch search --output=${outp} --offset=-20 "*" >output
    test_expect_equal_file expected output

    test_begin_subtest "${outp}: negative offset"
    notmuch search --output=${outp} "*" | tail -n 1 >expected
    notmuch search --output=${outp} --offset=-1 "*" >output
    test_expect_equal_file expected output

    test_begin_subtest "${outp}: negative offset combined with limit"
    notmuch search --output=${outp} "*" | tail -n 20 | head -n 10 >expected
    notmuch search --output=${outp} --offset=-20 --limit=10 "*" >output
    test_expect_equal_file expected output

    test_begin_subtest "${outp}: negative offset combined with equal limit"
    notmuch search --output=${outp} "*" | tail -n 20 >expected
    notmuch search --output=${outp} --offset=-20 --limit=20 "*" >output
    test_expect_equal_file expected output

    test_begin_subtest "${outp}: negative offset combined with large limit"
    notmuch search --output=${outp} "*" | tail -n 20 >expected
    notmuch search --output=${outp} --offset=-20 --limit=50 "*" >output
    test_expect_equal_file expected output

    test_begin_subtest "${outp}: negative offset larger then results"
    N=`notmuch count --output=${outp} "*"`
    notmuch search --output=${outp} "*" >expected
    notmuch search --output=${outp} --offset=-$((1 + ${N})) "*" >output
    test_expect_equal_file expected output
done

test_done
