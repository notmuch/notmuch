#!/usr/bin/env bash
test_description='"notmuch show" --offset and --limit parameters'
. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus

show () {
    local kind="$1"
    shift
    if [ "$kind" = messages ]; then
        set -- --unthreaded "$@"
    fi
    notmuch show --body=false --format=text --entire-thread=false "$@" "*" |
        sed -nre 's/^.message\{.*\<depth:0\>.*/&/p'
}

for outp in messages threads; do
    test_begin_subtest "$outp: limit does the right thing"
    show $outp | head -n 20 >expected
    show $outp --limit=20 >output
    test_expect_equal_file expected output

    test_begin_subtest "$outp: concatenation of limited shows"
    show $outp | head -n 20 >expected
    show $outp --limit=10 >output
    show $outp --limit=10 --offset=10 >>output
    test_expect_equal_file expected output

    test_begin_subtest "$outp: limit larger than result set"
    N=$(notmuch count --output=$outp "*")
    show $outp >expected
    show $outp --limit=$((1 + N)) >output
    test_expect_equal_file expected output

    test_begin_subtest "$outp: limit = 0"
    test_expect_equal "$(show $outp --limit=0)" ""

    test_begin_subtest "$outp: offset does the right thing"
    # note: tail -n +N is 1-based
    show $outp | tail -n +21 >expected
    show $outp --offset=20 >output
    test_expect_equal_file expected output

    test_begin_subtest "$outp: offset = 0"
    show $outp >expected
    show $outp --offset=0 >output
    test_expect_equal_file expected output

    test_begin_subtest "$outp: negative offset"
    show $outp | tail -n 20 >expected
    show $outp --offset=-20 >output
    test_expect_equal_file expected output

    test_begin_subtest "$outp: negative offset"
    show $outp | tail -n 1 >expected
    show $outp --offset=-1 >output
    test_expect_equal_file expected output

    test_begin_subtest "$outp: negative offset combined with limit"
    show $outp | tail -n 20 | head -n 10 >expected
    show $outp --offset=-20 --limit=10 >output
    test_expect_equal_file expected output

    test_begin_subtest "$outp: negative offset combined with equal limit"
    show $outp | tail -n 20 >expected
    show $outp --offset=-20 --limit=20 >output
    test_expect_equal_file expected output

    test_begin_subtest "$outp: negative offset combined with large limit"
    show $outp | tail -n 20 >expected
    show $outp --offset=-20 --limit=50 >output
    test_expect_equal_file expected output

    test_begin_subtest "$outp: negative offset larger than results"
    N=$(notmuch count --output=$outp "*")
    show $outp >expected
    show $outp --offset=-$((1 + N)) >output
    test_expect_equal_file expected output
done

test_done
