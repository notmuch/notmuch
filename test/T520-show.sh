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

add_email_corpus duplicate

ID1=debian/2.6.1.dfsg-4-1-g87ea161@87ea161e851dfb1ea324af00e4ecfccc18875e15

test_begin_subtest "format json, --duplicate=2, duplicate key"
output=$(notmuch show --format=json --duplicate=2 id:${ID1})
test_json_nodes <<<"$output" "dup:['duplicate']=2"

test_begin_subtest "format json, subject, --duplicate=1"
output=$(notmuch show --format=json --duplicate=1 id:${ID1})
file=$(notmuch search --output=files id:${ID1} | head -n 1)
subject=$(sed -n 's/^Subject: \(.*\)$/\1/p' < $file)
test_json_nodes <<<"$output" "subject:['headers']['Subject']=\"$subject\""

test_begin_subtest "format json, subject, --duplicate=2"
output=$(notmuch show --format=json --duplicate=2 id:${ID1})
file=$(notmuch search --output=files id:${ID1} | tail -n 1)
subject=$(sed -n 's/^Subject: \(.*\)$/\1/p' < $file)
test_json_nodes <<<"$output" "subject:['headers']['Subject']=\"$subject\""

ID2=87r2geywh9.fsf@tethera.net
for dup in {1..2}; do
    test_begin_subtest "format json, body, --duplicate=${dup}"
    output=$(notmuch show --format=json --duplicate=${dup} id:${ID2} | \
	     $NOTMUCH_PYTHON -B "$NOTMUCH_SRCDIR"/test/json_check_nodes.py "body:['body'][0]['content']" | \
	     grep '^# body')
    test_expect_equal "$output" "# body ${dup}"
done

ID3=87r2ecrr6x.fsf@zephyr.silentflame.com
for dup in {1..5}; do
    test_begin_subtest "format json, --duplicate=${dup}, 'duplicate' key"
    output=$(notmuch show --format=json --duplicate=${dup} id:${ID3})
    test_json_nodes <<<"$output" "dup:['duplicate']=${dup}"
done

test_done
