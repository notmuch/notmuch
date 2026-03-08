#!/usr/bin/env bash
test_description='extended wildcard support in Xapian query parser'
. $(dirname "$0")/test-lib.sh || exit 1

if [ "${NOTMUCH_HAVE_XAPIAN_WILDCARD_GLOB-0}" != "1" ]; then
    printf "Skipping due to missing extended wildcard support\n"
    test_done
fi

add_email_corpus

count=0
for field in "any" 'body' 'from' 'subject' 'to' ; do
    for glob in 'm?tch' '?a?ch' 'm?tc?' '*atch' 'mat*' '*at*'; do
	test_begin_subtest "field $field, glob $glob"
	case $field in
	    from|subject)
		test_subtest_known_broken
		;;
	esac
	count=$((count + 1))
	cookie="match$count"
	reversecookie="${count}match"
	queryglob="$glob$count"
	if [ $field == 'any' ]; then
	    msgfield='body'
	    queryprefix=''
	else
	    msgfield=$field
	    queryprefix="$field:"
	fi
	reversequeryglob="$count$glob"
	add_message "[$msgfield]=$cookie"
	add_message "[$msgfield]=$reversecookie"
	output=$(notmuch count "$queryprefix$queryglob")
	output="$output $(notmuch count "$queryprefix$reversequeryglob")"
	test_expect_equal "$output" "1 1"
    done
done
test_done
