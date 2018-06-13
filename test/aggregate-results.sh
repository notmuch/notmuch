#!/usr/bin/env bash

set -eu

fixed=0
success=0
failed=0
broken=0
total=0

for file
do
	while read type value
	do
		case $type in
		'')
			continue ;;
		fixed)
			fixed=$(($fixed + $value)) ;;
		success)
			success=$(($success + $value)) ;;
		failed)
			failed=$(($failed + $value)) ;;
		broken)
			broken=$(($broken + $value)) ;;
		total)
			total=$(($total + $value)) ;;
		esac
	done <"$file"
done

pluralize () {
    case $2 in
	1)
	    case $1 in
		test)
		    echo test ;;
		failure)
		    echo failure ;;
	    esac
	    ;;
	*)
	    case $1 in
		test)
		    echo tests ;;
		failure)
		    echo failures ;;
	    esac
	    ;;
    esac
}

echo "Notmuch test suite complete."
if [ "$fixed" = "0" ] && [ "$failed" = "0" ]; then
    tests=$(pluralize "test" $total)
    printf "All $total $tests "
    if [ "$broken" = "0" ]; then
	echo "passed."
    else
	failures=$(pluralize "failure" $broken)
	echo "behaved as expected ($broken expected $failures)."
    fi;
else
    echo "$success/$total tests passed."
    if [ "$broken" != "0" ]; then
	tests=$(pluralize "test" $broken)
	echo "$broken broken $tests failed as expected."
    fi
    if [ "$fixed" != "0" ]; then
	tests=$(pluralize "test" $fixed)
	echo "$fixed broken $tests now fixed."
    fi
    if [ "$failed" != "0" ]; then
	tests=$(pluralize "test" $failed)
	echo "$failed $tests failed."
    fi
fi

skipped=$(($total - $fixed - $success - $failed - $broken))
if [ "$skipped" != "0" ]; then
    tests=$(pluralize "test" $skipped)
    echo "$skipped $tests skipped."
fi

# Note that we currently do not consider skipped tests as failing the
# build.

if [ $success -gt 0 -a $fixed -eq 0 -a $failed -eq 0 ]
then
    exit 0
else
    exit 1
fi
