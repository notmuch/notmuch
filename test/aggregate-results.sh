#!/usr/bin/env bash

set -eu

fixed=0
success=0
failed=0
broken=0
total=0
all_skipped=0
rep_failed=0

for file
do
	if [ ! -f "$file" ]; then
		echo "'$file' does not exist!"
		rep_failed=$((rep_failed + 1))
		continue
	fi
	has_total=0
	while read type value
	do
		case $type in
		fixed)
			fixed=$((fixed + value)) ;;
		success)
			success=$((success + value)) ;;
		failed)
			failed=$((failed + value)) ;;
		broken)
			broken=$((broken + value)) ;;
		total)
			total=$((total + value))
			has_total=1
			if [ "$value" -eq 0 ]; then
				all_skipped=$((all_skipped + 1))
			fi
		esac
	done <"$file"
	if [ "$has_total" -eq 0 ]; then
		echo "'$file' lacks 'total ...'; results may be inconsistent."
		failed=$((failed + 1))
	fi
done

pluralize_s () { [ "$1" -eq 1 ] && s='' || s='s'; }

echo "Notmuch test suite complete."

if [ "$fixed" -eq 0 ] && [ "$failed" -eq 0 ] && [ "$rep_failed" -eq 0 ]; then
	pluralize_s "$total"
	printf "All $total test$s "
	if [ "$broken" -eq 0 ]; then
		echo "passed."
	else
		pluralize_s "$broken"
		echo "behaved as expected ($broken expected failure$s)."
	fi
else
	echo "$success/$total tests passed."
	if [ "$broken" -ne 0 ]; then
		pluralize_s "$broken"
		echo "$broken broken test$s failed as expected."
	fi
	if [ "$fixed" -ne 0 ]; then
		pluralize_s "$fixed"
		echo "$fixed broken test$s now fixed."
	fi
	if [ "$failed" -ne 0 ]; then
		pluralize_s "$failed"
		echo "$failed test$s failed."
	fi
fi

skipped=$((total - fixed - success - failed - broken))
if [ "$skipped" -ne 0 ]; then
	pluralize_s "$skipped"
	echo "$skipped test$s skipped."
fi
if [ "$all_skipped" -ne 0 ]; then
	pluralize_s "$all_skipped"
	echo "All tests in $all_skipped file$s skipped."
fi

if [ "$rep_failed" -ne 0 ]; then
	pluralize_s "$rep_failed"
	echo "$rep_failed test$s failed to report results."
fi

# Note that we currently do not consider skipped tests as failing the
# build.

if [ "$success" -gt 0 ] && [ "$fixed" -eq 0 ] &&
	[ "$failed" -eq 0 ] && [ "$rep_failed" -eq 0 ]
then
	exit 0
else
	exit 1
fi
