#!/usr/bin/env bash
test_description="hex encoding and decoding"
. ./test-lib.sh || exit 1

test_begin_subtest "round trip"
find $TEST_DIRECTORY/corpus -type f -print | sort | xargs cat > EXPECTED
$TEST_DIRECTORY/hex-xcode --direction=encode < EXPECTED | $TEST_DIRECTORY/hex-xcode --direction=decode > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "punctuation"
tag1='comic_swear=$&^%$^%\\//-+$^%$'
tag_enc1=$($TEST_DIRECTORY/hex-xcode --direction=encode "$tag1")
test_expect_equal "$tag_enc1" "comic_swear=%24%26%5e%25%24%5e%25%5c%5c%2f%2f-+%24%5e%25%24"

test_begin_subtest "round trip newlines"
printf 'this\n tag\t has\n spaces\n' > EXPECTED.$test_count
$TEST_DIRECTORY/hex-xcode --direction=encode  < EXPECTED.$test_count |\
	$TEST_DIRECTORY/hex-xcode --direction=decode > OUTPUT.$test_count
test_expect_equal_file EXPECTED.$test_count OUTPUT.$test_count

test_begin_subtest "round trip 8bit chars"
echo '%c3%91%c3%a5%c3%b0%c3%a3%c3%a5%c3%a9-%c3%8f%c3%8a' > EXPECTED.$test_count
$TEST_DIRECTORY/hex-xcode --direction=decode  < EXPECTED.$test_count |\
    $TEST_DIRECTORY/hex-xcode --direction=encode > OUTPUT.$test_count
test_expect_equal_file EXPECTED.$test_count OUTPUT.$test_count

test_begin_subtest "round trip (in-place)"
find $TEST_DIRECTORY/corpus -type f -print | sort | xargs cat > EXPECTED
$TEST_DIRECTORY/hex-xcode --in-place --direction=encode < EXPECTED |\
     $TEST_DIRECTORY/hex-xcode --in-place --direction=decode > OUTPUT
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "punctuation (in-place)"
tag1='comic_swear=$&^%$^%\\//-+$^%$'
tag_enc1=$($TEST_DIRECTORY/hex-xcode --in-place --direction=encode "$tag1")
test_expect_equal "$tag_enc1" "comic_swear=%24%26%5e%25%24%5e%25%5c%5c%2f%2f-+%24%5e%25%24"

test_begin_subtest "round trip newlines (in-place)"
printf 'this\n tag\t has\n spaces\n' > EXPECTED.$test_count
$TEST_DIRECTORY/hex-xcode --in-place --direction=encode  < EXPECTED.$test_count |\
    $TEST_DIRECTORY/hex-xcode --in-place --direction=decode > OUTPUT.$test_count
test_expect_equal_file EXPECTED.$test_count OUTPUT.$test_count

test_begin_subtest "round trip 8bit chars (in-place)"
echo '%c3%91%c3%a5%c3%b0%c3%a3%c3%a5%c3%a9-%c3%8f%c3%8a' > EXPECTED.$test_count
$TEST_DIRECTORY/hex-xcode --in-place --direction=decode  < EXPECTED.$test_count |\
    $TEST_DIRECTORY/hex-xcode --in-place --direction=encode > OUTPUT.$test_count
test_expect_equal_file EXPECTED.$test_count OUTPUT.$test_count

test_done
