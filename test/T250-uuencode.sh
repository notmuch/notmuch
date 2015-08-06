#!/usr/bin/env bash
test_description="handling of uuencoded data"
. ./test-lib.sh || exit 1

add_message [subject]=uuencodetest '[date]="Sat, 01 Jan 2000 12:00:00 -0000"' \
'[body]="This message is used to ensure that notmuch correctly handles a
message containing a block of uuencoded data. First, we have a marker
this content beforeuudata . Then we begin the uuencoded data itself:

begin 644 bogus-uuencoded-data
M0123456789012345678901234567890123456789012345678901234567890
MOBVIOUSLY, THIS IS NOT ANY SORT OF USEFUL UUENCODED DATA.    
MINSTEAD THIS IS JUST A WAY TO ENSURE THAT THIS BLOCK OF DATA 
MIS CORRECTLY IGNORED WHEN NOTMUCH CREATES ITS INDEX. SO WE   
MINCLUDE A DURINGUUDATA MARKER THAT SHOULD NOT RESULT IN ANY  
MSEARCH RESULT.                                               
\\\`
end

Finally, we have our afteruudata marker as well."'

test_begin_subtest "Ensure content before uu data is indexed"
output=$(notmuch search beforeuudata | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; uuencodetest (inbox unread)"

test_begin_subtest "Ensure uu data is not indexed"
output=$(notmuch search DURINGUUDATA | notmuch_search_sanitize)
test_expect_equal "$output" ""

test_begin_subtest "Ensure content after uu data is indexed"
output=$(notmuch search afteruudata | notmuch_search_sanitize)
test_expect_equal "$output" "thread:XXX   2000-01-01 [1/1] Notmuch Test Suite; uuencodetest (inbox unread)"

test_done
