#!/usr/bin/env bash
test_description="argument parsing"
. ./test-lib.sh

test_begin_subtest "sanity check"
$TEST_DIRECTORY/arg-test  pos1  --keyword=one --string=foo pos2 --int=7 > OUTPUT
cat <<EOF > EXPECTED
keyword 1
int 7
string foo
positional arg 1 pos1
positional arg 2 pos2
EOF
test_expect_equal_file OUTPUT EXPECTED

test_done
