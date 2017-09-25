#!/usr/bin/env bash
test_description="argument parsing"
. $(dirname "$0")/test-lib.sh || exit 1

test_begin_subtest "sanity check"
$TEST_DIRECTORY/arg-test  pos1  --keyword=one --boolean --string=foo pos2 --int=7 --flag=one --flag=three > OUTPUT
cat <<EOF > EXPECTED
boolean 1
keyword 1
flags 5
int 7
string foo
positional arg 1 pos1
positional arg 2 pos2
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "sanity check zero values"
$TEST_DIRECTORY/arg-test --keyword=zero --boolean=false --int=0 > OUTPUT
cat <<EOF > EXPECTED
boolean 0
keyword 0
int 0
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "space instead of = between parameter name and value"
# Note: spaces aren't allowed for booleans. false turns into a positional arg!
$TEST_DIRECTORY/arg-test --keyword one --boolean false --string foo --int 7 --flag one --flag three > OUTPUT
cat <<EOF > EXPECTED
boolean 1
keyword 1
flags 5
int 7
string foo
positional arg 1 false
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
