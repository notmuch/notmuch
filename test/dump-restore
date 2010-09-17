#!/bin/bash
test_description="\"notmuch dump\" and \"notmuch restore\""
. ./test-lib.sh

test_expect_success "Dumping all tags" "generate_message &&
notmuch new &&
$NOTMUCH dump dump.expected"

test_begin_subtest "Clearing all tags"
sed -e "s/(\([^(]*\))$/()/" < dump.expected > clear.expected
$NOTMUCH restore clear.expected 
$NOTMUCH dump clear.actual
test_expect_equal "$(< clear.actual)" "$(< clear.expected)"

test_begin_subtest "Restoring original tags"
$NOTMUCH restore dump.expected
$NOTMUCH dump dump.actual
test_expect_equal "$(< dump.actual)" "$(< dump.expected)"

test_expect_success "Restore with nothing to do" "$NOTMUCH restore dump.expected"

test_done
