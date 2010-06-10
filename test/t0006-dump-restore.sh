#!/bin/bash
test_description="\"notmuch dump\" and \"notmuch restore\""
. ./test-lib.sh
test_expect_success 'Generate some message' '
generate_message &&
notmuch new
'
test_expect_success "Dumping all tags" '
$NOTMUCH dump dump.expected &&
pass_if_equal "$?" "0"

'
test_expect_success "Clearing all tags" '
sed -e "s/(\([^(]*\))$/()/" < dump.expected > clear.expected
$NOTMUCH restore clear.expected &&
$NOTMUCH dump clear.actual &&
pass_if_equal "$(< clear.actual)" "$(< clear.expected)"

'
test_expect_success "Restoring original tags" '
$NOTMUCH restore dump.expected &&
$NOTMUCH dump dump.actual &&
pass_if_equal "$(< dump.actual)" "$(< dump.expected)"

'
test_expect_success "Restore with nothing to do" '
$NOTMUCH restore dump.expected &&
pass_if_equal "$?" "0"
'
test_done
