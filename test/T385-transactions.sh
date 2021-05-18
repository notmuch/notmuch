#!/usr/bin/env bash
test_description='transactions'
. $(dirname "$0")/test-lib.sh || exit 1

make_shim no-close <<EOF
#include <notmuch.h>
#include <stdio.h>
notmuch_status_t
notmuch_database_close (notmuch_database_t *notmuch)
{
  return notmuch_database_begin_atomic (notmuch);
}
EOF

for i in `seq 1 1024`
do
    generate_message '[subject]="'"subject $i"'"' \
	             '[body]="'"body $i"'"'
done

test_begin_subtest "initial new"
NOTMUCH_NEW > OUTPUT
cat <<EOF > EXPECTED
Added 1024 new messages to the database.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Some changes saved with open transaction"
notmuch config set database.autocommit 1000
rm -r ${MAIL_DIR}/.notmuch
notmuch_with_shim no-close new
output=$(notmuch count '*')
test_expect_equal "$output" "1000"

test_done
