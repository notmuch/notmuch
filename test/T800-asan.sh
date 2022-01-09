#!/usr/bin/env bash
test_description='run code with ASAN enabled against the library'
. $(dirname "$0")/test-lib.sh || exit 1

if [ $NOTMUCH_HAVE_ASAN -ne 1 ]; then
    printf "Skipping due to missing ASAN support\n"
    test_done
fi

add_email_corpus

TEST_CFLAGS="-fsanitize=address"

test_begin_subtest "open and destroy"
test_C ${MAIL_DIR} ${NOTMUCH_CONFIG} <<EOF
#include <notmuch.h>
#include <stdio.h>

int main(int argc, char **argv) {
  notmuch_database_t *db = NULL;

  notmuch_status_t st = notmuch_database_open_with_config(argv[1],
                                                          NOTMUCH_DATABASE_MODE_READ_ONLY,
                                                          argv[2], NULL, &db, NULL);

  printf("db != NULL: %d\n", db != NULL);
  if (db != NULL)
    notmuch_database_destroy(db);
  return 0;
}
EOF
cat <<EOF > EXPECTED
== stdout ==
db != NULL: 1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
