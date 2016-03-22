#!/usr/bin/env bash
test_description="library config API"

. ./test-lib.sh || exit 1

add_email_corpus

cat <<EOF > c_head
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <notmuch.h>

void run(int line, notmuch_status_t ret)
{
   if (ret) {
	fprintf (stderr, "line %d: %s\n", line, ret);
	exit (1);
   }
}

#define RUN(v)  run(__LINE__, v);

int main (int argc, char** argv)
{
   notmuch_database_t *db;
   char *val;
   notmuch_status_t stat;

   RUN(notmuch_database_open (argv[1], NOTMUCH_DATABASE_MODE_READ_WRITE, &db));

EOF

cat <<EOF > c_tail
   RUN(notmuch_database_destroy(db));
}
EOF

test_begin_subtest "notmuch_database_{set,get}_config"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
{
   RUN(notmuch_database_set_config (db, "testkey1", "testvalue1"));
   RUN(notmuch_database_set_config (db, "testkey2", "testvalue2"));
   RUN(notmuch_database_get_config (db, "testkey1", &val));
   printf("testkey1 = %s\n", val);
   RUN(notmuch_database_get_config (db, "testkey2", &val));
   printf("testkey2 = %s\n", val);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
testkey1 = testvalue1
testkey2 = testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
