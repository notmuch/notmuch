#!/usr/bin/env bash
test_description="library reopen API"

. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus

cat <<EOF > c_head
#include <string.h>
#include <stdlib.h>
#include <notmuch-test.h>

int main (int argc, char** argv)
{
   notmuch_database_t *db;
   char *val;
   notmuch_status_t stat;
   notmuch_database_mode_t mode = NOTMUCH_DATABASE_MODE_READ_ONLY;

   char *msg = NULL;

   for (int i = 1; i < argc; i++)
      if (strcmp (argv[i], "%NULL%") == 0) argv[i] = NULL;

   if (argv[2] && (argv[2][0] == 'w' || argv[2][0] == 'W'))
     mode = NOTMUCH_DATABASE_MODE_READ_WRITE;

   stat = notmuch_database_open_with_config (argv[1],
					     mode,
					     argv[3],
					     argv[4],
					     &db,
					     &msg);
   if (stat != NOTMUCH_STATUS_SUCCESS) {
     fprintf (stderr, "error opening database: %d %s\n", stat, msg ? msg : "");
     exit (1);
   }
EOF

cat <<EOF > c_tail
   EXPECT0(notmuch_database_destroy(db));
}
EOF

# The sequence of tests is important here

test_begin_subtest "notmuch_database_reopen (read=>write)"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} read ${NOTMUCH_CONFIG}
{
   EXPECT0(notmuch_database_reopen (db, NOTMUCH_DATABASE_MODE_READ_WRITE));
   EXPECT0(notmuch_database_set_config (db, "test.key1", "testvalue1"));
   EXPECT0(notmuch_database_set_config (db, "test.key2", "testvalue2"));
   EXPECT0(notmuch_database_get_config (db, "test.key1", &val));
   printf("test.key1 = %s\n", val);
   EXPECT0(notmuch_database_get_config (db, "test.key2", &val));
   printf("test.key2 = %s\n", val);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 = testvalue1
test.key2 = testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch_database_reopen (read=>read)"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} read ${NOTMUCH_CONFIG}
{
   EXPECT0(notmuch_database_reopen (db, NOTMUCH_DATABASE_MODE_READ_ONLY));
   EXPECT0(notmuch_database_get_config (db, "test.key1", &val));
   printf("test.key1 = %s\n", val);
   EXPECT0(notmuch_database_get_config (db, "test.key2", &val));
   printf("test.key2 = %s\n", val);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 = testvalue1
test.key2 = testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch_database_reopen (write=>read)"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} write ${NOTMUCH_CONFIG}
{
   EXPECT0(notmuch_database_reopen (db, NOTMUCH_DATABASE_MODE_READ_ONLY));
   EXPECT0(notmuch_database_get_config (db, "test.key1", &val));
   printf("test.key1 = %s\n", val);
   EXPECT0(notmuch_database_get_config (db, "test.key2", &val));
   printf("test.key2 = %s\n", val);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 = testvalue1
test.key2 = testvalue2
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "notmuch_database_reopen (write=>write)"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} write ${NOTMUCH_CONFIG}
{
   EXPECT0(notmuch_database_reopen (db, NOTMUCH_DATABASE_MODE_READ_WRITE));
   EXPECT0(notmuch_database_set_config (db, "test.key3", "testvalue3"));
   EXPECT0(notmuch_database_get_config (db, "test.key1", &val));
   printf("test.key1 = %s\n", val);
   EXPECT0(notmuch_database_get_config (db, "test.key2", &val));
   printf("test.key2 = %s\n", val);
   EXPECT0(notmuch_database_get_config (db, "test.key3", &val));
   printf("test.key3 = %s\n", val);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
test.key1 = testvalue1
test.key2 = testvalue2
test.key3 = testvalue3
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
