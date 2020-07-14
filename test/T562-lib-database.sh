#!/usr/bin/env bash
test_description="notmuch_database_* API"

. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus

test_begin_subtest "building database"
test_expect_success "NOTMUCH_NEW"

cat <<EOF > c_head
#include <stdio.h>
#include <notmuch.h>
#include <notmuch-test.h>
int main (int argc, char** argv)
{
   notmuch_database_t *db;
   notmuch_status_t stat;
   char *msg = NULL;

   stat = notmuch_database_open_verbose (argv[1], NOTMUCH_DATABASE_MODE_READ_WRITE, &db, &msg);
   if (stat != NOTMUCH_STATUS_SUCCESS) {
     fprintf (stderr, "error opening database: %d %s\n", stat, msg ? msg : "");
     exit (1);
   }
EOF

cat <<'EOF' > c_tail
   if (stat) {
       const char *stat_str = notmuch_database_status_string (db);
       if (stat_str)
           fputs (stat_str, stderr);
    }

}
EOF

test_begin_subtest "get status_string with closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        const char *str;
        EXPECT0(notmuch_database_close (db));
        str = notmuch_database_status_string (db);
        printf("%d\n", str == NULL);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "get path with closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        const char *path;
        EXPECT0(notmuch_database_close (db));
        path = notmuch_database_get_path (db);
        printf("%s\n", path);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
MAIL_DIR
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "get version with closed db"
test_subtest_known_broken
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        unsigned int version;
        EXPECT0(notmuch_database_close (db));
        version = notmuch_database_get_version (db);
        printf ("%u\n", version);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
0
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
