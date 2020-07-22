#!/usr/bin/env bash
test_description="notmuch_query_* API"

. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus

test_begin_subtest "building database"
test_expect_success "NOTMUCH_NEW"

cat <<EOF > c_head
#include <stdio.h>
#include <notmuch.h>
#include <notmuch-test.h>
#include <talloc.h>
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

test_begin_subtest "roundtrip query string with closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_query_t *query;
        const char *str = "id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net";
        const char *ret;

        EXPECT0(notmuch_database_close (db));
        query = notmuch_query_create (db, str);
        ret = notmuch_query_get_query_string (query);

        printf("%s\n%s\n", str, ret);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net
id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "retrieve closed db from query"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_query_t *query;
        const char *str = "id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net";
        notmuch_database_t *db2;

        query = notmuch_query_create (db, str);
        EXPECT0(notmuch_database_close (db));
        db2 = notmuch_query_get_database (query);

        printf("%d\n", db == db2);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
