#!/usr/bin/env bash
test_description="API tests for notmuch_thread_*"

. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus

test_begin_subtest "building database"
test_expect_success "NOTMUCH_NEW"

cat <<'EOF' > c_tail
   if (stat) {
       const char *stat_str = notmuch_database_status_string (db);
       if (stat_str)
           fputs (stat_str, stderr);
    }

}
EOF

cat <<EOF > c_head
#include <stdio.h>
#include <notmuch.h>
#include <notmuch-test.h>
int main (int argc, char** argv)
{
   notmuch_database_t *db;
   notmuch_status_t stat;
   char *msg = NULL;
   notmuch_thread_t *thread = NULL;
   notmuch_threads_t *threads = NULL;
   notmuch_query_t *query = NULL;
   const char *id = "thread:0000000000000009";

   stat = notmuch_database_open_verbose (argv[1], NOTMUCH_DATABASE_MODE_READ_WRITE, &db, &msg);
   if (stat != NOTMUCH_STATUS_SUCCESS) {
     fprintf (stderr, "error opening database: %d %s\n", stat, msg ? msg : "");
     exit (1);
   }

   query = notmuch_query_create (db, id);
   EXPECT0(notmuch_query_search_threads (query, &threads));
   thread = notmuch_threads_get (threads);
   EXPECT0(notmuch_database_close (db));
EOF

test_begin_subtest "get thread-id from closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        const char *id2;
        id2 = notmuch_thread_get_thread_id (thread);
        printf("%d\n%s\n", thread != NULL, id2);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
0000000000000009
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
