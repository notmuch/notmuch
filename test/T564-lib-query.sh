#!/usr/bin/env bash
test_description="notmuch_query_* API"

. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus

test_begin_subtest "building database"
test_expect_success "NOTMUCH_NEW"

cat <<EOF > c_head
#include <notmuch-test.h>

int main (int argc, char** argv)
{
   notmuch_database_t *db;
   notmuch_status_t stat;
   char *msg = NULL;

   stat = notmuch_database_open_with_config (argv[1],
					     NOTMUCH_DATABASE_MODE_READ_WRITE,
					     NULL, NULL, &db, &msg);
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

test_begin_subtest "set omit_excluded on closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_query_t *query;
        const char *str = "id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net";

        query = notmuch_query_create (db, str);
        EXPECT0(notmuch_database_close (db));
        notmuch_query_set_omit_excluded (query, NOTMUCH_EXCLUDE_ALL);

        printf("SUCCESS\n");
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
SUCCESS
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "roundtrip sort on closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_query_t *query;
        const char *str = "id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net";
        notmuch_sort_t sort;

        query = notmuch_query_create (db, str);
        EXPECT0(notmuch_database_close (db));
        notmuch_query_set_sort (query, NOTMUCH_SORT_UNSORTED);
        sort = notmuch_query_get_sort (query);
        printf("%d\n", sort == NOTMUCH_SORT_UNSORTED);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "add tag_exclude on closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_query_t *query;
        const char *str = "id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net";

        query = notmuch_query_create (db, str);
        EXPECT0(notmuch_database_close (db));
        stat = notmuch_query_add_tag_exclude (query, "spam");
        printf("%d\n", stat == NOTMUCH_STATUS_SUCCESS);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "search threads on closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_query_t *query;
        const char *str = "id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net";
        notmuch_threads_t *threads;

        query = notmuch_query_create (db, str);
        EXPECT0(notmuch_database_close (db));
        stat = notmuch_query_search_threads (query, &threads);

        printf("%d\n", stat == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
A Xapian exception occurred performing query: Database has been closed
Query string was: id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "search messages on closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_query_t *query;
        const char *str = "id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net";
        notmuch_messages_t *messages;

        query = notmuch_query_create (db, str);
        EXPECT0(notmuch_database_close (db));
        stat = notmuch_query_search_messages (query, &messages);

        printf("%d\n", stat == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
A Xapian exception occurred performing query: Database has been closed
Query string was: id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "count messages on closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_query_t *query;
        const char *str = "id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net";
        unsigned int count;

        query = notmuch_query_create (db, str);
        EXPECT0(notmuch_database_close (db));
        stat = notmuch_query_count_messages (query, &count);

        printf("%d\n", stat == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
A Xapian exception occurred performing query: Database has been closed
Query string was: id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "count threads on closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_query_t *query;
        const char *str = "id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net";
        unsigned int count;

        query = notmuch_query_create (db, str);
        EXPECT0(notmuch_database_close (db));
        stat = notmuch_query_count_threads (query, &count);

        printf("%d\n", stat == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
A Xapian exception occurred performing query: Database has been closed
Query string was: id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "destroy query with closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_query_t *query;
        const char *str = "id:1258471718-6781-1-git-send-email-dottedmag@dottedmag.net";

        query = notmuch_query_create (db, str);
        EXPECT0(notmuch_database_close (db));
        notmuch_query_destroy (query);

        printf("SUCCESS\n");
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
SUCCESS
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
