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
#include <talloc.h>
int main (int argc, char** argv)
{
   notmuch_database_t *db;
   notmuch_status_t stat = NOTMUCH_STATUS_SUCCESS;
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        unsigned int version;
        EXPECT0(notmuch_database_close (db));
        version = notmuch_database_get_version (db);
        printf ("%u\n", version);
        stat = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
0
== stderr ==
A Xapian exception occurred at lib/database.cc:XXX: Database has been closed
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "re-close a closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        EXPECT0(notmuch_database_close (db));
        stat = notmuch_database_close (db);
        printf ("%d\n", stat);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
0
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "destroy a closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        unsigned int version;
        EXPECT0(notmuch_database_close (db));
        stat = notmuch_database_destroy (db);
        printf ("%d\n", stat);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
0
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "destroy an open db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        unsigned int version;
        stat = notmuch_database_destroy (db);
        printf ("%d\n", stat);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
0
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "check a closed db for upgrade"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_bool_t ret;

        EXPECT0(notmuch_database_close (db));
        ret = notmuch_database_needs_upgrade (db);
        printf ("%d\n", ret == FALSE);
        stat = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
A Xapian exception occurred at lib/database.cc:XXX: Database has been closed
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "upgrade a closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_bool_t ret;

        EXPECT0(notmuch_database_close (db));
        stat = notmuch_database_upgrade (db, NULL, NULL);
        printf ("%d\n", ret == NOTMUCH_STATUS_SUCCESS);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "begin atomic section for a closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        EXPECT0(notmuch_database_close (db));
        stat = notmuch_database_begin_atomic (db);
        printf ("%d\n", stat == NOTMUCH_STATUS_SUCCESS ||
                        stat == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
        stat = NOTMUCH_STATUS_SUCCESS;
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "end atomic section for a closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        EXPECT0(notmuch_database_close (db));
        EXPECT0(notmuch_database_begin_atomic (db));
        stat = notmuch_database_end_atomic (db);
        printf ("%d\n", stat == NOTMUCH_STATUS_SUCCESS ||
                        stat == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
        stat = NOTMUCH_STATUS_SUCCESS;
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "get revision for a closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        const char *uuid;
        unsigned long rev;

        EXPECT0(notmuch_database_close (db));
        rev = notmuch_database_get_revision (db, &uuid);
        printf ("%d\n", rev, uuid);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
53
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "get directory for a closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_directory_t *dir;
        EXPECT0(notmuch_database_close (db));
        stat = notmuch_database_get_directory (db, "/nonexistent", &dir);
        printf ("%d\n", stat == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
A Xapian exception occurred creating a directory: Database has been closed.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "index file with a closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_message_t *msg;
        const char *path = talloc_asprintf(db, "%s/01:2,", argv[1]);
        EXPECT0(notmuch_database_close (db));
        stat = notmuch_database_index_file (db, path, NULL, &msg);
        printf ("%d\n", stat == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
A Xapian exception occurred finding message: Database has been closed.
EOF
test_expect_equal_file EXPECTED OUTPUT

generate_message '[filename]=relative_path'
test_begin_subtest "index file (relative path)"
test_subtest_known_broken
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_message_t *msg;
        stat = notmuch_database_index_file (db, "relative_path", NULL, &msg);
        printf ("%d\n", stat == NOTMUCH_STATUS_SUCCESS);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
