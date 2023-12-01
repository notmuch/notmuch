#!/usr/bin/env bash
test_description="notmuch_directory_* API"

. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus

test_begin_subtest "building database"
test_expect_success "NOTMUCH_NEW"

cat <<EOF > c_head
#include <notmuch-test.h>

int main (int argc, char** argv)
{
   notmuch_database_t *db;
   notmuch_directory_t *dir;
   notmuch_status_t stat = NOTMUCH_STATUS_SUCCESS;
   char *msg = NULL;

   stat = notmuch_database_open_with_config (argv[1],
					     NOTMUCH_DATABASE_MODE_READ_WRITE,
					     NULL, NULL, &db, &msg);
   if (stat != NOTMUCH_STATUS_SUCCESS) {
     fprintf (stderr, "error opening database: %d %s\n", stat, msg ? msg : "");
     exit (1);
   }

   EXPECT0(notmuch_database_get_directory (db, "bar", &dir));
   EXPECT0(notmuch_database_close (db));
EOF

cat <<'EOF' > c_tail
   if (stat) {
       const char *stat_str = notmuch_database_status_string (db);
       if (stat_str)
           fputs (stat_str, stderr);
    }

}
EOF

test_begin_subtest "get child directories for a closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_filenames_t *children;
        children = notmuch_directory_get_child_directories (dir);
        printf ("%d\n", children == NULL);
        stat = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
A Xapian exception occurred at directory.cc:XXX: Database has been closed
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "get child filenames for a closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_filenames_t *children;
        children = notmuch_directory_get_child_files (dir);
        printf ("%d\n", children == NULL);
        stat = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
A Xapian exception occurred at directory.cc:XXX: Database has been closed
EOF
test_expect_equal_file EXPECTED OUTPUT

backup_database
test_begin_subtest "delete directory document for a closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        stat = notmuch_directory_delete (dir);
        printf ("%d\n", stat == NOTMUCH_STATUS_CLOSED_DATABASE);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
Cannot write to a closed database.
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_database

backup_database
test_begin_subtest "get/set mtime of directory for a closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        time_t stamp = notmuch_directory_get_mtime (dir);
        stat = notmuch_directory_set_mtime (dir, stamp);
        printf ("%d\n", stat == NOTMUCH_STATUS_CLOSED_DATABASE);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
Cannot write to a closed database.
EOF
test_expect_equal_file EXPECTED OUTPUT
restore_database

test_done
