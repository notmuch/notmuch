#!/usr/bin/env bash
test_description="error reporting for library"

. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus

test_begin_subtest "building database"
test_expect_success "NOTMUCH_NEW"

test_begin_subtest "Open null pointer"
test_C <<'EOF'
#include <stdio.h>
#include <notmuch.h>
int main (int argc, char** argv)
{
    notmuch_database_t *db;
    notmuch_status_t stat;
    stat = notmuch_database_open (NULL, 0, 0);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
Error: Cannot open a database for a NULL path.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Open relative path"
test_C <<'EOF'
#include <stdio.h>
#include <notmuch.h>
int main (int argc, char** argv)
{
    notmuch_database_t *db;
    notmuch_status_t stat;
    stat = notmuch_database_open ("./nonexistent/foo", 0, 0);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
Error: Database path must be absolute.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Create database in relative path"
test_C <<'EOF'
#include <stdio.h>
#include <notmuch.h>
int main (int argc, char** argv)
{
    notmuch_database_t *db;
    notmuch_status_t stat;
    stat = notmuch_database_create ("./nonexistent/foo", &db);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
Error: Database path must be absolute.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Open nonexistent database"
test_C ${PWD}/nonexistent/foo <<'EOF'
#include <stdio.h>
#include <notmuch.h>
int main (int argc, char** argv)
{
    notmuch_database_t *db;
    notmuch_status_t stat;
    stat = notmuch_database_open (argv[1], 0, 0);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
Error opening database at CWD/nonexistent/foo/.notmuch: No such file or directory
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "create NULL path"
test_C <<'EOF'
#include <stdio.h>
#include <notmuch.h>
int main (int argc, char** argv)
{
    notmuch_status_t stat;
    stat = notmuch_database_create (NULL, NULL);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
Error: Cannot open a database for a NULL path.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Create database in nonexistent directory"
test_C ${PWD}/nonexistent/foo<<'EOF'
#include <stdio.h>
#include <notmuch.h>
int main (int argc, char** argv)
{
    notmuch_database_t *db;
    notmuch_status_t stat;
    stat = notmuch_database_create (argv[1], &db);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
Error: Cannot create database at CWD/nonexistent/foo: No such file or directory.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Write to read-only database"
test_C ${MAIL_DIR} <<'EOF'
#include <stdio.h>
#include <notmuch.h>
int main (int argc, char** argv)
{
   notmuch_database_t *db;
   notmuch_status_t stat;
   stat = notmuch_database_open (argv[1], NOTMUCH_DATABASE_MODE_READ_ONLY, &db);
   if (stat != NOTMUCH_STATUS_SUCCESS) {
     fprintf (stderr, "error opening database: %d\n", stat);
   }
   stat = notmuch_database_index_file (db, "/dev/null", NULL, NULL);
   if (stat)
       fputs (notmuch_database_status_string (db), stderr);

}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
Cannot write to a read-only database.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Add non-existent file"
test_C ${MAIL_DIR} <<'EOF'
#include <stdio.h>
#include <notmuch.h>
int main (int argc, char** argv)
{
   notmuch_database_t *db;
   notmuch_status_t stat;
   stat = notmuch_database_open (argv[1], NOTMUCH_DATABASE_MODE_READ_WRITE, &db);
   if (stat != NOTMUCH_STATUS_SUCCESS) {
     fprintf (stderr, "error opening database: %d\n", stat);
   }
   stat = notmuch_database_index_file (db, "./nonexistent", NULL, NULL);
   if (stat) {
       char *status_string = notmuch_database_status_string (db);
       if (status_string) fputs (status_string, stderr);
   }
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
Error opening ./nonexistent: No such file or directory
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "compact, overwriting existing backup"
test_C ${MAIL_DIR} <<'EOF'
#include <stdio.h>
#include <notmuch.h>
static void
status_cb (const char *msg, void *closure)
{
    printf ("%s\n", msg);
}
int main (int argc, char** argv)
{
   notmuch_database_t *db;
   notmuch_status_t stat;
   stat = notmuch_database_compact (argv[1], argv[1], status_cb, NULL);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
Path already exists: MAIL_DIR

== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

cat <<EOF > c_head
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <talloc.h>
#include <notmuch.h>

int main (int argc, char** argv)
{
   notmuch_database_t *db;
   notmuch_status_t stat;
   char *path;
   char *msg = NULL;
   int fd;

   stat = notmuch_database_open_verbose (argv[1], NOTMUCH_DATABASE_MODE_READ_WRITE, &db, &msg);
   if (stat != NOTMUCH_STATUS_SUCCESS) {
     fprintf (stderr, "error opening database: %d %s\n", stat, msg ? msg : "");
     exit (1);
   }
   fd = open(argv[2],O_WRONLY|O_TRUNC);
   if (fd < 0) {
       fprintf (stderr, "error opening %s\n", argv[1]);
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

POSTLIST_PATH=(${MAIL_DIR}/.notmuch/xapian/postlist.*)
backup_database
test_begin_subtest "Xapian exception finding message"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${POSTLIST_PATH}
   {
       notmuch_message_t *message = NULL;
       stat = notmuch_database_find_message (db, "id:nonexistent", &message);
   }
EOF
sed 's/^\(A Xapian exception [^:]*\):.*$/\1/' < OUTPUT > OUTPUT.clean
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
A Xapian exception occurred finding message
EOF
test_expect_equal_file EXPECTED OUTPUT.clean
restore_database

backup_database
test_begin_subtest "Xapian exception getting tags"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${POSTLIST_PATH}
   {
       notmuch_tags_t *tags = NULL;
       tags = notmuch_database_get_all_tags (db);
       stat = (tags == NULL);
   }
EOF
sed 's/^\(A Xapian exception [^:]*\):.*$/\1/' < OUTPUT > OUTPUT.clean
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
A Xapian exception occurred getting tags
EOF
test_expect_equal_file EXPECTED OUTPUT.clean
restore_database

backup_database
test_begin_subtest "Xapian exception creating directory"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${POSTLIST_PATH}
   {
       notmuch_directory_t *directory = NULL;
       stat = notmuch_database_get_directory (db, "none/existing", &directory);
   }
EOF
sed 's/^\(A Xapian exception [^:]*\):.*$/\1/' < OUTPUT > OUTPUT.clean
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
A Xapian exception occurred finding/creating a directory
EOF
test_expect_equal_file EXPECTED OUTPUT.clean
restore_database

backup_database
test_begin_subtest "Xapian exception searching messages"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${POSTLIST_PATH}
   {
       notmuch_messages_t *messages = NULL;
       notmuch_query_t *query=notmuch_query_create (db, "*");
       stat = notmuch_query_search_messages (query, &messages);
   }
EOF
sed 's/^\(A Xapian exception [^:]*\):.*$/\1/' < OUTPUT > OUTPUT.clean
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
A Xapian exception occurred performing query
Query string was: *
EOF
test_expect_equal_file EXPECTED OUTPUT.clean
restore_database

backup_database
test_begin_subtest "Xapian exception counting messages"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${POSTLIST_PATH}
   {
       int count;
       notmuch_query_t *query=notmuch_query_create (db, "id:87ocn0qh6d.fsf@yoom.home.cworth.org");
       stat = notmuch_query_count_messages (query, &count);
   }
EOF
sed 's/^\(A Xapian exception [^:]*\):.*$/\1/' < OUTPUT > OUTPUT.clean
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
A Xapian exception occurred performing query
Query string was: id:87ocn0qh6d.fsf@yoom.home.cworth.org
EOF
test_expect_equal_file EXPECTED OUTPUT.clean
restore_database

test_done
