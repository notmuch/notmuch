#!/usr/bin/env bash
test_description="error reporting for library"

. ./test-lib.sh

backup_database () {
    rm -rf notmuch-dir-backup
    cp -pR ${MAIL_DIR}/.notmuch notmuch-dir-backup
}
restore_database () {
    rm -rf ${MAIL_DIR}/.notmuch
    cp -pR notmuch-dir-backup ${MAIL_DIR}/.notmuch
}


add_email_corpus

test_expect_success "building database" "NOTMUCH_NEW"

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

test_begin_subtest "Open nonexistent database"
test_C <<'EOF'
#include <stdio.h>
#include <notmuch.h>
int main (int argc, char** argv)
{
    notmuch_database_t *db;
    notmuch_status_t stat;
    stat = notmuch_database_open ("/nonexistent/foo", 0, 0);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
Error opening database at /nonexistent/foo/.notmuch: No such file or directory
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
Error: Cannot create a database for a NULL path.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Create database in non-existant directory"
test_C <<'EOF'
#include <stdio.h>
#include <notmuch.h>
int main (int argc, char** argv)
{
    notmuch_database_t *db;
    notmuch_status_t stat;
    stat = notmuch_database_create ("/nonexistent/foo", &db);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
Error: Cannot create database at /nonexistent/foo: No such file or directory.
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
   stat = notmuch_database_add_message (db, "/dev/null", NULL);
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
   stat = notmuch_database_add_message (db, "/nonexistent", NULL);
   if (stat)
       fputs (notmuch_database_status_string (db), stderr);

}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
Error opening /nonexistent: No such file or directory
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
Path already exists: CWD/mail

== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

cat <<'EOF' > c_head
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
   int fd;

   stat = notmuch_database_open (argv[1], NOTMUCH_DATABASE_MODE_READ_WRITE, &db);
   if (stat != NOTMUCH_STATUS_SUCCESS) {
     fprintf (stderr, "error opening database: %d\n", stat);
   }
   path = talloc_asprintf (db, "%s/.notmuch/xapian/postlist.DB", argv[1]);
   fd = open(path,O_WRONLY|O_TRUNC);
   if (fd < 0)
       fprintf (stderr, "error opening %s\n");
EOF
cat <<'EOF' > c_tail
   if (stat) {
       const char *stat_str = notmuch_database_status_string (db);
       if (stat_str)
           fputs (stat_str, stderr);
    }

}
EOF

backup_database
test_begin_subtest "Xapian exception finding message"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
   {
       notmuch_message_t *message = NULL;
       stat = notmuch_database_find_message (db, "id:nonexistant", &message);
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
   {
       notmuch_directory_t *directory = NULL;
       stat = notmuch_database_get_directory (db, "none/existing", &directory);
   }
EOF
sed 's/^\(A Xapian exception [^:]*\):.*$/\1/' < OUTPUT > OUTPUT.clean
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
A Xapian exception occurred creating a directory
EOF
test_expect_equal_file EXPECTED OUTPUT.clean
restore_database

backup_database
test_begin_subtest "Xapian exception searching messages"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
   {
       notmuch_messages_t *messages = NULL;
       notmuch_query_t *query=notmuch_query_create (db, "*");
       stat = notmuch_query_search_messages_st (query, &messages);
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
   {
       notmuch_query_t *query=notmuch_query_create (db, "id:87ocn0qh6d.fsf@yoom.home.cworth.org");
       int count = notmuch_query_count_messages (query);
       stat = (count == 0);
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
