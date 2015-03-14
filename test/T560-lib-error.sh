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
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
== stderr ==
Cannot write to a read-only database.
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
== stderr ==
Path already exists: CWD/mail
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
