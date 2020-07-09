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
Error: Cannot create a database for a NULL path.
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
   path = talloc_asprintf (db, "%s/.notmuch/xapian/postlist.${db_ending}", argv[1]);
   fd = open(path,O_WRONLY|O_TRUNC);
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

backup_database
test_begin_subtest "Xapian exception finding message"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
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

cat <<EOF > c_head2
#include <stdio.h>
#include <notmuch.h>
#include <notmuch-test.h>
int main (int argc, char** argv)
{
   notmuch_database_t *db;
   notmuch_status_t stat;
   char *msg = NULL;
   notmuch_message_t *message = NULL;
   const char *id = "1258471718-6781-1-git-send-email-dottedmag@dottedmag.net";

   stat = notmuch_database_open_verbose (argv[1], NOTMUCH_DATABASE_MODE_READ_WRITE, &db, &msg);
   if (stat != NOTMUCH_STATUS_SUCCESS) {
     fprintf (stderr, "error opening database: %d %s\n", stat, msg ? msg : "");
     exit (1);
   }
   EXPECT0(notmuch_database_find_message (db, id, &message));
   EXPECT0(notmuch_database_close (db));
EOF

test_begin_subtest "Handle getting message-id from closed database"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        const char *id2;
        id2=notmuch_message_get_message_id (message);
        printf("%d\n%d\n", message != NULL, id2==NULL);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle getting thread-id from closed database"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        const char *id2;
        id2=notmuch_message_get_thread_id (message);
        printf("%d\n%d\n", message != NULL, id2==NULL);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle getting header from closed database"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        const char *from;
        from=notmuch_message_get_header (message, "from");
        printf("%s\n%d\n", id, from == NULL);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1258471718-6781-1-git-send-email-dottedmag@dottedmag.net
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

# XXX TODO: test on a message from notmuch_thread_get_toplevel_messages
# XXX this test only tests the trivial code path
test_begin_subtest "Handle getting replies from closed database"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_messages_t *replies;
        replies = notmuch_message_get_replies (message);
        printf("%d\n%d\n", message != NULL, replies==NULL);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle getting message filename from closed database"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        const char *filename;
        filename = notmuch_message_get_filename (message);
        printf("%d\n%d\n", message != NULL, filename == NULL);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle getting all message filenames from closed database"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_filenames_t *filenames;
        filenames = notmuch_message_get_filenames (message);
        printf("%d\n%d\n", message != NULL, filenames == NULL);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle getting ghost flag from closed database"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_bool_t result;
        result = notmuch_message_get_flag (message, NOTMUCH_MESSAGE_FLAG_GHOST);
        printf("%d\n%d\n", message != NULL, result == FALSE);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle getting date from closed database"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        time_t result;
        result = notmuch_message_get_date (message);
        printf("%d\n%d\n", message != NULL, result == 0);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle getting tags from closed database"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_tags_t *result;
        result = notmuch_message_get_tags (message);
        printf("%d\n%d\n", message != NULL, result == NULL);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle counting files from closed database"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        int result;
        result = notmuch_message_count_files (message);
        printf("%d\n%d\n", message != NULL, result < 0);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle adding tag with closed database"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_status_t status;
        status = notmuch_message_add_tag (message, "boom");
        printf("%d\n%d\n", message != NULL, status == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle removing tag with closed database"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_status_t status;
        status = notmuch_message_remove_tag (message, "boom");
        printf("%d\n%d\n", message != NULL, status == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle read maildir flag with closed database"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_bool_t is_set = -1;
        is_set = notmuch_message_has_maildir_flag (message, 'S');
        printf("%d\n%d\n", message != NULL, is_set == FALSE || is_set == TRUE);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle checking maildir flag with closed db (new API)"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_status_t status;
        notmuch_bool_t out;
        status = notmuch_message_has_maildir_flag_st (message, 'S', &out);
        printf("%d\n%d\n", message != NULL,  status == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle converting maildir flags to tags with closed db"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_status_t status;
        status = notmuch_message_maildir_flags_to_tags (message);
        printf("%d\n%d\n", message != NULL,  status == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle removing all tags with closed db"
cat c_head2 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_status_t status;
        status = notmuch_message_remove_all_tags (message);
        printf("%d\n%d\n", message != NULL,  status == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
