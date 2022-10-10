#!/usr/bin/env bash
test_description="API tests for tags"

. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus

test_begin_subtest "building database"
test_expect_success "NOTMUCH_NEW"

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

   stat = notmuch_database_open_with_config (argv[1],
					      NOTMUCH_DATABASE_MODE_READ_WRITE,
					      NULL, NULL, &db, &msg);
   if (stat != NOTMUCH_STATUS_SUCCESS) {
     fprintf (stderr, "error opening database\n%s\n%s\n", notmuch_status_to_string (stat), msg ? msg : "");
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
test_begin_subtest "Xapian exception getting tags"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR} ${POSTLIST_PATH}
   {
      notmuch_tags_t *tags = NULL;
      fd = open(argv[2],O_WRONLY|O_TRUNC);
      if (fd < 0) {
	  fprintf (stderr, "error opening %s\n", argv[1]);
	  exit (1);
       }
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

test_begin_subtest "NULL tags are not valid"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
   {
       notmuch_bool_t valid = TRUE;
       valid = notmuch_tags_valid (NULL);
       fprintf(stdout, "valid = %d\n", valid);
   }
EOF
cat <<'EOF' >EXPECTED
== stdout ==
valid = 0
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
