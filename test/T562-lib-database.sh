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
        EXPECT0(notmuch_database_close (db));
        stat = notmuch_database_upgrade (db, NULL, NULL);
        printf ("%d\n", stat == NOTMUCH_STATUS_SUCCESS);
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
A Xapian exception occurred finding/creating a directory: Database has been closed.
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

test_begin_subtest "index file (absolute path outside mail root)"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_message_t *msg;
        stat = notmuch_database_index_file (db, "/dev/zero", NULL, &msg);
        printf ("%d\n", stat == NOTMUCH_STATUS_FILE_ERROR);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
Error opening /dev/zero: path outside mail root
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "remove message file with a closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        EXPECT0(notmuch_database_close (db));
        stat = notmuch_database_remove_message (db, "01:2,");
        printf ("%d\n", stat == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
A Xapian exception occurred finding/creating a directory: Database has been closed.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "find message by filename with a closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_message_t *msg;
        EXPECT0(notmuch_database_close (db));
        stat = notmuch_database_find_message_by_filename (db, "01:2,", &msg);
        printf ("%d\n", stat == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
A Xapian exception occurred finding/creating a directory: Database has been closed.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle getting tags from closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_tags_t *result;
        EXPECT0(notmuch_database_close (db));
        result = notmuch_database_get_all_tags (db);
        printf("%d\n",  result == NULL);
        stat = NOTMUCH_STATUS_XAPIAN_EXCEPTION;
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
A Xapian exception occurred getting tags: Database has been closed.
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "get config from closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        char *result;
        EXPECT0(notmuch_database_close (db));
        stat = notmuch_database_get_config (db, "foo", &result);
        printf("%d\n",  stat == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
0
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "set config in closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        EXPECT0(notmuch_database_close (db));
        stat = notmuch_database_set_config (db, "foo", "bar");
        printf("%d\n",  stat == NOTMUCH_STATUS_XAPIAN_EXCEPTION);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
Error: A Xapian exception occurred setting metadata: Database has been closed
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "get indexopts from closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_indexopts_t *result;
        EXPECT0(notmuch_database_close (db));
        result = notmuch_database_get_default_indexopts (db);
        printf("%d\n", result != NULL);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "get decryption policy from closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_indexopts_t *result;
        result = notmuch_database_get_default_indexopts (db);
        EXPECT0(notmuch_database_close (db));
        notmuch_decryption_policy_t policy = notmuch_indexopts_get_decrypt_policy (result);
        printf ("%d\n",  policy == NOTMUCH_DECRYPT_AUTO);
        notmuch_indexopts_destroy (result);
        printf ("SUCCESS\n");
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
SUCCESS
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "set decryption policy with closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_indexopts_t *result;
        result = notmuch_database_get_default_indexopts (db);
        EXPECT0(notmuch_database_close (db));
        notmuch_decryption_policy_t policy = notmuch_indexopts_get_decrypt_policy (result);
        stat = notmuch_indexopts_set_decrypt_policy (result, policy);
        printf("%d\n%d\n",  policy == NOTMUCH_DECRYPT_AUTO, stat == NOTMUCH_STATUS_SUCCESS);
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
