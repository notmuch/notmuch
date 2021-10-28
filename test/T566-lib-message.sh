#!/usr/bin/env bash
test_description="API tests for notmuch_message_*"

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

cat <<EOF > c_head0
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
EOF

cp c_head0 c_head
echo "   EXPECT0(notmuch_database_close (db));" >> c_head

test_begin_subtest "Handle getting message-id from closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
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

# XXX this test only tests the trivial code path
test_begin_subtest "Handle getting replies from closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
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

test_begin_subtest "iterate over all message filenames from closed database"
cat c_head0 - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_filenames_t *filenames;
        filenames = notmuch_message_get_filenames (message);
        EXPECT0(notmuch_database_close (db));
        for (; notmuch_filenames_valid (filenames);
               notmuch_filenames_move_to_next (filenames)) {
            const char *filename = notmuch_filenames_get (filenames);
            printf("%s\n", filename);
        }
        notmuch_filenames_destroy (filenames);
        printf("SUCCESS\n");
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
MAIL_DIR/01:2,
SUCCESS
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle getting ghost flag from closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
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
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_status_t status;
        notmuch_bool_t out;
        status = notmuch_message_has_maildir_flag_st (message, 'S', &out);
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

test_begin_subtest "Handle converting maildir flags to tags with closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_status_t status;
        status = notmuch_message_maildir_flags_to_tags (message);
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

test_begin_subtest "Handle removing all tags with closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_status_t status;
        status = notmuch_message_remove_all_tags (message);
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

test_begin_subtest "Handle freezing message with closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_status_t status;
        status = notmuch_message_freeze (message);
        printf("%d\n%d\n", message != NULL, status == NOTMUCH_STATUS_SUCCESS);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle thawing message with closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_status_t status;
        status = notmuch_message_thaw (message);
        printf("%d\n%d\n", message != NULL, status == NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle destroying message with closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_message_destroy (message);
        printf("%d\n%d\n", message != NULL, 1);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle retrieving closed db from message"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_database_t *db2;
        db2 = notmuch_message_get_database (message);
        printf("%d\n%d\n", message != NULL, db == db2);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle reindexing message with closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_status_t status;
        status = notmuch_message_reindex (message, NULL);
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

test_done
