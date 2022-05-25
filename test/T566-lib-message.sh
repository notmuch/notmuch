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
   const char *id = "87pr7gqidx.fsf@yoom.home.cworth.org";

   stat = notmuch_database_open_with_config (argv[1],
					     NOTMUCH_DATABASE_MODE_READ_WRITE,
					     NULL, NULL, &db, &msg);
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
87pr7gqidx.fsf@yoom.home.cworth.org
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
MAIL_DIR/cur/40:2,
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
        printf("%d\n%d\n", message != NULL, status == NOTMUCH_STATUS_CLOSED_DATABASE);
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
        printf("%d\n%d\n", message != NULL, status == NOTMUCH_STATUS_CLOSED_DATABASE);
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

test_begin_subtest "_notmuch_message_add_term catches exceptions"
cat c_head0 - c_tail <<'EOF' | test_private_C ${MAIL_DIR}
    {
	notmuch_private_status_t status;
	/* This relies on Xapian throwing an exception for adding empty terms */
	status = _notmuch_message_add_term (message, "body", "");
	printf("%d\n%d\n", message != NULL, status != NOTMUCH_STATUS_SUCCESS );
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "_notmuch_message_remove_term catches exceptions"
cat c_head0 - c_tail <<'EOF' | test_private_C ${MAIL_DIR}
    {
	notmuch_private_status_t status;
	/* Xapian throws the same exception for empty and non-existent terms;
	 * error string varies between Xapian versions. */
	status = _notmuch_message_remove_term (message, "tag", "nonexistent");
	printf("%d\n%d\n", message != NULL, status == NOTMUCH_STATUS_SUCCESS );
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "_notmuch_message_add_filename on closed db"
cat c_head - c_tail <<'EOF' | test_private_C ${MAIL_DIR}
    {
	notmuch_private_status_t status;
	status = _notmuch_message_add_filename (message, "some-filename");
	printf("%d\n%d\n", message != NULL, status != NOTMUCH_STATUS_SUCCESS);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "_notmuch_message_remove_filename on closed db"
cat c_head - c_tail <<'EOF' | test_private_C ${MAIL_DIR}
    {
	notmuch_private_status_t status;
	status = _notmuch_message_remove_filename (message, "some-filename");
	printf("%d\n%d\n", message != NULL, status != NOTMUCH_STATUS_SUCCESS);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle converting tags to maildir flags with closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
	notmuch_status_t status;
	status = notmuch_message_tags_to_maildir_flags (message);
	printf("%d\n%d\n", message != NULL, status != NOTMUCH_STATUS_SUCCESS);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

POSTLIST_PATH=(${MAIL_DIR}/.notmuch/xapian/postlist.*)
test_begin_subtest "Handle converting tags to maildir flags with corrupted db"
backup_database
cat c_head0 - c_tail <<'EOF' | test_C ${MAIL_DIR} ${POSTLIST_PATH}
    {
        notmuch_status_t status;

        status = notmuch_message_add_tag (message, "draft");
        if (status) exit(1);

        int fd = open(argv[2],O_WRONLY|O_TRUNC);
        if (fd < 0) {
            fprintf (stderr, "error opening %s\n", argv[1]);
            exit (1);
        }

        status = notmuch_message_tags_to_maildir_flags (message);
        printf("%d\n%d\n", message != NULL, status != NOTMUCH_STATUS_SUCCESS);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
restore_database
notmuch new
notmuch tag -draft id:87pr7gqidx.fsf@yoom.home.cworth.org
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "Handle removing all tags with closed db"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_status_t status;
        status = notmuch_message_remove_all_tags (message);
        printf("%d\n%d\n", message != NULL, status == NOTMUCH_STATUS_CLOSED_DATABASE);
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
        printf("%d\n%d\n", message != NULL, status == NOTMUCH_STATUS_CLOSED_DATABASE);
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
        printf("%d\n%d\n", message != NULL, status == NOTMUCH_STATUS_CLOSED_DATABASE);
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
