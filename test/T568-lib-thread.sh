#!/usr/bin/env bash
test_description="API tests for notmuch_thread_*"

. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus

test_begin_subtest "building database"
test_expect_success "NOTMUCH_NEW"

test_begin_subtest "finding thread"
THREAD=$(notmuch search --output=threads id:20091117190054.GU3165@dottiness.seas.harvard.edu)
count=$(notmuch count $THREAD)
test_expect_equal "$count" "7"

cat <<'EOF' > c_tail
   if (stat) {
       const char *stat_str = notmuch_database_status_string (db);
       if (stat_str)
           fputs (stat_str, stderr);
    }

}
EOF

cat <<EOF > c_head
#include <notmuch-test.h>

int main (int argc, char** argv)
{
   notmuch_database_t *db;
   notmuch_status_t stat;
   char *msg = NULL;
   notmuch_thread_t *thread = NULL;
   notmuch_threads_t *threads = NULL;
   notmuch_query_t *query = NULL;
   const char *id = "${THREAD}";

   stat = notmuch_database_open_verbose (argv[1], NOTMUCH_DATABASE_MODE_READ_WRITE, &db, &msg);
   if (stat != NOTMUCH_STATUS_SUCCESS) {
     fprintf (stderr, "error opening database: %d %s\n", stat, msg ? msg : "");
     exit (1);
   }

   query = notmuch_query_create (db, id);
   EXPECT0(notmuch_query_search_threads (query, &threads));
   thread = notmuch_threads_get (threads);
   EXPECT0(notmuch_database_close (db));
EOF

test_begin_subtest "get thread-id from closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        const char *id2;
        id2 = notmuch_thread_get_thread_id (thread);
        printf("%d\n%s\n", thread != NULL, id2);
    }
EOF
thread_num=${THREAD#thread:}
cat <<EOF > EXPECTED
== stdout ==
1
${thread_num}
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "get total messages with closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        int count;
        count = notmuch_thread_get_total_messages (thread);
        printf("%d\n%d\n", thread != NULL, count);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
7
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "get total files with closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        int count;
        count = notmuch_thread_get_total_files (thread);
        printf("%d\n%d\n", thread != NULL, count);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
7
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "get top level messages with closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        notmuch_messages_t *messages;
        messages = notmuch_thread_get_toplevel_messages (thread);
        printf("%d\n%d\n", thread != NULL, messages != NULL);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "iterate over level messages with closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
      notmuch_messages_t *messages;
      for (messages = notmuch_thread_get_toplevel_messages (thread);
           notmuch_messages_valid (messages);
           notmuch_messages_move_to_next (messages)) {
        notmuch_message_t *message = notmuch_messages_get (messages);
        const char *mid = notmuch_message_get_message_id (message);
        printf("%s\n", mid);
      }
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
20091117190054.GU3165@dottiness.seas.harvard.edu
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "iterate over level messages with closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
      notmuch_messages_t *messages;
      for (messages = notmuch_thread_get_toplevel_messages (thread);
           notmuch_messages_valid (messages);
           notmuch_messages_move_to_next (messages)) {
        notmuch_message_t *message = notmuch_messages_get (messages);
        const char *mid = notmuch_message_get_message_id (message);
        printf("%s\n", mid);
      }
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
20091117190054.GU3165@dottiness.seas.harvard.edu
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "iterate over replies with closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
      notmuch_messages_t *messages = notmuch_thread_get_toplevel_messages (thread);
      notmuch_message_t *message = notmuch_messages_get (messages);
      notmuch_messages_t *replies;
      for (replies = notmuch_message_get_replies (message);
           notmuch_messages_valid (replies);
           notmuch_messages_move_to_next (replies)) {
        notmuch_message_t *message = notmuch_messages_get (replies);
        const char *mid = notmuch_message_get_message_id (message);

        printf("%s\n", mid);
      }
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
87iqd9rn3l.fsf@vertex.dottedmag
87ocn0qh6d.fsf@yoom.home.cworth.org
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "iterate over all messages with closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
      notmuch_messages_t *messages;
      for (messages = notmuch_thread_get_messages (thread);
           notmuch_messages_valid (messages);
           notmuch_messages_move_to_next (messages)) {
        notmuch_message_t *message = notmuch_messages_get (messages);
        const char *mid = notmuch_message_get_message_id (message);
        printf("%s\n", mid);
      }
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
20091117190054.GU3165@dottiness.seas.harvard.edu
87iqd9rn3l.fsf@vertex.dottedmag
20091117203301.GV3165@dottiness.seas.harvard.edu
87fx8can9z.fsf@vertex.dottedmag
yunaayketfm.fsf@aiko.keithp.com
20091118005040.GA25380@dottiness.seas.harvard.edu
87ocn0qh6d.fsf@yoom.home.cworth.org
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "get authors from closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        const char *authors;
        authors = notmuch_thread_get_authors (thread);
        printf("%d\n%s\n", thread != NULL, authors);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
Lars Kellogg-Stedman, Mikhail Gusarov, Keith Packard, Carl Worth
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "get subject from closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        const char *subject;
        subject = notmuch_thread_get_subject (thread);
        printf("%d\n%s\n", thread != NULL, subject);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
[notmuch] Working with Maildir storage?
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "oldest date from closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        time_t stamp;
        stamp = notmuch_thread_get_oldest_date (thread);
        printf("%d\n%d\n", thread != NULL, stamp > 0);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "newest date from closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        time_t stamp;
        stamp = notmuch_thread_get_newest_date (thread);
        printf("%d\n%d\n", thread != NULL, stamp > 0);
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
1
1
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "iterate tags from closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
      notmuch_tags_t *tags;
      const char *tag;
      for (tags = notmuch_thread_get_tags (thread);
           notmuch_tags_valid (tags);
           notmuch_tags_move_to_next (tags))
        {
          tag = notmuch_tags_get (tags);
          printf ("%s\n", tag);
        }
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
inbox
signed
unread
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "collect tags with closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
      notmuch_messages_t *messages = notmuch_thread_get_messages (thread);

      notmuch_tags_t *tags = notmuch_messages_collect_tags (messages);

      const char *tag;
      for (tags = notmuch_thread_get_tags (thread);
           notmuch_tags_valid (tags);
           notmuch_tags_move_to_next (tags))
        {
          tag = notmuch_tags_get (tags);
          printf ("%s\n", tag);
        }
      notmuch_tags_destroy (tags);
      notmuch_messages_destroy (messages);

      printf("SUCCESS\n");
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
inbox
signed
unread
SUCCESS
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "destroy thread with closed database"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
    {
        time_t stamp;
        notmuch_thread_destroy (thread);
        printf("SUCCESS\n");
    }
EOF
cat <<EOF > EXPECTED
== stdout ==
SUCCESS
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
