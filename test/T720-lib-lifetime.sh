#!/usr/bin/env bash
#
# Copyright (c) 2018 rhn
#


test_description="Lifetime constraints for library"

. $(dirname "$0")/test-lib.sh || exit 1

add_email_corpus threading

test_begin_subtest "building database"
test_expect_success "NOTMUCH_NEW"

test_begin_subtest "Message outlives parent Messages from replies"

test_C ${MAIL_DIR} <<'EOF'
#include <stdio.h>
#include <stdlib.h>
#include <notmuch.h>
int main (int argc, char** argv)
{
   notmuch_database_t *db;
   notmuch_status_t stat;
   char* msg = NULL;

   stat = notmuch_database_open_with_config (argv[1],
					     NOTMUCH_DATABASE_MODE_READ_ONLY,
					     "", NULL, &db, &msg);
   if (msg) fputs (msg, stderr);

   if (stat != NOTMUCH_STATUS_SUCCESS) {
     fprintf (stderr, "error opening database: %d\n", stat);
     exit (1);
   }

   notmuch_query_t *query = notmuch_query_create (db, "id:B00-root@example.org");
   notmuch_threads_t *threads;

   stat = notmuch_query_search_threads (query, &threads);
   if (stat != NOTMUCH_STATUS_SUCCESS) {
     fprintf (stderr, "error querying threads: %d\n", stat);
     exit (1);
   }

   if (!notmuch_threads_valid (threads)) {
     fprintf (stderr, "invalid threads");
     exit (1);
   }

   notmuch_thread_t *thread = notmuch_threads_get (threads);
   notmuch_messages_t *messages = notmuch_thread_get_messages (thread);

   if (!notmuch_messages_valid (messages)) {
     fprintf (stderr, "invalid messages");
     exit (1);
   }

   notmuch_message_t *message = notmuch_messages_get (messages);
   notmuch_messages_t *replies = notmuch_message_get_replies (message);
   if (!notmuch_messages_valid (replies)) {
     fprintf (stderr, "invalid replies");
     exit (1);
   }

   notmuch_message_t *reply = notmuch_messages_get (replies);

   notmuch_messages_destroy (replies); // the reply should not get destroyed here
   notmuch_message_destroy (message);
   notmuch_messages_destroy (messages); // nor here

   const char *mid = notmuch_message_get_message_id (reply); // should not crash when accessing
   fprintf (stdout, "Reply id: %s\n", mid);
   notmuch_message_destroy (reply);
   notmuch_thread_destroy (thread); // this destroys the reply
   notmuch_threads_destroy (threads);
   notmuch_query_destroy (query);
   notmuch_database_destroy (db);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
Reply id: B01-child@example.org
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
