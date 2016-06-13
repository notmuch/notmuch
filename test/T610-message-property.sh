#!/usr/bin/env bash
test_description="message property API"

. ./test-lib.sh || exit 1

add_email_corpus

cat <<EOF > c_head
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <notmuch-test.h>

int main (int argc, char** argv)
{
   notmuch_database_t *db;
   notmuch_message_t *message = NULL;
   const char *val;
   notmuch_status_t stat;

   EXPECT0(notmuch_database_open (argv[1], NOTMUCH_DATABASE_MODE_READ_WRITE, &db));
   EXPECT0(notmuch_database_find_message(db, "4EFC743A.3060609@april.org", &message));
   if (message == NULL) {
	fprintf (stderr, "unable to find message");
	exit (1);
   }
EOF

cat <<EOF > c_tail
   EXPECT0(notmuch_database_destroy(db));
}
EOF

test_begin_subtest "notmuch_message_{add,get,remove}_property"
cat c_head - c_tail <<'EOF' | test_C ${MAIL_DIR}
{
   EXPECT0(notmuch_message_add_property (message, "testkey1", "testvalue1"));
   EXPECT0(notmuch_message_get_property (message, "testkey1", &val));
   printf("testkey1[1] = %s\n", val);
   EXPECT0(notmuch_message_add_property (message, "testkey2", "this value has spaces and = sign"));
   EXPECT0(notmuch_message_get_property (message, "testkey1", &val));
   printf("testkey1[2] = %s\n", val);
   EXPECT0(notmuch_message_get_property (message, "testkey1", &val));

   EXPECT0(notmuch_message_get_property (message, "testkey2", &val));
   printf("testkey2 = %s\n", val);

   /* Add second value for key */
   EXPECT0(notmuch_message_add_property (message, "testkey2", "zztestvalue3"));
   EXPECT0(notmuch_message_get_property (message, "testkey2", &val));
   printf("testkey2 = %s\n", val);

   /* remove first value for key */
   EXPECT0(notmuch_message_remove_property (message, "testkey2", "this value has spaces and = sign"));
   EXPECT0(notmuch_message_get_property (message, "testkey2", &val));
   printf("testkey2 = %s\n", val);

   /* remove non-existant value for key */
   EXPECT0(notmuch_message_remove_property (message, "testkey2", "this value has spaces and = sign"));
   EXPECT0(notmuch_message_get_property (message, "testkey2", &val));
   printf("testkey2 = %s\n", val);

   /* remove only value for key */
   EXPECT0(notmuch_message_remove_property (message, "testkey2", "zztestvalue3"));
   EXPECT0(notmuch_message_get_property (message, "testkey2", &val));
   printf("testkey2 = %s\n", val == NULL ? "NULL" : val);
}
EOF
cat <<'EOF' >EXPECTED
== stdout ==
testkey1[1] = testvalue1
testkey1[2] = testvalue1
testkey2 = this value has spaces and = sign
testkey2 = this value has spaces and = sign
testkey2 = zztestvalue3
testkey2 = zztestvalue3
testkey2 = NULL
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT



test_done
