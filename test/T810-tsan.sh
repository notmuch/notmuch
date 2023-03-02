#!/usr/bin/env bash

test_directory=$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null && pwd)

test_description='run code with TSan enabled against the library'
# Note it is hard to ensure race conditions are deterministic so this
# only provides best effort detection.

. "$test_directory"/test-lib.sh || exit 1

if [ $NOTMUCH_HAVE_TSAN -ne 1 ]; then
    printf "Skipping due to missing TSan support\n"
    test_done
fi

export TSAN_OPTIONS="suppressions=$test_directory/T810-tsan.suppressions"
TEST_CFLAGS="${TEST_CFLAGS:-} -fsanitize=thread"

cp -r ${MAIL_DIR} ${MAIL_DIR}-2

test_begin_subtest "create"
test_C ${MAIL_DIR} ${MAIL_DIR}-2 <<EOF
#include <notmuch-test.h>
#include <pthread.h>

void *thread (void *arg) {
  char *mail_dir = arg;
  /*
   * Calls into notmuch_query_search_messages which was using the thread-unsafe
   * Xapian::Query::MatchAll.
   */
  EXPECT0(notmuch_database_create (mail_dir, NULL));
  return NULL;
}

int main (int argc, char **argv) {
  pthread_t t1, t2;
  EXPECT0(pthread_create (&t1, NULL, thread, argv[1]));
  EXPECT0(pthread_create (&t2, NULL, thread, argv[2]));
  EXPECT0(pthread_join (t1, NULL));
  EXPECT0(pthread_join (t2, NULL));
  return 0;
}
EOF
cat <<EOF > EXPECTED
== stdout ==
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

add_email_corpus
rm -r ${MAIL_DIR}-2
cp -r ${MAIL_DIR} ${MAIL_DIR}-2

test_begin_subtest "query"
test_C ${MAIL_DIR} ${MAIL_DIR}-2 <<EOF
#include <notmuch-test.h>
#include <pthread.h>

void *thread (void *arg) {
  char *mail_dir = arg;
  notmuch_database_t *db;
  /*
   * 'from' is NOTMUCH_FIELD_PROBABILISTIC | NOTMUCH_FIELD_PROCESSOR and an
   * empty string gets us to RegexpFieldProcessor::operator which was using
   * the tread-unsafe Xapian::Query::MatchAll.
   */
  EXPECT0(notmuch_database_open_with_config (mail_dir,
                                             NOTMUCH_DATABASE_MODE_READ_ONLY,
                                             NULL, NULL, &db, NULL));
  notmuch_query_t *query = notmuch_query_create (db, "from:\"\"");
  notmuch_messages_t *messages;
  EXPECT0(notmuch_query_search_messages (query, &messages));
  return NULL;
}

int main (int argc, char **argv) {
  pthread_t t1, t2;
  EXPECT0(pthread_create (&t1, NULL, thread, argv[1]));
  EXPECT0(pthread_create (&t2, NULL, thread, argv[2]));
  EXPECT0(pthread_join (t1, NULL));
  EXPECT0(pthread_join (t2, NULL));
  return 0;
}
EOF
cat <<EOF > EXPECTED
== stdout ==
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
