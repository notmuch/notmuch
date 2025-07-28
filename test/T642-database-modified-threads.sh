#!/usr/bin/env bash
test_description="handling NOTMUCH_STATUS_OPERATION_INVALIDATED in _notmuch_thread_create"
. $(dirname "$0")/test-lib.sh || exit 1

# See comment at the top of T641-database-modified-messages.sh
# for more details.

# add default corpus for the initial query
add_email_corpus
# As per above, copy LKML corpus email files, but do not add them to the DB yet.
cp -a $NOTMUCH_SRCDIR/test/corpora/lkml ${MAIL_DIR}/

test_begin_subtest "handling NOTMUCH_STATUS_OPERATION_INVALIDATED in _notmuch_thread_create"

test_C ${MAIL_DIR} <<EOF
#include <notmuch-test.h>

int
main (int argc, char **argv)
{
    const char *path = argv[1];

    notmuch_database_t *rw_db, *ro_db;
    notmuch_messages_t *messages;
    notmuch_threads_t *threads;
    notmuch_query_t *query_ro, *query_rw;
    notmuch_status_t status;
    char* msg = NULL;
    unsigned try;

    EXPECT0 (notmuch_database_open_with_config (argv[1],
					        NOTMUCH_DATABASE_MODE_READ_ONLY,
					        "", NULL, &ro_db, &msg));
    if (msg) fputs (msg, stderr);
    assert (ro_db);

    query_ro = notmuch_query_create (ro_db, "");
    assert (query_ro);

    EXPECT0 (notmuch_query_search_threads (query_ro, &threads));

    // index the previously copied LKML corpus files
    EXPECT0 (system ("notmuch new --quiet"));

    EXPECT0 (notmuch_database_open_with_config (argv[1],
					        NOTMUCH_DATABASE_MODE_READ_WRITE,
					        "", NULL, &rw_db, &msg));
    if (msg) fputs (msg, stderr);

    query_rw = notmuch_query_create (rw_db, "");
    EXPECT0 (notmuch_query_search_messages (query_rw, &messages));

    for (;
	 ! notmuch_messages_status (messages);
	 notmuch_messages_move_to_next (messages)) {
	notmuch_message_t *message = notmuch_messages_get (messages);
	EXPECT0 (notmuch_message_add_tag (message, "tag"));
    }

    notmuch_database_close (rw_db);

    // try iterating over the query up to twice, we expect a Xapian
    // DatabaseModifiedError (mapped to NOTMUCH_STATUS_OPERATION_INVALIDATED)
    // on the first try
    for (try = 0; try < 2; try++) {
	for (;
	     ! notmuch_threads_status (threads);
	     notmuch_threads_move_to_next (threads)) {
	    notmuch_thread_t *thread = notmuch_threads_get (threads);
	}
	status = notmuch_threads_status (threads);
	if (status != NOTMUCH_STATUS_OPERATION_INVALIDATED)
	    break;

	notmuch_query_destroy (query_ro);
	notmuch_database_close (ro_db);

	EXPECT0 (notmuch_database_open_with_config (argv[1],
						    NOTMUCH_DATABASE_MODE_READ_ONLY,
						    "", NULL, &ro_db, &msg));
	query_ro = notmuch_query_create (ro_db, "");
	assert (query_ro);
	EXPECT0 (notmuch_query_search_threads (query_ro, &threads));
    }

    if (status == NOTMUCH_STATUS_ITERATOR_EXHAUSTED)
	printf ("SUCCESS\n");
    else
	printf ("status: %s\n", notmuch_status_to_string (status));
    return 0;
}
EOF

cat <<'EOF' >EXPECTED
== stdout ==
SUCCESS
== stderr ==
EOF
test_expect_equal_file EXPECTED OUTPUT

test_done
