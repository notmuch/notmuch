#!/usr/bin/env bash
test_description="handling DatabaseModifiedError from _notmuch_message_create"
. $(dirname "$0")/test-lib.sh || exit 1

# Triggering this exception is slightly magical. We need to
# - first create a query, and from it a messages iterator
# - then modify the database "enough"; it's not totally clear how much is
#   enough, but adding the LKML corpus emails and doing some light tagging
#   seems to work
# - finally, iterate through the messages iterator, which should trigger the
#   DatabaseModifiedError exception

# add default corpus for the initial query
add_email_corpus
# As per above, copy LKML corpus email files, but do not add them to the DB yet.
cp -a $NOTMUCH_SRCDIR/test/corpora/lkml ${MAIL_DIR}/

test_begin_subtest "catching DatabaseModifiedError in _notmuch_message_create"

test_C ${MAIL_DIR} <<EOF
#include <notmuch-test.h>

int
main (int argc, char **argv)
{
    const char *path = argv[1];

    notmuch_database_t *rw_db, *ro_db;
    notmuch_messages_t *messages_ro, *messages_rw;
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

    EXPECT0 (notmuch_query_search_messages (query_ro, &messages_ro));

    // index the previously copied LKML corpus files
    EXPECT0 (system ("notmuch new --quiet"));

    EXPECT0 (notmuch_database_open_with_config (argv[1],
						NOTMUCH_DATABASE_MODE_READ_WRITE,
						"", NULL, &rw_db, &msg));
    if (msg) fputs (msg, stderr);

    query_rw = notmuch_query_create (rw_db, "");
    EXPECT0 (notmuch_query_search_messages (query_rw, &messages_rw));

    for (;
	 ! notmuch_messages_status (messages_rw);
	 notmuch_messages_move_to_next (messages_rw)) {
	notmuch_message_t *message = notmuch_messages_get (messages_rw);
	EXPECT0 (notmuch_message_add_tag (message, "tag"));
    }

    notmuch_database_close (rw_db);

    // try iterating over the query up to twice, we expect a Xapian
    // DatabaseModifiedError (mapped to NOTMUCH_STATUS_OPERATION_INVALIDATED)
    // on the first try
    for (try = 0; try < 2; try++) {
	for (;
	     ! notmuch_messages_status (messages_ro);
	     notmuch_messages_move_to_next (messages_ro)) {
	    notmuch_message_t *message = notmuch_messages_get (messages_ro);
	}
	status = notmuch_messages_status (messages_ro);
	if (status != NOTMUCH_STATUS_OPERATION_INVALIDATED)
	    break;

	notmuch_query_destroy (query_ro);
	notmuch_database_close (ro_db);

	EXPECT0 (notmuch_database_open_with_config (argv[1],
						    NOTMUCH_DATABASE_MODE_READ_ONLY,
						    "", NULL, &ro_db, &msg));
	query_ro = notmuch_query_create (ro_db, "");
	assert (query_ro);
	EXPECT0 (notmuch_query_search_messages (query_ro, &messages_ro));
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
