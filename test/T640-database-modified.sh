#!/usr/bin/env bash
test_description="DatabaseModifiedError handling"
. ./test-lib.sh || exit 1

# add enough messages to trigger the exception
add_email_corpus

test_begin_subtest "catching DatabaseModifiedError in _notmuch_message_ensure_metadata"
# it seems to need to be an early document to trigger the exception
first_id=$(notmuch search --output=messages '*'| head -1 | sed s/^id://)

test_C ${MAIL_DIR} <<EOF
#include <unistd.h>
#include <stdlib.h>
#include <notmuch-test.h>
#include <talloc.h>
#include <assert.h>
int
main (int argc, char **argv)
{
    const char *path = argv[1];

    notmuch_database_t *rw_db, *ro_db;
    notmuch_messages_t *messages;
    notmuch_message_t *message, *ro_message;
    notmuch_query_t *query;
    notmuch_tags_t *tags;
    int i;

    EXPECT0 (notmuch_database_open (path, NOTMUCH_DATABASE_MODE_READ_ONLY, &ro_db));
    assert(ro_db);

    EXPECT0 (notmuch_database_find_message (ro_db, "${first_id}", &ro_message));
    assert(ro_message);

    EXPECT0 (notmuch_database_open (path, NOTMUCH_DATABASE_MODE_READ_WRITE, &rw_db));
    query = notmuch_query_create(rw_db, "");
    EXPECT0 (notmuch_query_search_messages (query, &messages));

    for (;
	 notmuch_messages_valid (messages);
	 notmuch_messages_move_to_next (messages)) {
	message = notmuch_messages_get (messages);
	for (i=0; i<200; i++) {
	    char *tag_str = talloc_asprintf(rw_db, "%d", i);
	    EXPECT0 (notmuch_message_add_tag (message, tag_str));
	    talloc_free (tag_str);
	}
    }

    notmuch_database_close (rw_db);

    tags = notmuch_message_get_tags (ro_message);
    if (tags)
	printf("SUCCESS\n");
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
