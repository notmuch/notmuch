#include "notmuch-client.h"

notmuch_status_t
print_status_query (const char *loc,
		    const notmuch_query_t *query,
		    notmuch_status_t status)
{
    if (status) {
	const char *msg;
	notmuch_database_t *notmuch;

	fprintf (stderr, "%s: %s\n", loc,
		 notmuch_status_to_string (status));

	notmuch = notmuch_query_get_database (query);
	msg = notmuch_database_status_string (notmuch);
	if (msg)
	    fputs (msg, stderr);
    }
    return status;
}

notmuch_status_t
print_status_database (const char *loc,
		    const notmuch_database_t *notmuch,
		    notmuch_status_t status)
{
    if (status) {
	const char *msg;

	fprintf (stderr, "%s: %s\n", loc,
		 notmuch_status_to_string (status));
	msg = notmuch_database_status_string (notmuch);
	if (msg)
	    fputs (msg, stderr);
    }
    return status;
}

int
status_to_exit (notmuch_status_t status)
{
    switch (status) {
    case NOTMUCH_STATUS_SUCCESS:
	return EXIT_SUCCESS;
    case NOTMUCH_STATUS_OUT_OF_MEMORY:
    case NOTMUCH_STATUS_XAPIAN_EXCEPTION:
    case NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID:
    case NOTMUCH_STATUS_FILE_ERROR:
	return EX_TEMPFAIL;
    default:
	return EXIT_FAILURE;
    }
}
