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
