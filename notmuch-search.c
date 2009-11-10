/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2009 Carl Worth
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-client.h"

int
notmuch_search_command (void *ctx, int argc, char *argv[])
{
    void *local = talloc_new (ctx);
    notmuch_database_t *notmuch = NULL;
    notmuch_query_t *query;
    notmuch_threads_t *threads;
    notmuch_thread_t *thread;
    notmuch_tags_t *tags;
    char *query_str;
    const char *relative_date;
    time_t date;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;

    notmuch = notmuch_database_open (NULL);
    if (notmuch == NULL) {
	ret = 1;
	goto DONE;
    }

    query_str = query_string_from_args (local, argc, argv);

    query = notmuch_query_create (notmuch, query_str);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	ret = 1;
	goto DONE;
    }

    for (threads = notmuch_query_search_threads (query);
	 notmuch_threads_has_more (threads);
	 notmuch_threads_advance (threads))
    {
	int first = 1;

	thread = notmuch_threads_get (threads);

	date = notmuch_thread_get_oldest_date (thread);
	relative_date = notmuch_time_relative_date (local, date);

	printf ("thread:%s %12s %s",
		notmuch_thread_get_thread_id (thread),
		relative_date,
		notmuch_thread_get_subject (thread));

	printf (" (");
	for (tags = notmuch_thread_get_tags (thread);
	     notmuch_tags_has_more (tags);
	     notmuch_tags_advance (tags))
	{
	    if (! first)
		printf (" ");
	    printf ("%s", notmuch_tags_get (tags));
	    first = 0;
	}
	printf (")\n");

	notmuch_thread_destroy (thread);
    }

    notmuch_query_destroy (query);

  DONE:
    if (notmuch)
	notmuch_database_close (notmuch);
    talloc_free (local);

    return ret;
}
