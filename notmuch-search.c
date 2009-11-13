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
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    notmuch_query_t *query;
    notmuch_threads_t *threads;
    notmuch_thread_t *thread;
    notmuch_tags_t *tags;
    char *query_str;
    const char *relative_date;
    time_t date;

    config = notmuch_config_open (ctx, NULL, NULL);
    if (config == NULL)
	return 1;

    notmuch = notmuch_database_open (notmuch_config_get_database_path (config));
    if (notmuch == NULL)
	return 1;

    query_str = query_string_from_args (ctx, argc, argv);
    if (query_str == NULL) {
	fprintf (stderr, "Out of moemory.\n");
	return 1;
    }

    query = notmuch_query_create (notmuch, query_str);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	return 1;
    }

    for (threads = notmuch_query_search_threads (query, 0, -1);
	 notmuch_threads_has_more (threads);
	 notmuch_threads_advance (threads))
    {
	int first = 1;

	thread = notmuch_threads_get (threads);

	date = notmuch_thread_get_oldest_date (thread);
	relative_date = notmuch_time_relative_date (ctx, date);

	printf ("thread:%s %12s %s; %s",
		notmuch_thread_get_thread_id (thread),
		relative_date,
		notmuch_thread_get_authors (thread),
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
    notmuch_database_close (notmuch);

    return 0;
}
