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

/* If the user asks for more than this number of threads, then break
   the results down into chunks so that results start appearing
   quickly rather than the user having to wait until all results are
   available before anything appears.

   Since each subsequent chunk ends up having to re-do threading work
   done by all previous chunks, we double the chunk size repeatedly
   until all desired results have been returned.
*/
#define NOTMUCH_SEARCH_INITIAL_CHUNK_SIZE 100

/* Do the actual search for a chunk of threads and display the results,
   (returning the number of threads found in this chunk). */
static int
do_search_threads (const void *ctx,
		   notmuch_query_t *query,
		   notmuch_sort_t sort,
		   int first, int max_threads)
{
    notmuch_thread_t *thread;
    notmuch_threads_t *threads;
    notmuch_tags_t *tags;
    time_t date;
    const char *relative_date;
    int num_threads = 0;

    for (threads = notmuch_query_search_threads (query, first, max_threads);
	 notmuch_threads_has_more (threads);
	 notmuch_threads_advance (threads))
    {
	int first_tag = 1;

	thread = notmuch_threads_get (threads);
	num_threads++;

	if (sort == NOTMUCH_SORT_OLDEST_FIRST)
	    date = notmuch_thread_get_oldest_date (thread);
	else
	    date = notmuch_thread_get_newest_date (thread);

	relative_date = notmuch_time_relative_date (ctx, date);

	printf ("thread:%s %12s [%d/%d] %s; %s",
		notmuch_thread_get_thread_id (thread),
		relative_date,
		notmuch_thread_get_matched_messages (thread),
		notmuch_thread_get_total_messages (thread),
		notmuch_thread_get_authors (thread),
		notmuch_thread_get_subject (thread));

	printf (" (");
	for (tags = notmuch_thread_get_tags (thread);
	     notmuch_tags_has_more (tags);
	     notmuch_tags_advance (tags))
	{
	    if (! first_tag)
		printf (" ");
	    printf ("%s", notmuch_tags_get (tags));
	    first_tag = 0;
	}
	printf (")\n");

	notmuch_thread_destroy (thread);
    }

    return num_threads;
}

int
notmuch_search_command (void *ctx, int argc, char *argv[])
{
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    notmuch_query_t *query;
    char *query_str;
    int i, first = 0, max_threads = -1;
    char *opt, *end;
    notmuch_sort_t sort = NOTMUCH_SORT_NEWEST_FIRST;
    int chunk_size;
    int threads_in_chunk;

    for (i = 0; i < argc && argv[i][0] == '-'; i++) {
	if (strcmp (argv[i], "--") == 0) {
	    i++;
	    break;
	}
	if (STRNCMP_LITERAL (argv[i], "--first=") == 0) {
	    opt = argv[i] + sizeof ("--first=") - 1;
	    first = strtoul (opt, &end, 10);
	    if (*opt == '\0' || *end != '\0') {
		fprintf (stderr, "Invalid value for --first: %s\n", opt);
		return 1;
	    }
	} else if (STRNCMP_LITERAL (argv[i], "--max-threads=") == 0) {
	    opt = argv[i] + sizeof ("--max-threads=") - 1;
	    max_threads = strtoul (opt, &end, 10);
	    if (*opt == '\0' || *end != '\0') {
		fprintf (stderr, "Invalid value for --max-threads: %s\n", opt);
		return 1;
	    }
	} else if (STRNCMP_LITERAL (argv[i], "--sort=") == 0) {
	    opt = argv[i] + sizeof ("--sort=") - 1;
	    if (strcmp (opt, "oldest-first") == 0) {
		sort = NOTMUCH_SORT_OLDEST_FIRST;
	    } else if (strcmp (opt, "newest-first") == 0) {
		sort = NOTMUCH_SORT_NEWEST_FIRST;
	    } else {
		fprintf (stderr, "Invalid value for --sort: %s\n", opt);
		return 1;
	    }
	} else {
	    fprintf (stderr, "Unrecognized option: %s\n", argv[i]);
	    return 1;
	}
    }

    argc -= i;
    argv += i;

    config = notmuch_config_open (ctx, NULL, NULL);
    if (config == NULL)
	return 1;

    notmuch = notmuch_database_open (notmuch_config_get_database_path (config),
				     NOTMUCH_DATABASE_MODE_READ_ONLY);
    if (notmuch == NULL)
	return 1;

    query_str = query_string_from_args (ctx, argc, argv);
    if (query_str == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }
    if (*query_str == '\0') {
	fprintf (stderr, "Error: notmuch search requires at least one search term.\n");
	return 1;
    }

    query = notmuch_query_create (notmuch, query_str);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	return 1;
    }

    notmuch_query_set_sort (query, sort);

    /* If we receive a max-threads option, then the user is
       responsible for any chunking and we return all results at
       once. */
    if (max_threads < 0)
	chunk_size = NOTMUCH_SEARCH_INITIAL_CHUNK_SIZE;
    else
	chunk_size = max_threads;

    do {
	threads_in_chunk = do_search_threads (ctx, query, sort,
					      first, chunk_size);
	if (chunk_size == max_threads)
	    break;

	first += chunk_size;

	chunk_size *= 2;

    } while (threads_in_chunk);

    notmuch_query_destroy (query);
    notmuch_database_close (notmuch);

    return 0;
}
