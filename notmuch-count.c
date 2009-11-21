/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2009 Carl Worth
 * Copyright © 2009 Keith Packard
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
 * Author: Keith Packard <keithp@keithp.com>
 */

#include "notmuch-client.h"

int
notmuch_count_command (void *ctx, int argc, char *argv[])
{
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    notmuch_query_t *query;
    char *query_str;
    int i;
#if 0
    char *opt, *end;
    int i, first = 0, max_threads = -1;
    notmuch_sort_t sort = NOTMUCH_SORT_NEWEST_FIRST;
#endif

    for (i = 0; i < argc && argv[i][0] == '-'; i++) {
	if (strcmp (argv[i], "--") == 0) {
	    i++;
	    break;
	}
#if 0
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
	} else
#endif
	{
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
	fprintf (stderr, "Error: notmuch count requires at least one count term.\n");
	return 1;
    }

    query = notmuch_query_create (notmuch, query_str);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	return 1;
    }

    printf ("%u\n", notmuch_query_count_messages(query));

    notmuch_query_destroy (query);
    notmuch_database_close (notmuch);

    return 0;
}
