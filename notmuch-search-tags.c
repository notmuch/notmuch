/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2009 Carl Worth
 * Copyright © 2009 Jan Janak
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
 * Author: Jan Janak <jan@ryngle.com>
 */

#include "notmuch-client.h"

static void
print_tags (notmuch_tags_t *tags)
{
    const char *t;

    while ((t = notmuch_tags_get (tags))) {
	printf ("%s\n", t);
	notmuch_tags_advance (tags);
    }
}

int
notmuch_search_tags_command (void *ctx, int argc, char *argv[])
{
    notmuch_messages_t *msgs;
    notmuch_tags_t *tags;
    notmuch_config_t *config;
    notmuch_database_t *db;
    notmuch_query_t *query;
    char *query_str;

    tags = NULL;
    config = NULL;
    db = NULL;
    query = NULL;

    if ((config = notmuch_config_open (ctx, NULL, NULL)) == NULL) {
	goto error;
    }

    db = notmuch_database_open (notmuch_config_get_database_path (config),
				NOTMUCH_DATABASE_MODE_READ_ONLY);
    if (db == NULL) {
	goto error;
    }

    if (argc > 0) {
	if ((query_str = query_string_from_args (ctx, argc, argv)) == NULL) {
	    fprintf (stderr, "Out of memory.\n");
	    goto error;
	}

	if (*query_str == '\0') {
	    fprintf (stderr, "Error: Invalid search string.\n");
	    goto error;
	}

	if ((query = notmuch_query_create (db, query_str)) == NULL) {
	    fprintf (stderr, "Out of memory\n");
	    goto error;
	}


	msgs = notmuch_query_search_messages (query);
	if ((tags = notmuch_messages_collect_tags (msgs)) == NULL) goto error;
    } else {
	if ((tags = notmuch_database_get_all_tags (db)) == NULL) {
	    fprintf (stderr, "Error while getting tags from the database.\n");
	    goto error;
	}
    }

    print_tags (tags);

    notmuch_tags_destroy (tags);
    if (query) notmuch_query_destroy (query);
    notmuch_database_close (db);
    return 0;

error:
    if (tags) notmuch_tags_destroy (tags);
    if (query) notmuch_query_destroy (query);
    if (db) notmuch_database_close (db);
    return 1;
}
