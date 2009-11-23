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

static int
list_all_tags (notmuch_database_t* db)
{
    notmuch_tags_t* tags;
    const char* t;

    if ((tags = notmuch_database_get_all_tags (db)) == NULL) {
	fprintf (stderr, "Error while obtaining tags from the database.\n");
	return 1;
    }

    while((t = notmuch_tags_get (tags))) {
	printf ("%s\n", t);
	notmuch_tags_advance (tags);
    }

    notmuch_tags_destroy (tags);
    return 0;
}

int
notmuch_search_tags_command (void *ctx, int argc, char *argv[])
{
    notmuch_config_t *config;
    notmuch_database_t *db;

    config = NULL;
    db = NULL;

    if ((config = notmuch_config_open (ctx, NULL, NULL)) == NULL) {
	goto error;
    }

    db = notmuch_database_open (notmuch_config_get_database_path (config),
				NOTMUCH_DATABASE_MODE_READ_ONLY);
    if (db == NULL) {
	goto error;
    }

    if (list_all_tags (db) != 0) goto error;

    notmuch_database_close (db);
    return 0;

error:
    if (db) notmuch_database_close (db);
    return 1;
}
