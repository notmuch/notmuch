/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2013 Ben Gamari
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
 * Author: Ben Gamari <bgamari.foss@gmail.com>
 */

#include "notmuch-client.h"

static void
status_update_cb (const char *msg, unused (void *closure))
{
    printf("%s\n", msg);
}

int
notmuch_compact_command (notmuch_config_t *config,
			 unused (int argc),
			 unused (char *argv[]))
{
    const char *path = notmuch_config_get_database_path (config);
    const char *backup_path;
    notmuch_status_t ret;

    backup_path = talloc_asprintf (config, "%s/xapian.old", path);
    if (! backup_path)
	return 1;

    printf ("Compacting database...\n");
    ret = notmuch_database_compact (path, backup_path, status_update_cb, NULL);
    if (ret) {
	fprintf (stderr, "Compaction failed: %s\n", notmuch_status_to_string(ret));
    } else {
	printf ("\n");
	printf ("\n");
	printf ("The old database has been moved to %s", backup_path);
	printf ("\n");
	printf ("To delete run,\n");
	printf ("\n");
	printf ("    rm -R %s\n", backup_path);
	printf ("\n");
    }

    return 0;
}
