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
notmuch_dump_command (unused (void *ctx), int argc, char *argv[])
{
    FILE *output = NULL;
    notmuch_database_t *notmuch = NULL;
    notmuch_query_t *query;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    notmuch_tags_t *tags;
    int ret = 0;

    if (argc) {
	output = fopen (argv[0], "w");
	if (output == NULL) {
	    fprintf (stderr, "Error opening %s for writing: %s\n",
		     argv[0], strerror (errno));
	    ret = 1;
	    goto DONE;
	}
    } else {
	output = stdout;
    }

    notmuch = notmuch_database_open (NULL);
    if (notmuch == NULL) {
	ret = 1;
	goto DONE;
    }

    query = notmuch_query_create (notmuch, "");
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	ret = 1;
	goto DONE;
    }

    notmuch_query_set_sort (query, NOTMUCH_SORT_MESSAGE_ID);

    for (messages = notmuch_query_search_messages (query);
	 notmuch_messages_has_more (messages);
	 notmuch_messages_advance (messages))
    {
	int first = 1;
	message = notmuch_messages_get (messages);

	fprintf (output,
		 "%s (", notmuch_message_get_message_id (message));

	for (tags = notmuch_message_get_tags (message);
	     notmuch_tags_has_more (tags);
	     notmuch_tags_advance (tags))
	{
	    if (! first)
		fprintf (output, " ");

	    fprintf (output, "%s", notmuch_tags_get (tags));

	    first = 0;
	}

	fprintf (output, ")\n");

	notmuch_message_destroy (message);
    }

    notmuch_query_destroy (query);

  DONE:
    if (notmuch)
	notmuch_database_close (notmuch);
    if (output && output != stdout)
	fclose (output);

    return ret;
}
