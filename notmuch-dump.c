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
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    notmuch_query_t *query;
    FILE *output = stdout;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    notmuch_tags_t *tags;
    const char *query_str = "";

    config = notmuch_config_open (ctx, NULL, NULL);
    if (config == NULL)
	return 1;

    if (notmuch_database_open (notmuch_config_get_database_path (config),
			       NOTMUCH_DATABASE_MODE_READ_ONLY, &notmuch))
	return 1;

    char *output_file_name = NULL;
    int opt_index;

    notmuch_opt_desc_t options[] = {
	{ NOTMUCH_OPT_STRING, &output_file_name, "output", 'o', 0  },
	{ 0, 0, 0, 0, 0 }
    };

    opt_index = parse_arguments (argc, argv, options, 1);

    if (opt_index < 0) {
	/* diagnostics already printed */
	return 1;
    }

    if (output_file_name) {
	output = fopen (output_file_name, "w");
	if (output == NULL) {
	    fprintf (stderr, "Error opening %s for writing: %s\n",
		     output_file_name, strerror (errno));
	    return 1;
	}
    }


    if (opt_index < argc) {
	query_str = query_string_from_args (notmuch, argc - opt_index, argv + opt_index);
	if (query_str == NULL) {
	    fprintf (stderr, "Out of memory.\n");
	    return 1;
	}
    }

    query = notmuch_query_create (notmuch, query_str);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	return 1;
    }
    /* Don't ask xapian to sort by Message-ID. Xapian optimizes returning the
     * first results quickly at the expense of total time.
     */
    notmuch_query_set_sort (query, NOTMUCH_SORT_UNSORTED);

    for (messages = notmuch_query_search_messages (query);
	 notmuch_messages_valid (messages);
	 notmuch_messages_move_to_next (messages)) {
	int first = 1;
	message = notmuch_messages_get (messages);

	fprintf (output,
		 "%s (", notmuch_message_get_message_id (message));

	for (tags = notmuch_message_get_tags (message);
	     notmuch_tags_valid (tags);
	     notmuch_tags_move_to_next (tags)) {
	    if (! first)
		fprintf (output, " ");

	    fprintf (output, "%s", notmuch_tags_get (tags));

	    first = 0;
	}

	fprintf (output, ")\n");

	notmuch_message_destroy (message);
    }

    if (output != stdout)
	fclose (output);

    notmuch_query_destroy (query);
    notmuch_database_destroy (notmuch);

    return 0;
}
