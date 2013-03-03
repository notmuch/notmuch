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
#include "dump-restore-private.h"
#include "string-util.h"

int
notmuch_dump_command (notmuch_config_t *config, int argc, char *argv[])
{
    notmuch_database_t *notmuch;
    notmuch_query_t *query;
    FILE *output = stdout;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    notmuch_tags_t *tags;
    const char *query_str = "";

    if (notmuch_database_open (notmuch_config_get_database_path (config),
			       NOTMUCH_DATABASE_MODE_READ_ONLY, &notmuch))
	return 1;

    char *output_file_name = NULL;
    int opt_index;

    int output_format = DUMP_FORMAT_SUP;

    notmuch_opt_desc_t options[] = {
	{ NOTMUCH_OPT_KEYWORD, &output_format, "format", 'f',
	  (notmuch_keyword_t []){ { "sup", DUMP_FORMAT_SUP },
				  { "batch-tag", DUMP_FORMAT_BATCH_TAG },
				  { 0, 0 } } },
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

    char *buffer = NULL;
    size_t buffer_size = 0;

    for (messages = notmuch_query_search_messages (query);
	 notmuch_messages_valid (messages);
	 notmuch_messages_move_to_next (messages)) {
	int first = 1;
	const char *message_id;

	message = notmuch_messages_get (messages);
	message_id = notmuch_message_get_message_id (message);

	if (output_format == DUMP_FORMAT_BATCH_TAG &&
	    strchr (message_id, '\n')) {
	    /* This will produce a line break in the output, which
	     * would be difficult to handle in tools.  However, it's
	     * also impossible to produce an email containing a line
	     * break in a message ID because of unfolding, so we can
	     * safely disallow it. */
	    fprintf (stderr, "Warning: skipping message id containing line break: \"%s\"\n", message_id);
	    notmuch_message_destroy (message);
	    continue;
	}

	if (output_format == DUMP_FORMAT_SUP) {
	    fprintf (output, "%s (", message_id);
	}

	for (tags = notmuch_message_get_tags (message);
	     notmuch_tags_valid (tags);
	     notmuch_tags_move_to_next (tags)) {
	    const char *tag_str = notmuch_tags_get (tags);

	    if (! first)
		fputs (" ", output);

	    first = 0;

	    if (output_format == DUMP_FORMAT_SUP) {
		fputs (tag_str, output);
	    } else {
		if (hex_encode (notmuch, tag_str,
				&buffer, &buffer_size) != HEX_SUCCESS) {
		    fprintf (stderr, "Error: failed to hex-encode tag %s\n",
			     tag_str);
		    return 1;
		}
		fprintf (output, "+%s", buffer);
	    }
	}

	if (output_format == DUMP_FORMAT_SUP) {
	    fputs (")\n", output);
	} else {
	    if (make_boolean_term (notmuch, "id", message_id,
				   &buffer, &buffer_size)) {
		    fprintf (stderr, "Error quoting message id %s: %s\n",
			     message_id, strerror (errno));
		    return 1;
	    }
	    fprintf (output, " -- %s\n", buffer);
	}

	notmuch_message_destroy (message);
    }

    if (output != stdout)
	fclose (output);

    notmuch_query_destroy (query);
    notmuch_database_destroy (notmuch);

    return 0;
}
