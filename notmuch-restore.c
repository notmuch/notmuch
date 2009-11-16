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
notmuch_restore_command (unused (void *ctx), int argc, char *argv[])
{
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    FILE *input;
    char *line = NULL;
    size_t line_size;
    ssize_t line_len;
    regex_t regex;
    int rerr;

    config = notmuch_config_open (ctx, NULL, NULL);
    if (config == NULL)
	return 1;

    notmuch = notmuch_database_open (notmuch_config_get_database_path (config));
    if (notmuch == NULL)
	return 1;

    if (argc) {
	input = fopen (argv[0], "r");
	if (input == NULL) {
	    fprintf (stderr, "Error opening %s for reading: %s\n",
		     argv[0], strerror (errno));
	    return 1;
	}
    } else {
	printf ("No filename given. Reading dump from stdin.\n");
	input = stdin;
    }

    /* Dump output is one line per message. We match a sequence of
     * non-space characters for the message-id, then one or more
     * spaces, then a list of space-separated tags as a sequence of
     * characters within literal '(' and ')'. */
    xregcomp (&regex,
	      "^([^ ]+) \\(([^)]*)\\)$",
	      REG_EXTENDED);

    while ((line_len = getline (&line, &line_size, input)) != -1) {
	regmatch_t match[3];
	char *message_id, *tags, *tag, *next;
	notmuch_message_t *message;
	notmuch_status_t status;

	chomp_newline (line);

	rerr = xregexec (&regex, line, 3, match, 0);
	if (rerr == REG_NOMATCH)
	{
	    fprintf (stderr, "Warning: Ignoring invalid input line: %s\n",
		     line);
	    continue;
	}

	message_id = xstrndup (line + match[1].rm_so,
			       match[1].rm_eo - match[1].rm_so);
	tags = xstrndup (line + match[2].rm_so,
			 match[2].rm_eo - match[2].rm_so);

	message = notmuch_database_find_message (notmuch, message_id);
	if (message == NULL) {
	    fprintf (stderr, "Warning: Cannot apply tags to missing message: %s\n",
		     message_id);
	    goto NEXT_LINE;
	}

	notmuch_message_freeze (message);

	notmuch_message_remove_all_tags (message);

	next = tags;
	while (next) {
	    tag = strsep (&next, " ");
	    if (*tag == '\0')
		continue;
	    status = notmuch_message_add_tag (message, tag);
	    if (status) {
		fprintf (stderr,
			 "Error applying tag %s to message %s:\n",
			 tag, message_id);
		fprintf (stderr, "%s\n",
			 notmuch_status_to_string (status));
	    }
	}

	notmuch_message_thaw (message);
	notmuch_message_destroy (message);
      NEXT_LINE:
	free (message_id);
	free (tags);
    }

    regfree (&regex);

    if (line)
	free (line);

    notmuch_database_close (notmuch);
    if (input != stdin)
	fclose (input);

    return 0;
}
