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

static int
tag_message (notmuch_database_t *notmuch, const char *message_id,
	     char *file_tags, notmuch_bool_t remove_all,
	     notmuch_bool_t synchronize_flags)
{
    notmuch_status_t status;
    notmuch_tags_t *db_tags;
    char *db_tags_str;
    notmuch_message_t *message = NULL;
    const char *tag;
    char *next;
    int ret = 0;

    status = notmuch_database_find_message (notmuch, message_id, &message);
    if (status || message == NULL) {
	fprintf (stderr, "Warning: Cannot apply tags to %smessage: %s\n",
		 message ? "" : "missing ", message_id);
	if (status)
	    fprintf (stderr, "%s\n", notmuch_status_to_string (status));
	return 1;
    }

    /* In order to detect missing messages, this check/optimization is
     * intentionally done *after* first finding the message. */
    if (! remove_all && (file_tags == NULL || *file_tags == '\0'))
	goto DONE;

    db_tags_str = NULL;
    for (db_tags = notmuch_message_get_tags (message);
	 notmuch_tags_valid (db_tags);
	 notmuch_tags_move_to_next (db_tags)) {
	tag = notmuch_tags_get (db_tags);

	if (db_tags_str)
	    db_tags_str = talloc_asprintf_append (db_tags_str, " %s", tag);
	else
	    db_tags_str = talloc_strdup (message, tag);
    }

    if (((file_tags == NULL || *file_tags == '\0') &&
	 (db_tags_str == NULL || *db_tags_str == '\0')) ||
	(file_tags && db_tags_str && strcmp (file_tags, db_tags_str) == 0))
	goto DONE;

    notmuch_message_freeze (message);

    if (remove_all)
	notmuch_message_remove_all_tags (message);

    next = file_tags;
    while (next) {
	tag = strsep (&next, " ");
	if (*tag == '\0')
	    continue;
	status = notmuch_message_add_tag (message, tag);
	if (status) {
	    fprintf (stderr, "Error applying tag %s to message %s:\n",
		     tag, message_id);
	    fprintf (stderr, "%s\n", notmuch_status_to_string (status));
	    ret = 1;
	}
    }

    notmuch_message_thaw (message);

    if (synchronize_flags)
	notmuch_message_tags_to_maildir_flags (message);

  DONE:
    if (message)
	notmuch_message_destroy (message);

    return ret;
}

int
notmuch_restore_command (unused (void *ctx), int argc, char *argv[])
{
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    notmuch_bool_t synchronize_flags;
    notmuch_bool_t accumulate = FALSE;
    char *input_file_name = NULL;
    FILE *input = stdin;
    char *line = NULL;
    size_t line_size;
    ssize_t line_len;
    regex_t regex;
    int rerr;
    int opt_index;

    config = notmuch_config_open (ctx, NULL, NULL);
    if (config == NULL)
	return 1;

    if (notmuch_database_open (notmuch_config_get_database_path (config),
			       NOTMUCH_DATABASE_MODE_READ_WRITE, &notmuch))
	return 1;

    synchronize_flags = notmuch_config_get_maildir_synchronize_flags (config);

    notmuch_opt_desc_t options[] = {
	{ NOTMUCH_OPT_STRING, &input_file_name, "input", 'i', 0 },
	{ NOTMUCH_OPT_BOOLEAN,  &accumulate, "accumulate", 'a', 0 },
	{ 0, 0, 0, 0, 0 }
    };

    opt_index = parse_arguments (argc, argv, options, 1);

    if (opt_index < 0) {
	/* diagnostics already printed */
	return 1;
    }

    if (input_file_name) {
	input = fopen (input_file_name, "r");
	if (input == NULL) {
	    fprintf (stderr, "Error opening %s for reading: %s\n",
		     input_file_name, strerror (errno));
	    return 1;
	}
    }

    if (opt_index < argc) {
	fprintf (stderr,
		 "Unused positional parameter: %s\n",
		 argv[opt_index]);
	return 1;
    }

    /* Dump output is one line per message. We match a sequence of
     * non-space characters for the message-id, then one or more
     * spaces, then a list of space-separated tags as a sequence of
     * characters within literal '(' and ')'. */
    if ( xregcomp (&regex,
		   "^([^ ]+) \\(([^)]*)\\)$",
		   REG_EXTENDED) )
	INTERNAL_ERROR ("compile time constant regex failed.");

    while ((line_len = getline (&line, &line_size, input)) != -1) {
	regmatch_t match[3];
	char *message_id, *file_tags;

	chomp_newline (line);

	rerr = xregexec (&regex, line, 3, match, 0);
	if (rerr == REG_NOMATCH) {
	    fprintf (stderr, "Warning: Ignoring invalid input line: %s\n",
		     line);
	    continue;
	}

	message_id = xstrndup (line + match[1].rm_so,
			       match[1].rm_eo - match[1].rm_so);
	file_tags = xstrndup (line + match[2].rm_so,
			      match[2].rm_eo - match[2].rm_so);

	tag_message (notmuch, message_id, file_tags, ! accumulate,
		     synchronize_flags);

	free (message_id);
	free (file_tags);
    }

    regfree (&regex);

    if (line)
	free (line);

    notmuch_database_destroy (notmuch);
    if (input != stdin)
	fclose (input);

    return 0;
}
