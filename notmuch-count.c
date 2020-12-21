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
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Keith Packard <keithp@keithp.com>
 */

#include "notmuch-client.h"

enum {
    OUTPUT_THREADS,
    OUTPUT_MESSAGES,
    OUTPUT_FILES,
};

/* Return the number of files matching the query, or -1 for an error */
static int
count_files (notmuch_query_t *query)
{
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    notmuch_filenames_t *filenames;
    notmuch_status_t status;
    int count = 0;

    status = notmuch_query_search_messages (query, &messages);
    if (print_status_query ("notmuch count", query, status))
	return -1;

    for (;
	 notmuch_messages_valid (messages);
	 notmuch_messages_move_to_next (messages)) {
	message = notmuch_messages_get (messages);
	filenames = notmuch_message_get_filenames (message);

	for (;
	     notmuch_filenames_valid (filenames);
	     notmuch_filenames_move_to_next (filenames))
	    count++;

	notmuch_filenames_destroy (filenames);
	notmuch_message_destroy (message);
    }

    notmuch_messages_destroy (messages);

    return count;
}

/* return 0 on success, -1 on failure */
static int
print_count (notmuch_database_t *notmuch, const char *query_str,
	     const char **exclude_tags, size_t exclude_tags_length, int output, int print_lastmod)
{
    notmuch_query_t *query;
    size_t i;
    int count;
    unsigned int ucount;
    unsigned long revision;
    const char *uuid;
    int ret = 0;
    notmuch_status_t status;

    query = notmuch_query_create (notmuch, query_str);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	return -1;
    }

    for (i = 0; i < exclude_tags_length; i++) {
	status = notmuch_query_add_tag_exclude (query, exclude_tags[i]);
	if (status && status != NOTMUCH_STATUS_IGNORED) {
	    print_status_query ("notmuch count", query, status);
	    return -1;
	}
    }

    switch (output) {
    case OUTPUT_MESSAGES:
	status = notmuch_query_count_messages (query, &ucount);
	if (print_status_query ("notmuch count", query, status))
	    return -1;
	printf ("%u", ucount);
	break;
    case OUTPUT_THREADS:
	status = notmuch_query_count_threads (query, &ucount);
	if (print_status_query ("notmuch count", query, status))
	    return -1;
	printf ("%u", ucount);
	break;
    case OUTPUT_FILES:
	count = count_files (query);
	if (count >= 0) {
	    printf ("%d", count);
	} else {
	    ret = -1;
	    goto DONE;
	}
	break;
    }

    if (print_lastmod) {
	revision = notmuch_database_get_revision (notmuch, &uuid);
	printf ("\t%s\t%lu\n", uuid, revision);
    } else {
	fputs ("\n", stdout);
    }

  DONE:
    notmuch_query_destroy (query);

    return ret;
}

static int
count_file (notmuch_database_t *notmuch, FILE *input, const char **exclude_tags,
	    size_t exclude_tags_length, int output, int print_lastmod)
{
    char *line = NULL;
    ssize_t line_len;
    size_t line_size;
    int ret = 0;

    while (! ret && (line_len = getline (&line, &line_size, input)) != -1) {
	chomp_newline (line);
	ret = print_count (notmuch, line, exclude_tags, exclude_tags_length,
			   output, print_lastmod);
    }

    if (line)
	free (line);

    return ret;
}

int
notmuch_count_command (notmuch_config_t *config, unused(notmuch_database_t *notmuch), int argc, char *argv[])
{
    notmuch_database_t *notmuch;
    char *query_str;
    int opt_index;
    int output = OUTPUT_MESSAGES;
    bool exclude = true;
    const char **search_exclude_tags = NULL;
    size_t search_exclude_tags_length = 0;
    bool batch = false;
    bool print_lastmod = false;
    FILE *input = stdin;
    const char *input_file_name = NULL;
    int ret;

    notmuch_opt_desc_t options[] = {
	{ .opt_keyword = &output, .name = "output", .keywords =
	      (notmuch_keyword_t []){ { "threads", OUTPUT_THREADS },
				      { "messages", OUTPUT_MESSAGES },
				      { "files", OUTPUT_FILES },
				      { 0, 0 } } },
	{ .opt_bool = &exclude, .name = "exclude" },
	{ .opt_bool = &print_lastmod, .name = "lastmod" },
	{ .opt_bool = &batch, .name = "batch" },
	{ .opt_string = &input_file_name, .name = "input" },
	{ .opt_inherit = notmuch_shared_options },
	{ }
    };

    opt_index = parse_arguments (argc, argv, options, 1);
    if (opt_index < 0)
	return EXIT_FAILURE;

    notmuch_process_shared_options (argv[0]);

    if (input_file_name) {
	batch = true;
	input = fopen (input_file_name, "r");
	if (input == NULL) {
	    fprintf (stderr, "Error opening %s for reading: %s\n",
		     input_file_name, strerror (errno));
	    return EXIT_FAILURE;
	}
    }

    if (batch && opt_index != argc) {
	fprintf (stderr, "--batch and query string are not compatible\n");
	if (input)
	    fclose (input);
	return EXIT_FAILURE;
    }

    if (notmuch_database_open (notmuch_config_get_database_path (config),
			       NOTMUCH_DATABASE_MODE_READ_ONLY, &notmuch))
	return EXIT_FAILURE;

    notmuch_exit_if_unmatched_db_uuid (notmuch);

    query_str = query_string_from_args (config, argc - opt_index, argv + opt_index);
    if (query_str == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return EXIT_FAILURE;
    }

    if (exclude) {
	search_exclude_tags = notmuch_config_get_search_exclude_tags
				  (config, &search_exclude_tags_length);
    }

    if (batch)
	ret = count_file (notmuch, input, search_exclude_tags,
			  search_exclude_tags_length, output, print_lastmod);
    else
	ret = print_count (notmuch, query_str, search_exclude_tags,
			   search_exclude_tags_length, output, print_lastmod);

    notmuch_database_destroy (notmuch);

    if (input != stdin)
	fclose (input);

    return ret ? EXIT_FAILURE : EXIT_SUCCESS;
}
