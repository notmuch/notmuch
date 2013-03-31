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

enum {
    OUTPUT_THREADS,
    OUTPUT_MESSAGES,
};

/* The following is to allow future options to be added more easily */
enum {
    EXCLUDE_TRUE,
    EXCLUDE_FALSE,
};

static int
print_count (notmuch_database_t *notmuch, const char *query_str,
	     const char **exclude_tags, size_t exclude_tags_length, int output)
{
    notmuch_query_t *query;
    size_t i;

    query = notmuch_query_create (notmuch, query_str);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	return 1;
    }

    for (i = 0; i < exclude_tags_length; i++)
	notmuch_query_add_tag_exclude (query, exclude_tags[i]);

    switch (output) {
    case OUTPUT_MESSAGES:
	printf ("%u\n", notmuch_query_count_messages (query));
	break;
    case OUTPUT_THREADS:
	printf ("%u\n", notmuch_query_count_threads (query));
	break;
    }

    notmuch_query_destroy (query);

    return 0;
}

static int
count_file (notmuch_database_t *notmuch, FILE *input, const char **exclude_tags,
	    size_t exclude_tags_length, int output)
{
    char *line = NULL;
    ssize_t line_len;
    size_t line_size;
    int ret = 0;

    while (!ret && (line_len = getline (&line, &line_size, input)) != -1) {
	chomp_newline (line);
	ret = print_count (notmuch, line, exclude_tags, exclude_tags_length,
			   output);
    }

    if (line)
	free (line);

    return ret;
}

int
notmuch_count_command (notmuch_config_t *config, int argc, char *argv[])
{
    notmuch_database_t *notmuch;
    char *query_str;
    int opt_index;
    int output = OUTPUT_MESSAGES;
    int exclude = EXCLUDE_TRUE;
    const char **search_exclude_tags = NULL;
    size_t search_exclude_tags_length = 0;
    notmuch_bool_t batch = FALSE;
    FILE *input = stdin;
    char *input_file_name = NULL;
    int ret;

    notmuch_opt_desc_t options[] = {
	{ NOTMUCH_OPT_KEYWORD, &output, "output", 'o',
	  (notmuch_keyword_t []){ { "threads", OUTPUT_THREADS },
				  { "messages", OUTPUT_MESSAGES },
				  { 0, 0 } } },
	{ NOTMUCH_OPT_KEYWORD, &exclude, "exclude", 'x',
	  (notmuch_keyword_t []){ { "true", EXCLUDE_TRUE },
				  { "false", EXCLUDE_FALSE },
				  { 0, 0 } } },
	{ NOTMUCH_OPT_BOOLEAN, &batch, "batch", 0, 0 },
	{ NOTMUCH_OPT_STRING, &input_file_name, "input", 'i', 0 },
	{ 0, 0, 0, 0, 0 }
    };

    opt_index = parse_arguments (argc, argv, options, 1);

    if (opt_index < 0) {
	return 1;
    }

    if (input_file_name) {
	batch = TRUE;
	input = fopen (input_file_name, "r");
	if (input == NULL) {
	    fprintf (stderr, "Error opening %s for reading: %s\n",
		     input_file_name, strerror (errno));
	    return 1;
	}
    }

    if (batch && opt_index != argc) {
	fprintf (stderr, "--batch and query string are not compatible\n");
	return 1;
    }

    if (notmuch_database_open (notmuch_config_get_database_path (config),
			       NOTMUCH_DATABASE_MODE_READ_ONLY, &notmuch))
	return 1;

    query_str = query_string_from_args (config, argc-opt_index, argv+opt_index);
    if (query_str == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    if (exclude == EXCLUDE_TRUE) {
	search_exclude_tags = notmuch_config_get_search_exclude_tags
	    (config, &search_exclude_tags_length);
    }

    if (batch)
	ret = count_file (notmuch, input, search_exclude_tags,
			  search_exclude_tags_length, output);
    else
	ret = print_count (notmuch, query_str, search_exclude_tags,
			   search_exclude_tags_length, output);

    notmuch_database_destroy (notmuch);

    if (input != stdin)
	fclose (input);

    return ret;
}
