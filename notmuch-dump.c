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
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-client.h"
#include "hex-escape.h"
#include "string-util.h"
#include "zlib-extra.h"

static int
database_dump_config (notmuch_database_t *notmuch, gzFile output)
{
    notmuch_config_list_t *list;
    int ret = EXIT_FAILURE;
    char *buffer = NULL;
    size_t buffer_size = 0;

    if (print_status_database ("notmuch dump", notmuch,
			       notmuch_database_get_config_list (notmuch, NULL, &list)))
	goto DONE;

    for (; notmuch_config_list_valid (list); notmuch_config_list_move_to_next (list)) {
	if (hex_encode (notmuch, notmuch_config_list_key (list),
			&buffer, &buffer_size) != HEX_SUCCESS) {
	    fprintf (stderr, "Error: failed to hex-encode config key %s\n",
		     notmuch_config_list_key (list));
	    goto DONE;
	}
	GZPRINTF (output, "#@ %s", buffer);

	if (hex_encode (notmuch, notmuch_config_list_value (list),
			&buffer, &buffer_size) != HEX_SUCCESS) {
	    fprintf (stderr, "Error: failed to hex-encode config value %s\n",
		     notmuch_config_list_value (list) );
	    goto DONE;
	}

	GZPUTS (output, " ");
	GZPUTS (output, buffer);
	GZPUTS (output, "\n");
    }

    ret = EXIT_SUCCESS;

  DONE:
    if (list)
	notmuch_config_list_destroy (list);

    if (buffer)
	talloc_free (buffer);

    return ret;
}

static void
print_dump_header (gzFile output, int output_format, int include)
{
    const char *sep = "";

    GZPRINTF (output, "#notmuch-dump %s:%d ",
	      (output_format == DUMP_FORMAT_SUP) ? "sup" : "batch-tag",
	      NOTMUCH_DUMP_VERSION);

    if (include & DUMP_INCLUDE_CONFIG) {
	GZPUTS (output, "config");
	sep = ",";
    }
    if (include & DUMP_INCLUDE_PROPERTIES) {
	GZPRINTF (output, "%sproperties", sep);
	sep = ",";
    }
    if (include & DUMP_INCLUDE_TAGS) {
	GZPRINTF (output, "%stags", sep);
    }
    GZPUTS (output, "\n");
}

static int
dump_properties_message (void *ctx,
			 notmuch_message_t *message,
			 gzFile output,
			 char **buffer_p, size_t *size_p)
{
    const char *message_id;
    notmuch_message_properties_t *list;
    bool first = true;

    message_id = notmuch_message_get_message_id (message);

    if (strchr (message_id, '\n')) {
	fprintf (stderr, "Warning: skipping message id containing line break: \"%s\"\n", message_id);
	return 0;
    }

    for (list = notmuch_message_get_properties (message, "", false);
	 notmuch_message_properties_valid (list); notmuch_message_properties_move_to_next (list)) {
	const char *key, *val;

	if (first) {
	    if (hex_encode (ctx, message_id, buffer_p, size_p) != HEX_SUCCESS) {
		fprintf (stderr, "Error: failed to hex-encode message-id %s\n", message_id);
		return 1;
	    }
	    GZPRINTF (output, "#= %s", *buffer_p);
	    first = false;
	}

	key = notmuch_message_properties_key (list);
	val = notmuch_message_properties_value (list);

	if (hex_encode (ctx, key, buffer_p, size_p) != HEX_SUCCESS) {
	    fprintf (stderr, "Error: failed to hex-encode key %s\n", key);
	    return 1;
	}
	GZPRINTF (output, " %s", *buffer_p);

	if (hex_encode (ctx, val, buffer_p, size_p) != HEX_SUCCESS) {
	    fprintf (stderr, "Error: failed to hex-encode value %s\n", val);
	    return 1;
	}
	GZPRINTF (output, "=%s", *buffer_p);
    }
    notmuch_message_properties_destroy (list);

    if (! first)
	GZPRINTF (output, "\n", *buffer_p);

    return 0;
}

static int
dump_tags_message (void *ctx,
		   notmuch_message_t *message, int output_format,
		   gzFile output,
		   char **buffer_p, size_t *size_p)
{
    int first = 1;
    const char *message_id;

    message_id = notmuch_message_get_message_id (message);

    if (output_format == DUMP_FORMAT_BATCH_TAG &&
	strchr (message_id, '\n')) {
	/* This will produce a line break in the output, which
	 * would be difficult to handle in tools.  However, it's
	 * also impossible to produce an email containing a line
	 * break in a message ID because of unfolding, so we can
	 * safely disallow it. */
	fprintf (stderr, "Warning: skipping message id containing line break: \"%s\"\n", message_id);
	return EXIT_SUCCESS;
    }

    if (output_format == DUMP_FORMAT_SUP) {
	GZPRINTF (output, "%s (", message_id);
    }

    for (notmuch_tags_t *tags = notmuch_message_get_tags (message);
	 notmuch_tags_valid (tags);
	 notmuch_tags_move_to_next (tags)) {
	const char *tag_str = notmuch_tags_get (tags);

	if (! first)
	    GZPUTS (output, " ");

	first = 0;

	if (output_format == DUMP_FORMAT_SUP) {
	    GZPUTS (output, tag_str);
	} else {
	    if (hex_encode (ctx, tag_str,
			    buffer_p, size_p) != HEX_SUCCESS) {
		fprintf (stderr, "Error: failed to hex-encode tag %s\n",
			 tag_str);
		return EXIT_FAILURE;
	    }
	    GZPRINTF (output, "+%s", *buffer_p);
	}
    }

    if (output_format == DUMP_FORMAT_SUP) {
	GZPUTS (output, ")\n");
    } else {
	if (make_boolean_term (ctx, "id", message_id,
			       buffer_p, size_p)) {
	    fprintf (stderr, "Error quoting message id %s: %s\n",
		     message_id, strerror (errno));
	    return EXIT_FAILURE;
	}
	GZPRINTF (output, " -- %s\n", *buffer_p);
    }
    return EXIT_SUCCESS;
}

static int
database_dump_file (notmuch_database_t *notmuch, gzFile output,
		    const char *query_str, int output_format, int include)
{
    notmuch_query_t *query;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    notmuch_status_t status;
    char *buffer = NULL;
    size_t buffer_size = 0;

    print_dump_header (output, output_format, include);

    if (include & DUMP_INCLUDE_CONFIG) {
	if (print_status_database ("notmuch dump", notmuch,
				   database_dump_config (notmuch, output)))
	    return EXIT_FAILURE;
    }

    if (! (include & (DUMP_INCLUDE_TAGS | DUMP_INCLUDE_PROPERTIES)))
	return EXIT_SUCCESS;

    if (! query_str)
	query_str = "";

    query = notmuch_query_create (notmuch, query_str);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	return EXIT_FAILURE;
    }
    /* Don't ask xapian to sort by Message-ID. Xapian optimizes returning the
     * first results quickly at the expense of total time.
     */
    notmuch_query_set_sort (query, NOTMUCH_SORT_UNSORTED);

    status = notmuch_query_search_messages (query, &messages);
    if (print_status_query ("notmuch dump", query, status))
	return EXIT_FAILURE;

    for (;
	 notmuch_messages_valid (messages);
	 notmuch_messages_move_to_next (messages)) {

	message = notmuch_messages_get (messages);

	if ((include & DUMP_INCLUDE_TAGS) &&
	    dump_tags_message (notmuch, message, output_format, output,
			       &buffer, &buffer_size))
	    return EXIT_FAILURE;

	if ((include & DUMP_INCLUDE_PROPERTIES) &&
	    dump_properties_message (notmuch, message, output,
				     &buffer, &buffer_size))
	    return EXIT_FAILURE;

	notmuch_message_destroy (message);
    }

    notmuch_query_destroy (query);

    return EXIT_SUCCESS;
}

/* Dump database into output_file_name if it's non-NULL, stdout
 * otherwise.
 */
int
notmuch_database_dump (notmuch_database_t *notmuch,
		       const char *output_file_name,
		       const char *query_str,
		       dump_format_t output_format,
		       dump_include_t include,
		       bool gzip_output)
{
    gzFile output = NULL;
    const char *mode = gzip_output ? "w9" : "wT";
    const char *name_for_error = output_file_name ? output_file_name : "stdout";

    char *tempname = NULL;
    int outfd = -1;

    int ret = -1;

    if (output_file_name) {
	tempname = talloc_asprintf (notmuch, "%s.XXXXXX", output_file_name);
	outfd = mkstemp (tempname);
    } else {
	outfd = dup (STDOUT_FILENO);
    }

    if (outfd < 0) {
	fprintf (stderr, "Bad output file %s\n", name_for_error);
	goto DONE;
    }

    output = gzdopen (outfd, mode);

    if (output == NULL) {
	fprintf (stderr, "Error opening %s for (gzip) writing: %s\n",
		 name_for_error, strerror (errno));
	if (close (outfd))
	    fprintf (stderr, "Error closing %s during shutdown: %s\n",
		     name_for_error, strerror (errno));
	goto DONE;
    }

    ret = database_dump_file (notmuch, output, query_str, output_format, include);
    if (ret) goto DONE;

    ret = gzflush (output, Z_FINISH);
    if (ret) {
	fprintf (stderr, "Error flushing output: %s\n", gzerror_str (output));
	goto DONE;
    }

    if (output_file_name) {
	ret = fsync (outfd);
	if (ret) {
	    fprintf (stderr, "Error syncing %s to disk: %s\n",
		     name_for_error, strerror (errno));
	    goto DONE;
	}
    }

    ret = gzclose_w (output);
    if (ret) {
	fprintf (stderr, "Error closing %s: %s\n", name_for_error,
		 gzerror_str (output));
	ret = EXIT_FAILURE;
	output = NULL;
	goto DONE;
    } else
        output = NULL;

    if (output_file_name) {
	ret = rename (tempname, output_file_name);
	if (ret) {
	    fprintf (stderr, "Error renaming %s to %s: %s\n",
		     tempname, output_file_name, strerror (errno));
	    goto DONE;
	}

    }
  DONE:
    if (ret != EXIT_SUCCESS && output)
	(void) gzclose_w (output);

    if (ret != EXIT_SUCCESS && output_file_name)
	(void) unlink (tempname);

    return ret;
}

int
notmuch_dump_command (unused(notmuch_config_t *config), notmuch_database_t *notmuch , int argc, char *argv[])
{
    const char *query_str = NULL;
    int ret;

    notmuch_exit_if_unmatched_db_uuid (notmuch);

    const char *output_file_name = NULL;
    int opt_index;

    int output_format = DUMP_FORMAT_BATCH_TAG;
    int include = 0;
    bool gzip_output = 0;

    notmuch_opt_desc_t options[] = {
	{ .opt_keyword = &output_format, .name = "format", .keywords =
	      (notmuch_keyword_t []){ { "sup", DUMP_FORMAT_SUP },
				      { "batch-tag", DUMP_FORMAT_BATCH_TAG },
				      { 0, 0 } } },
	{ .opt_flags = &include, .name = "include", .keywords =
	      (notmuch_keyword_t []){ { "config", DUMP_INCLUDE_CONFIG },
				      { "properties", DUMP_INCLUDE_PROPERTIES },
				      { "tags", DUMP_INCLUDE_TAGS } } },
	{ .opt_string = &output_file_name, .name = "output" },
	{ .opt_bool = &gzip_output, .name = "gzip" },
	{ .opt_inherit = notmuch_shared_options },
	{ }
    };

    opt_index = parse_arguments (argc, argv, options, 1);
    if (opt_index < 0)
	return EXIT_FAILURE;

    notmuch_process_shared_options (argv[0]);

    if (include == 0)
	include = DUMP_INCLUDE_CONFIG | DUMP_INCLUDE_TAGS | DUMP_INCLUDE_PROPERTIES;

    if (opt_index < argc) {
	query_str = query_string_from_args (notmuch, argc - opt_index, argv + opt_index);
	if (query_str == NULL) {
	    fprintf (stderr, "Out of memory.\n");
	    return EXIT_FAILURE;
	}
    }

    ret = notmuch_database_dump (notmuch, output_file_name, query_str,
				 output_format, include, gzip_output);

    notmuch_database_destroy (notmuch);

    return ret;
}
