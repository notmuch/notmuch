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
#include "tag-util.h"
#include "string-util.h"

static volatile sig_atomic_t interrupted;

static void
handle_sigint (unused (int sig))
{
    static char msg[] = "Stopping...         \n";

    /* This write is "opportunistic", so it's okay to ignore the
     * result.  It is not required for correctness, and if it does
     * fail or produce a short write, we want to get out of the signal
     * handler as quickly as possible, not retry it. */
    IGNORE_RESULT (write (2, msg, sizeof (msg) - 1));
    interrupted = 1;
}


static char *
_optimize_tag_query (void *ctx, const char *orig_query_string,
		     const tag_op_list_t *list)
{
    /* This is subtler than it looks.  Xapian ignores the '-' operator
     * at the beginning both queries and parenthesized groups and,
     * furthermore, the presence of a '-' operator at the beginning of
     * a group can inhibit parsing of the previous operator.  Hence,
     * the user-provided query MUST appear first, but it is safe to
     * parenthesize and the exclusion part of the query must not use
     * the '-' operator (though the NOT operator is fine). */

    char *escaped = NULL;
    size_t escaped_len = 0;
    char *query_string;
    const char *join = "";
    size_t i;

    /* Don't optimize if there are no tag changes. */
    if (tag_op_list_size (list) == 0)
	return talloc_strdup (ctx, orig_query_string);

    /* Build the new query string */
    if (strcmp (orig_query_string, "*") == 0)
	query_string = talloc_strdup (ctx, "(");
    else
	query_string = talloc_asprintf (ctx, "( %s ) and (", orig_query_string);

    for (i = 0; i < tag_op_list_size (list) && query_string; i++) {
	/* XXX in case of OOM, query_string will be deallocated when
	 * ctx is, which might be at shutdown */
	if (make_boolean_term (ctx,
			       "tag", tag_op_list_tag (list, i),
			       &escaped, &escaped_len))
	    return NULL;

	query_string = talloc_asprintf_append_buffer (
	    query_string, "%s%s%s", join,
	    tag_op_list_isremove (list, i) ? "" : "not ",
	    escaped);
	join = " or ";
    }

    if (query_string)
	query_string = talloc_strdup_append_buffer (query_string, ")");

    talloc_free (escaped);
    return query_string;
}

/* Tag messages matching 'query_string' according to 'tag_ops'
 */
static int
tag_query (void *ctx, notmuch_database_t *notmuch, const char *query_string,
	   tag_op_list_t *tag_ops, tag_op_flag_t flags)
{
    notmuch_query_t *query;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    notmuch_status_t status;

    int ret = NOTMUCH_STATUS_SUCCESS;

    if (! (flags & TAG_FLAG_REMOVE_ALL)) {
	/* Optimize the query so it excludes messages that already
	 * have the specified set of tags. */
	query_string = _optimize_tag_query (ctx, query_string, tag_ops);
	if (query_string == NULL) {
	    fprintf (stderr, "Out of memory.\n");
	    return 1;
	}
	flags |= TAG_FLAG_PRE_OPTIMIZED;
    }

    query = notmuch_query_create (notmuch, query_string);
    if (query == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    /* tagging is not interested in any special sort order */
    notmuch_query_set_sort (query, NOTMUCH_SORT_UNSORTED);

    status = notmuch_query_search_messages (query, &messages);
    if (print_status_query ("notmuch tag", query, status))
	return status;

    for (;
	 notmuch_messages_valid (messages) && ! interrupted;
	 notmuch_messages_move_to_next (messages)) {
	message = notmuch_messages_get (messages);
	ret = tag_op_list_apply (message, tag_ops, flags);
	notmuch_message_destroy (message);
	if (ret != NOTMUCH_STATUS_SUCCESS)
	    break;
    }

    notmuch_query_destroy (query);

    return ret || interrupted;
}

static int
tag_file (void *ctx, notmuch_database_t *notmuch, tag_op_flag_t flags,
	  FILE *input)
{
    char *line = NULL;
    char *query_string = NULL;
    size_t line_size = 0;
    ssize_t line_len;
    int ret = 0;
    int warn = 0;
    tag_op_list_t *tag_ops;

    tag_ops = tag_op_list_create (ctx);
    if (tag_ops == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    while ((line_len = getline (&line, &line_size, input)) != -1 &&
	   ! interrupted) {

	ret = parse_tag_line (ctx, line, TAG_FLAG_NONE,
			      &query_string, tag_ops);

	if (ret > 0) {
	    if (ret != TAG_PARSE_SKIPPED)
		/* remember there has been problematic lines */
		warn = 1;
	    ret = 0;
	    continue;
	}

	if (ret < 0)
	    break;

	ret = tag_query (ctx, notmuch, query_string, tag_ops, flags);
	if (ret)
	    break;
    }

    if (line)
	free (line);

    return ret || warn;
}

int
notmuch_tag_command (unused(notmuch_config_t *config), notmuch_database_t *notmuch, int argc, char *argv[])
{
    tag_op_list_t *tag_ops = NULL;
    char *query_string = NULL;
    struct sigaction action;
    tag_op_flag_t tag_flags = TAG_FLAG_NONE;
    bool batch = false;
    bool remove_all = false;
    FILE *input = stdin;
    const char *input_file_name = NULL;
    int opt_index;
    int ret;
    notmuch_bool_t synchronize_flags;

    /* Set up our handler for SIGINT */
    memset (&action, 0, sizeof (struct sigaction));
    action.sa_handler = handle_sigint;
    sigemptyset (&action.sa_mask);
    action.sa_flags = SA_RESTART;
    sigaction (SIGINT, &action, NULL);

    notmuch_opt_desc_t options[] = {
	{ .opt_bool = &batch, .name = "batch" },
	{ .opt_string = &input_file_name, .name = "input" },
	{ .opt_bool = &remove_all, .name = "remove-all" },
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

    if (batch) {
	if (opt_index != argc) {
	    fprintf (stderr, "Can't specify both cmdline and stdin!\n");
	    if (input)
		fclose (input);
	    return EXIT_FAILURE;
	}
    } else {
	tag_ops = tag_op_list_create (notmuch);
	if (tag_ops == NULL) {
	    fprintf (stderr, "Out of memory.\n");
	    return EXIT_FAILURE;
	}

	if (parse_tag_command_line (notmuch, argc - opt_index, argv + opt_index,
				    &query_string, tag_ops))
	    return EXIT_FAILURE;

	if (tag_op_list_size (tag_ops) == 0 && ! remove_all) {
	    fprintf (stderr, "Error: 'notmuch tag' requires at least one tag to add or remove.\n");
	    return EXIT_FAILURE;
	}

	if (*query_string == '\0') {
	    fprintf (stderr, "Error: notmuch tag requires at least one search term.\n");
	    return EXIT_FAILURE;
	}
    }

    notmuch_exit_if_unmatched_db_uuid (notmuch);

    if (print_status_database (
	    "notmuch restore",
	    notmuch,
	    notmuch_config_get_bool (notmuch, NOTMUCH_CONFIG_SYNC_MAILDIR_FLAGS,
				     &synchronize_flags)))
	return EXIT_FAILURE;

    if (synchronize_flags)
	tag_flags |= TAG_FLAG_MAILDIR_SYNC;

    if (remove_all)
	tag_flags |= TAG_FLAG_REMOVE_ALL;

    if (batch)
	ret = tag_file (notmuch, notmuch, tag_flags, input);
    else
	ret = tag_query (notmuch, notmuch, query_string, tag_ops, tag_flags);

    notmuch_database_destroy (notmuch);

    if (input != stdin)
	fclose (input);

    return ret || interrupted ? EXIT_FAILURE : EXIT_SUCCESS;
}
