/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2016 Daniel Kahn Gillmor
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
 * Author: Daniel Kahn Gillmor <dkg@fifthhorseman.net>
 */

#include "notmuch-client.h"
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

/* reindex all messages matching 'query_string' using the passed-in indexopts
 */
static int
reindex_query (notmuch_database_t *notmuch, const char *query_string,
	       notmuch_indexopts_t *indexopts)
{
    notmuch_query_t *query;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    notmuch_status_t status;

    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;

    query = notmuch_query_create (notmuch, query_string);
    if (query == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    /* reindexing is not interested in any special sort order */
    notmuch_query_set_sort (query, NOTMUCH_SORT_UNSORTED);

    status = notmuch_query_search_messages (query, &messages);
    if (print_status_query ("notmuch reindex", query, status))
	return status;

    ret = notmuch_database_begin_atomic (notmuch);
    for (;
	 notmuch_messages_valid (messages) && ! interrupted;
	 notmuch_messages_move_to_next (messages)) {
	message = notmuch_messages_get (messages);

	ret = notmuch_message_reindex (message, indexopts);
	if (ret != NOTMUCH_STATUS_SUCCESS)
	    break;
	notmuch_message_destroy (message);
    }

    if (! ret)
	ret = notmuch_database_end_atomic (notmuch);

    notmuch_query_destroy (query);

    return ret || interrupted;
}

int
notmuch_reindex_command (unused(notmuch_config_t *config), notmuch_database_t *notmuch, int argc, char *argv[])
{
    char *query_string = NULL;
    struct sigaction action;
    int opt_index;
    int ret;
    notmuch_status_t status;

    /* Set up our handler for SIGINT */
    memset (&action, 0, sizeof (struct sigaction));
    action.sa_handler = handle_sigint;
    sigemptyset (&action.sa_mask);
    action.sa_flags = SA_RESTART;
    sigaction (SIGINT, &action, NULL);

    notmuch_opt_desc_t options[] = {
	{ .opt_inherit = notmuch_shared_indexing_options },
	{ .opt_inherit = notmuch_shared_options },
	{ }
    };

    opt_index = parse_arguments (argc, argv, options, 1);
    if (opt_index < 0)
	return EXIT_FAILURE;

    notmuch_process_shared_options (argv[0]);

    notmuch_exit_if_unmatched_db_uuid (notmuch);

    status = notmuch_process_shared_indexing_options (notmuch);
    if (status != NOTMUCH_STATUS_SUCCESS) {
	fprintf (stderr, "Error: Failed to process index options. (%s)\n",
		 notmuch_status_to_string (status));
	return EXIT_FAILURE;
    }

    query_string = query_string_from_args (notmuch, argc - opt_index, argv + opt_index);
    if (query_string == NULL) {
	fprintf (stderr, "Out of memory\n");
	return EXIT_FAILURE;
    }

    if (*query_string == '\0') {
	fprintf (stderr, "Error: notmuch reindex requires at least one search term.\n");
	return EXIT_FAILURE;
    }

    ret = reindex_query (notmuch, query_string, indexing_cli_choices.opts);

    notmuch_database_destroy (notmuch);

    return ret || interrupted ? EXIT_FAILURE : EXIT_SUCCESS;
}
