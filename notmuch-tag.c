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

static volatile sig_atomic_t interrupted;

static void
handle_sigint (unused (int sig))
{
    ssize_t ignored;

    static char msg[] = "Stopping...         \n";
    ignored = write(2, msg, sizeof(msg)-1);
    interrupted = 1;
}

int
notmuch_tag_command (void *ctx, unused (int argc), unused (char *argv[]))
{
    int *add_tags, *remove_tags;
    int add_tags_count = 0;
    int remove_tags_count = 0;
    char *query_string;
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    notmuch_query_t *query;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    struct sigaction action;
    int i;

    /* Setup our handler for SIGINT */
    memset (&action, 0, sizeof (struct sigaction));
    action.sa_handler = handle_sigint;
    sigemptyset (&action.sa_mask);
    action.sa_flags = SA_RESTART;
    sigaction (SIGINT, &action, NULL);

    add_tags = talloc_size (ctx, argc * sizeof (int));
    if (add_tags == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    remove_tags = talloc_size (ctx, argc * sizeof (int));
    if (remove_tags == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    for (i = 0; i < argc; i++) {
	if (strcmp (argv[i], "--") == 0) {
	    i++;
	    break;
	}
	if (argv[i][0] == '+') {
	    add_tags[add_tags_count++] = i;
	} else if (argv[i][0] == '-') {
	    remove_tags[remove_tags_count++] = i;
	} else {
	    break;
	}
    }

    if (add_tags_count == 0 && remove_tags_count == 0) {
	fprintf (stderr, "Error: 'notmuch tag' requires at least one tag to add or remove.\n");
	return 1;
    }

    query_string = query_string_from_args (ctx, argc - i, &argv[i]);

    if (*query_string == '\0') {
	fprintf (stderr, "Error: notmuch tag requires at least one search term.\n");
	return 1;
    }

    config = notmuch_config_open (ctx, NULL, NULL);
    if (config == NULL)
	return 1;

    notmuch = notmuch_database_open (notmuch_config_get_database_path (config),
				     NOTMUCH_DATABASE_MODE_READ_WRITE);
    if (notmuch == NULL)
	return 1;
    notmuch_database_set_maildir_sync (notmuch,
				       notmuch_config_get_maildir_sync (config));

    query = notmuch_query_create (notmuch, query_string);
    if (query == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    /* tagging is not interested in any special sort order */
    notmuch_query_set_sort (query, NOTMUCH_SORT_UNSORTED);

    for (messages = notmuch_query_search_messages (query);
	 notmuch_messages_valid (messages) && !interrupted;
	 notmuch_messages_move_to_next (messages))
    {
	message = notmuch_messages_get (messages);

	notmuch_message_freeze (message);

	for (i = 0; i < remove_tags_count; i++)
	    notmuch_message_remove_tag (message,
					argv[remove_tags[i]] + 1);

	for (i = 0; i < add_tags_count; i++)
	    notmuch_message_add_tag (message, argv[add_tags[i]] + 1);

	notmuch_message_thaw (message);

	notmuch_message_destroy (message);
    }

    notmuch_query_destroy (query);
    notmuch_database_close (notmuch);

    return interrupted;
}
