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
notmuch_tag_command (void *ctx, unused (int argc), unused (char *argv[]))
{
    void *local;
    int *add_tags, *remove_tags;
    int add_tags_count = 0;
    int remove_tags_count = 0;
    char *query_string;
    notmuch_database_t *notmuch = NULL;
    notmuch_query_t *query;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    int ret = 0;
    int i;

    local = talloc_new (ctx);
    if (local == NULL) {
	ret = 1;
	goto DONE;
    }

    add_tags = talloc_size (local, argc * sizeof (int));
    if (add_tags == NULL) {
	ret = 1;
	goto DONE;
    }

    remove_tags = talloc_size (local, argc * sizeof (int));
    if (remove_tags == NULL) {
	ret = 1;
	goto DONE;
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
	ret = 1;
	goto DONE;
    }

    if (i == argc) {
	fprintf (stderr, "Error: 'notmuch tag' requires at least one search term.\n");
	ret = 1;
	goto DONE;
    }

    notmuch = notmuch_database_open (NULL);
    if (notmuch == NULL) {
	ret = 1;
	goto DONE;
    }

    query_string = query_string_from_args (local, argc - i, &argv[i]);

    query = notmuch_query_create (notmuch, query_string);
    if (query == NULL) {
	fprintf (stderr, "Out of memory.\n");
	ret = 1;
	goto DONE;
    }

    for (messages = notmuch_query_search_messages (query);
	 notmuch_messages_has_more (messages);
	 notmuch_messages_advance (messages))
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

  DONE:
    if (notmuch)
	notmuch_database_close (notmuch);

    talloc_free (local);

    return ret;
}
