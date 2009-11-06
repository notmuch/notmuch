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
 * Authors: Carl Worth <cworth@cworth.org>
 *	    Keith Packard <keithp@keithp.com>
 */

#include "notmuch-client.h"
#include "gmime-filter-reply.h"

static void
reply_part(GMimeObject *part, int *part_count)
{
    GMimeContentDisposition *disposition;
    GMimeContentType *content_type;
    GMimeDataWrapper *wrapper;

    (void) part_count;
    disposition = g_mime_object_get_content_disposition (part);
    if (disposition &&
	strcmp (disposition->disposition, GMIME_DISPOSITION_ATTACHMENT) == 0)
    {
	const char *filename = g_mime_part_get_filename (GMIME_PART (part));
	content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

	printf ("Attachment: %s (%s)\n", filename,
		g_mime_content_type_to_string (content_type));
	return;
    }

    content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

    if (g_mime_content_type_is_type (content_type, "text", "*") &&
	!g_mime_content_type_is_type (content_type, "text", "html"))
    {
	GMimeStream *stream_stdout = NULL, *stream_filter = NULL;
	stream_stdout = g_mime_stream_file_new (stdout);
	if (stream_stdout) {
	    g_mime_stream_file_set_owner (GMIME_STREAM_FILE (stream_stdout), FALSE);
	    stream_filter = g_mime_stream_filter_new(stream_stdout);
	}
	g_mime_stream_filter_add(GMIME_STREAM_FILTER(stream_filter),
				 g_mime_filter_reply_new(TRUE));
	wrapper = g_mime_part_get_content_object (GMIME_PART (part));
	if (wrapper && stream_filter)
	    g_mime_data_wrapper_write_to_stream (wrapper, stream_filter);
	if (stream_filter)
	    g_object_unref(stream_filter);
	if (stream_stdout)
	    g_object_unref(stream_stdout);
    }
    else
    {
	printf ("Non-text part: %s\n",
		g_mime_content_type_to_string (content_type));
    }
}

int
notmuch_reply_command (void *ctx, int argc, char *argv[])
{
    void *local = talloc_new (ctx);
    char *query_string;
    notmuch_database_t *notmuch = NULL;
    notmuch_query_t *query = NULL;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    int ret = 0;

    const char *headers[] = {
	    "Subject", "From", "To", "Cc", "Bcc", "Date",
	    "In-Reply-To", "References"
    };
    const char *name, *value;
    unsigned int i;

    notmuch = notmuch_database_open (NULL);
    if (notmuch == NULL) {
	ret = 1;
	goto DONE;
    }

    query_string = query_string_from_args (local, argc, argv);
    if (query_string == NULL) {
	fprintf (stderr, "Out of memory\n");
	ret = 1;
	goto DONE;
    }

    query = notmuch_query_create (notmuch, query_string);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	ret = 1;
	goto DONE;
    }

    for (messages = notmuch_query_search_messages (query);
	 notmuch_messages_has_more (messages);
	 notmuch_messages_advance (messages))
    {
	message = notmuch_messages_get (messages);

	for (i = 0; i < ARRAY_SIZE (headers); i++) {
	    name = headers[i];
	    value = notmuch_message_get_header (message, name);
	    if (value)
		printf ("%s: %s\n", name, value);
	}

	show_message_body (notmuch_message_get_filename (message), reply_part);

	notmuch_message_destroy (message);
    }

  DONE:
    if (local)
	talloc_free (local);

    if (query)
	notmuch_query_destroy (query);

    if (notmuch)
	notmuch_database_close (notmuch);

    return ret;
}
