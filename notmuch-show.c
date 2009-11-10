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

static const char *
_get_tags_as_string (void *ctx, notmuch_message_t *message)
{
    notmuch_tags_t *tags;
    int first = 1;
    const char *tag;
    char *result;

    result = talloc_strdup (ctx, "");
    if (result == NULL)
	return NULL;

    for (tags = notmuch_message_get_tags (message);
	 notmuch_tags_has_more (tags);
	 notmuch_tags_advance (tags))
    {
	tag = notmuch_tags_get (tags);

	result = talloc_asprintf_append (result, "%s%s",
					 first ? "" : " ", tag);
	first = 0;
    }

    return result;
}

/* Get a nice, single-line summary of message. */
static const char *
_get_one_line_summary (void *ctx, notmuch_message_t *message)
{
    const char *from;
    time_t date;
    const char *relative_date;
    const char *tags;

    from = notmuch_message_get_header (message, "from");

    date = notmuch_message_get_date (message);
    relative_date = notmuch_time_relative_date (ctx, date);

    tags = _get_tags_as_string (ctx, message);

    return talloc_asprintf (ctx, "%s (%s) (%s)",
			    from, relative_date, tags);
}

static void
show_message_part (GMimeObject *part, int *part_count)
{
    GMimeStream *stream;
    GMimeDataWrapper *wrapper;
    GMimeContentDisposition *disposition;
    GMimeContentType *content_type;

    *part_count = *part_count + 1;

    if (GMIME_IS_MULTIPART (part)) {
	GMimeMultipart *multipart = GMIME_MULTIPART (part);
	int i;

	for (i = 0; i < g_mime_multipart_get_count (multipart); i++) {
	    if (GMIME_IS_MULTIPART_SIGNED (multipart)) {
		/* Don't index the signature. */
		if (i == 1)
		    continue;
		if (i > 1)
		    fprintf (stderr, "Warning: Unexpected extra parts of mutlipart/signed. Continuing.\n");
	    }
	    show_message_part (g_mime_multipart_get_part (multipart, i),
			       part_count);
	}
	return;
    }

    if (GMIME_IS_MESSAGE_PART (part)) {
	GMimeMessage *mime_message;

	mime_message = g_mime_message_part_get_message (GMIME_MESSAGE_PART (part));

	show_message_part (g_mime_message_get_mime_part (mime_message),
			   part_count);

	return;
    }

    if (! (GMIME_IS_PART (part))) {
	fprintf (stderr, "Warning: Not displaying unknown mime part: %s.\n",
		 g_type_name (G_OBJECT_TYPE (part)));
	return;
    }

    disposition = g_mime_object_get_content_disposition (part);
    if (disposition &&
	strcmp (disposition->disposition, GMIME_DISPOSITION_ATTACHMENT) == 0)
    {
	const char *filename = g_mime_part_get_filename (GMIME_PART (part));
	content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

	printf ("\fattachment{ ID: %d, Content-type: %s\n",
		*part_count,
		g_mime_content_type_to_string (content_type));
	printf ("Attachment: %s (%s)\n", filename,
		g_mime_content_type_to_string (content_type));
	printf ("\fattachment}\n");

	return;
    }

    content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

    printf ("\fpart{ ID: %d, Content-type: %s\n",
	    *part_count,
	    g_mime_content_type_to_string (content_type));

    if (g_mime_content_type_is_type (content_type, "text", "*") &&
	!g_mime_content_type_is_type (content_type, "text", "html"))
    {
	stream = g_mime_stream_file_new (stdout);
	g_mime_stream_file_set_owner (GMIME_STREAM_FILE (stream), FALSE);

	wrapper = g_mime_part_get_content_object (GMIME_PART (part));
	if (wrapper)
	    g_mime_data_wrapper_write_to_stream (wrapper, stream);

	g_object_unref (stream);
    }
    else
    {
	printf ("Non-text part: %s\n",
		g_mime_content_type_to_string (content_type));
    }

    printf ("\fpart}\n");
}

static notmuch_status_t
show_message_body (const char *filename)
{
    GMimeStream *stream = NULL;
    GMimeParser *parser = NULL;
    GMimeMessage *mime_message = NULL;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;
    static int initialized = 0;
    FILE *file = NULL;
    int part_count = 0;

    if (! initialized) {
	g_mime_init (0);
	initialized = 1;
    }

    file = fopen (filename, "r");
    if (! file) {
	fprintf (stderr, "Error opening %s: %s\n", filename, strerror (errno));
	ret = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    stream = g_mime_stream_file_new (file);
    g_mime_stream_file_set_owner (GMIME_STREAM_FILE (stream), FALSE);

    parser = g_mime_parser_new_with_stream (stream);

    mime_message = g_mime_parser_construct_message (parser);

    show_message_part (g_mime_message_get_mime_part (mime_message),
		       &part_count);

  DONE:
    if (mime_message)
	g_object_unref (mime_message);

    if (parser)
	g_object_unref (parser);

    if (stream)
	g_object_unref (stream);

    if (file)
	fclose (file);

    return ret;
}

int
notmuch_show_command (void *ctx, unused (int argc), unused (char *argv[]))
{
    void *local = talloc_new (ctx);
    char *query_string;
    notmuch_database_t *notmuch = NULL;
    notmuch_query_t *query = NULL;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    int ret = 0;

    const char *headers[] = {
	"From", "To", "Cc", "Bcc", "Date"
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

	printf ("\fmessage{ id:%s filename:%s\n",
		notmuch_message_get_message_id (message),
		notmuch_message_get_filename (message));

	printf ("\fheader{\n");

	printf ("%s\n", _get_one_line_summary (local, message));

	printf ("%s\n", notmuch_message_get_header (message, "subject"));

	for (i = 0; i < ARRAY_SIZE (headers); i++) {
	    name = headers[i];
	    value = notmuch_message_get_header (message, name);
	    if (value)
		printf ("%s: %s\n", name, value);
	}

	printf ("\fheader}\n");
	printf ("\fbody{\n");

	show_message_body (notmuch_message_get_filename (message));

	printf ("\fbody}\n");

	printf ("\fmessage}\n");

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
