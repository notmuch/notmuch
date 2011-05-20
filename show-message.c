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

static void
show_message_part (GMimeObject *part,
		   int *part_count,
		   const notmuch_show_format_t *format,
		   int first)
{
    if (GMIME_IS_MULTIPART (part)) {
	GMimeMultipart *multipart = GMIME_MULTIPART (part);
	int i;

	*part_count = *part_count + 1;
	format->part (part, part_count, first);

	for (i = 0; i < g_mime_multipart_get_count (multipart); i++) {
	    show_message_part (g_mime_multipart_get_part (multipart, i),
			       part_count, format, i == 0);
	}

	if (format->part_end)
	    format->part_end (part);

	return;
    }

    if (GMIME_IS_MESSAGE_PART (part)) {
	GMimeMessage *mime_message;

	mime_message = g_mime_message_part_get_message (GMIME_MESSAGE_PART (part));

	show_message_part (g_mime_message_get_mime_part (mime_message),
			   part_count, format, first);

	return;
    }

    if (! (GMIME_IS_PART (part))) {
	fprintf (stderr, "Warning: Not displaying unknown mime part: %s.\n",
		 g_type_name (G_OBJECT_TYPE (part)));
	return;
    }

    *part_count = *part_count + 1;

    format->part (part, part_count, first);
    if (format->part_end)
	format->part_end (part);
}

notmuch_status_t
show_message_body (const char *filename,
		   const notmuch_show_format_t *format)
{
    GMimeStream *stream = NULL;
    GMimeParser *parser = NULL;
    GMimeMessage *mime_message = NULL;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;
    FILE *file = NULL;
    int part_count = 0;

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
		       &part_count,
		       format,
		       TRUE);

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

static void
show_one_part_output (GMimeObject *part)
{
    GMimeStream *stream_filter = NULL;
    GMimeDataWrapper *wrapper;
    GMimeStream *stream_stdout = g_mime_stream_file_new (stdout);

    stream_filter = g_mime_stream_filter_new(stream_stdout);
    wrapper = g_mime_part_get_content_object (GMIME_PART (part));
    if (wrapper && stream_filter)
	g_mime_data_wrapper_write_to_stream (wrapper, stream_filter);
    if (stream_filter)
	g_object_unref(stream_filter);
}

static void
show_one_part_worker (GMimeObject *part, int *part_count, int desired_part)
{
    if (GMIME_IS_MULTIPART (part)) {
	GMimeMultipart *multipart = GMIME_MULTIPART (part);
	int i;

	for (i = 0; i < g_mime_multipart_get_count (multipart); i++) {
		show_one_part_worker (g_mime_multipart_get_part (multipart, i),
				      part_count, desired_part);
	}
	return;
    }

    if (GMIME_IS_MESSAGE_PART (part)) {
	GMimeMessage *mime_message;

	mime_message = g_mime_message_part_get_message (GMIME_MESSAGE_PART (part));

	show_one_part_worker (g_mime_message_get_mime_part (mime_message),
			      part_count, desired_part);

	return;
    }

    if (! (GMIME_IS_PART (part)))
	return;

    *part_count = *part_count + 1;

    if (*part_count == desired_part)
	    show_one_part_output (part);
}

notmuch_status_t
show_one_part (const char *filename, int part)
{
	GMimeStream *stream = NULL;
	GMimeParser *parser = NULL;
	GMimeMessage *mime_message = NULL;
	notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;
	FILE *file = NULL;
	int part_count = 0;

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

	show_one_part_worker (g_mime_message_get_mime_part (mime_message),
			      &part_count, part);

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
