/* message.c - Utility functions for parsing an email message for notmuch.
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

#include <stdarg.h>

#include "notmuch-private.h"

#include <gmime/gmime.h>

#include <glib.h> /* GHashTable */

struct _notmuch_message_file {
    /* File object */
    FILE *file;
    char *filename;

    /* Cache for decoded headers */
    GHashTable *headers;

    GMimeMessage *message;
};

static int
_notmuch_message_file_destructor (notmuch_message_file_t *message)
{
    if (message->headers)
	g_hash_table_destroy (message->headers);

    if (message->message)
	g_object_unref (message->message);

    if (message->file)
	fclose (message->file);

    return 0;
}

/* Create a new notmuch_message_file_t for 'filename' with 'ctx' as
 * the talloc owner. */
notmuch_message_file_t *
_notmuch_message_file_open_ctx (notmuch_database_t *notmuch,
				void *ctx, const char *filename)
{
    notmuch_message_file_t *message;

    message = talloc_zero (ctx, notmuch_message_file_t);
    if (unlikely (message == NULL))
	return NULL;

    /* Only needed for error messages during parsing. */
    message->filename = talloc_strdup (message, filename);
    if (message->filename == NULL)
	goto FAIL;

    talloc_set_destructor (message, _notmuch_message_file_destructor);

    message->file = fopen (filename, "r");
    if (message->file == NULL)
	goto FAIL;

    return message;

  FAIL:
    _notmuch_database_log (notmuch, "Error opening %s: %s\n",
			  filename, strerror (errno));
    _notmuch_message_file_close (message);

    return NULL;
}

notmuch_message_file_t *
_notmuch_message_file_open (notmuch_database_t *notmuch,
			    const char *filename)
{
    return _notmuch_message_file_open_ctx (notmuch, NULL, filename);
}

void
_notmuch_message_file_close (notmuch_message_file_t *message)
{
    talloc_free (message);
}

static notmuch_bool_t
_is_mbox (FILE *file)
{
    char from_buf[5];
    notmuch_bool_t ret = FALSE;

    /* Is this mbox? */
    if (fread (from_buf, sizeof (from_buf), 1, file) == 1 &&
	strncmp (from_buf, "From ", 5) == 0)
	ret = TRUE;

    rewind (file);

    return ret;
}

notmuch_status_t
_notmuch_message_file_parse (notmuch_message_file_t *message)
{
    GMimeStream *stream;
    GMimeParser *parser;
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;
    static int initialized = 0;
    notmuch_bool_t is_mbox;

    if (message->message)
	return NOTMUCH_STATUS_SUCCESS;

    is_mbox = _is_mbox (message->file);

    if (! initialized) {
	g_mime_init (GMIME_ENABLE_RFC2047_WORKAROUNDS);
	initialized = 1;
    }

    message->headers = g_hash_table_new_full (strcase_hash, strcase_equal,
					      free, g_free);
    if (! message->headers)
	return NOTMUCH_STATUS_OUT_OF_MEMORY;

    stream = g_mime_stream_file_new (message->file);

    /* We'll own and fclose the FILE* ourselves. */
    g_mime_stream_file_set_owner (GMIME_STREAM_FILE (stream), FALSE);

    parser = g_mime_parser_new_with_stream (stream);
    g_mime_parser_set_scan_from (parser, is_mbox);

    message->message = g_mime_parser_construct_message (parser);
    if (! message->message) {
	status = NOTMUCH_STATUS_FILE_NOT_EMAIL;
	goto DONE;
    }

    if (is_mbox && ! g_mime_parser_eos (parser)) {
	/*
	 * This is a multi-message mbox. (For historical reasons, we
	 * do support single-message mboxes.)
	 */
	status = NOTMUCH_STATUS_FILE_NOT_EMAIL;
    }

  DONE:
    g_object_unref (stream);
    g_object_unref (parser);

    if (status) {
	g_hash_table_destroy (message->headers);
	message->headers = NULL;

	if (message->message) {
	    g_object_unref (message->message);
	    message->message = NULL;
	}

	rewind (message->file);
    }

    return status;
}

notmuch_status_t
_notmuch_message_file_get_mime_message (notmuch_message_file_t *message,
					GMimeMessage **mime_message)
{
    notmuch_status_t status;

    status = _notmuch_message_file_parse (message);
    if (status)
	return status;

    *mime_message = message->message;

    return NOTMUCH_STATUS_SUCCESS;
}

/*
 * Get all instances of a header decoded and concatenated.
 *
 * The result must be freed using g_free().
 *
 * Return NULL on errors, empty string for non-existing headers.
 */

static char *
_extend_header (char *combined, const char *value) {
    char *decoded;

    decoded = g_mime_utils_header_decode_text (value);
    if (! decoded) {
	if (combined) {
	    g_free (combined);
	    combined = NULL;
	}
	goto DONE;
    }

    if (combined) {
	char *tmp = g_strdup_printf ("%s %s", combined, decoded);
	g_free (decoded);
	g_free (combined);
	if (! tmp) {
	    combined = NULL;
	    goto DONE;
	}

	combined = tmp;
    } else {
	combined = decoded;
    }
 DONE:
    return combined;
}

#if (GMIME_MAJOR_VERSION < 3)
static char *
_notmuch_message_file_get_combined_header (notmuch_message_file_t *message,
					   const char *header)
{
    GMimeHeaderList *headers;
    GMimeHeaderIter *iter;
    char *combined = NULL;

    headers = g_mime_object_get_header_list (GMIME_OBJECT (message->message));
    if (! headers)
	return NULL;

    iter = g_mime_header_iter_new ();
    if (! iter)
	return NULL;

    if (! g_mime_header_list_get_iter (headers, iter))
	goto DONE;

    do {
	const char *value;
	if (strcasecmp (g_mime_header_iter_get_name (iter), header) != 0)
	    continue;

	/* Note that GMime retains ownership of value... */
	value = g_mime_header_iter_get_value (iter);

	combined = _extend_header (combined, value);
    } while (g_mime_header_iter_next (iter));

    /* Return empty string for non-existing headers. */
    if (! combined)
	combined = g_strdup ("");

  DONE:
    g_mime_header_iter_free (iter);

    return combined;
}
#else
static char *
_notmuch_message_file_get_combined_header (notmuch_message_file_t *message,
					   const char *header)
{
    char *combined = NULL;
    GMimeHeaderList *headers;

    headers = g_mime_object_get_header_list (GMIME_OBJECT (message->message));
    if (! headers)
	return NULL;


    for (int i=0; i < g_mime_header_list_get_count (headers); i++) {
	const char *value;
	GMimeHeader *g_header = g_mime_header_list_get_header_at (headers, i);

	if (strcasecmp (g_mime_header_get_name (g_header), header) != 0)
	    continue;

	/* GMime retains ownership of value, we hope */
	value = g_mime_header_get_value (g_header);

	combined = _extend_header (combined, value);
    }

    /* Return empty string for non-existing headers. */
    if (! combined)
	combined = g_strdup ("");

    return combined;
}
#endif

const char *
_notmuch_message_file_get_header (notmuch_message_file_t *message,
				 const char *header)
{
    const char *value;
    char *decoded;

    if (_notmuch_message_file_parse (message))
	return NULL;

    /* If we have a cached decoded value, use it. */
    value = g_hash_table_lookup (message->headers, header);
    if (value)
	return value;

    if (strcasecmp (header, "received") == 0) {
	/*
	 * The Received: header is special. We concatenate all
	 * instances of the header as we use this when analyzing the
	 * path the mail has taken from sender to recipient.
	 */
	decoded = _notmuch_message_file_get_combined_header (message, header);
    } else {
	value = g_mime_object_get_header (GMIME_OBJECT (message->message),
					  header);
	if (value)
	    decoded = g_mime_utils_header_decode_text (value);
	else
	    decoded = g_strdup ("");
    }

    if (! decoded)
	return NULL;

    /* Cache the decoded value. We also own the strings. */
    g_hash_table_insert (message->headers, xstrdup (header), decoded);

    return decoded;
}
