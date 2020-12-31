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
    /* open stream to (possibly gzipped) file */
    GMimeStream *stream;
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

    if (message->stream)
	g_object_unref (message->stream);

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

    const char *prefix = notmuch_config_get (notmuch, NOTMUCH_CONFIG_MAIL_ROOT);

    if (prefix == NULL)
	goto FAIL;

    if (*filename == '/') {
	if (strncmp (filename, prefix, strlen (prefix)) != 0) {
	    _notmuch_database_log (notmuch, "Error opening %s: path outside mail root\n",
				   filename);
	    errno = 0;
	    goto FAIL;
	}
	message->filename = talloc_strdup (message, filename);
    } else {
	message->filename = talloc_asprintf (message, "%s/%s", prefix, filename);
    }

    if (message->filename == NULL)
	goto FAIL;

    talloc_set_destructor (message, _notmuch_message_file_destructor);

    message->stream = g_mime_stream_gzfile_open (message->filename);
    if (message->stream == NULL)
	goto FAIL;

    return message;

  FAIL:
    if (errno)
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

const char *
_notmuch_message_file_get_filename (notmuch_message_file_t *message_file)
{
    return message_file->filename;
}

void
_notmuch_message_file_close (notmuch_message_file_t *message)
{
    talloc_free (message);
}

static bool
_is_mbox (GMimeStream *stream)
{
    char from_buf[5];
    bool ret = false;

    /* Is this mbox? */
    if (g_mime_stream_read (stream, from_buf, sizeof (from_buf)) == sizeof (from_buf) &&
	strncmp (from_buf, "From ", 5) == 0)
	ret = true;

    g_mime_stream_reset (stream);

    return ret;
}

notmuch_status_t
_notmuch_message_file_parse (notmuch_message_file_t *message)
{
    GMimeParser *parser;
    notmuch_status_t status = NOTMUCH_STATUS_SUCCESS;
    static int initialized = 0;
    bool is_mbox;

    if (message->message)
	return NOTMUCH_STATUS_SUCCESS;

    is_mbox = _is_mbox (message->stream);

    if (! initialized) {
	g_mime_init ();
	initialized = 1;
    }

    message->headers = g_hash_table_new_full (strcase_hash, strcase_equal,
					      free, g_free);
    if (! message->headers)
	return NOTMUCH_STATUS_OUT_OF_MEMORY;

    parser = g_mime_parser_new_with_stream (message->stream);
    g_mime_parser_set_scan_from (parser, is_mbox);

    message->message = g_mime_parser_construct_message (parser, NULL);
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
    g_mime_stream_reset (message->stream);
    g_object_unref (parser);

    if (status) {
	g_hash_table_destroy (message->headers);
	message->headers = NULL;

	if (message->message) {
	    g_object_unref (message->message);
	    message->message = NULL;
	}

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
_extend_header (char *combined, const char *value)
{
    char *decoded;

    decoded = g_mime_utils_header_decode_text (NULL, value);
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

static char *
_notmuch_message_file_get_combined_header (notmuch_message_file_t *message,
					   const char *header)
{
    char *combined = NULL;
    GMimeHeaderList *headers;

    headers = g_mime_object_get_header_list (GMIME_OBJECT (message->message));
    if (! headers)
	return NULL;


    for (int i = 0; i < g_mime_header_list_get_count (headers); i++) {
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
	    decoded = g_mime_utils_header_decode_text (NULL, value);
	else
	    decoded = g_strdup ("");
    }

    if (! decoded)
	return NULL;

    /* Cache the decoded value. We also own the strings. */
    g_hash_table_insert (message->headers, xstrdup (header), decoded);

    return decoded;
}

notmuch_status_t
_notmuch_message_file_get_headers (notmuch_message_file_t *message_file,
				   const char **from_out,
				   const char **subject_out,
				   const char **to_out,
				   const char **date_out,
				   char **message_id_out)
{
    notmuch_status_t ret;
    const char *header;
    const char *from, *to, *subject, *date;
    char *message_id = NULL;

    /* Parse message up front to get better error status. */
    ret = _notmuch_message_file_parse (message_file);
    if (ret)
	goto DONE;

    /* Before we do any real work, (especially before doing a
     * potential SHA-1 computation on the entire file's contents),
     * let's make sure that what we're looking at looks like an
     * actual email message.
     */
    from = _notmuch_message_file_get_header (message_file, "from");
    subject = _notmuch_message_file_get_header (message_file, "subject");
    to = _notmuch_message_file_get_header (message_file, "to");
    date = _notmuch_message_file_get_header (message_file, "date");

    if ((from == NULL || *from == '\0') &&
	(subject == NULL || *subject == '\0') &&
	(to == NULL || *to == '\0')) {
	ret = NOTMUCH_STATUS_FILE_NOT_EMAIL;
	goto DONE;
    }

    /* Now that we're sure it's mail, the first order of business
     * is to find a message ID (or else create one ourselves).
     */
    header = _notmuch_message_file_get_header (message_file, "message-id");
    if (header && *header != '\0') {
	message_id = _notmuch_message_id_parse (message_file, header, NULL);

	/* So the header value isn't RFC-compliant, but it's
	 * better than no message-id at all.
	 */
	if (message_id == NULL)
	    message_id = talloc_strdup (message_file, header);
    }

    if (message_id == NULL ) {
	/* No message-id at all, let's generate one by taking a
	 * hash over the file's contents.
	 */
	char *sha1 = _notmuch_sha1_of_file (_notmuch_message_file_get_filename (message_file));

	/* If that failed too, something is really wrong. Give up. */
	if (sha1 == NULL) {
	    ret = NOTMUCH_STATUS_FILE_ERROR;
	    goto DONE;
	}

	message_id = talloc_asprintf (message_file, "notmuch-sha1-%s", sha1);
	free (sha1);
    }
  DONE:
    if (ret == NOTMUCH_STATUS_SUCCESS) {
	if (from_out)
	    *from_out = from;
	if (subject_out)
	    *subject_out = subject;
	if (to_out)
	    *to_out = to;
	if (date_out)
	    *date_out = date;
	if (message_id_out)
	    *message_id_out = message_id;
    }
    return ret;
}
