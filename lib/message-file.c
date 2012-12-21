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
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include <stdarg.h>

#include "notmuch-private.h"

#include <gmime/gmime.h>

#include <glib.h> /* GHashTable */

typedef struct {
    char *str;
    size_t size;
    size_t len;
} header_value_closure_t;

struct _notmuch_message_file {
    /* File object */
    FILE *file;

    /* Header storage */
    int restrict_headers;
    GHashTable *headers;
    int broken_headers;
    int good_headers;
    size_t header_size; /* Length of full message header in bytes. */

    /* Parsing state */
    char *line;
    size_t line_size;
    header_value_closure_t value;

    int parsing_started;
    int parsing_finished;
};

static int
strcase_equal (const void *a, const void *b)
{
    return strcasecmp (a, b) == 0;
}

static unsigned int
strcase_hash (const void *ptr)
{
    const char *s = ptr;

    /* This is the djb2 hash. */
    unsigned int hash = 5381;
    while (s && *s) {
	hash = ((hash << 5) + hash) + tolower (*s);
	s++;
    }

    return hash;
}

static int
_notmuch_message_file_destructor (notmuch_message_file_t *message)
{
    if (message->line)
	free (message->line);

    if (message->value.size)
	free (message->value.str);

    if (message->headers)
	g_hash_table_destroy (message->headers);

    if (message->file)
	fclose (message->file);

    return 0;
}

/* Create a new notmuch_message_file_t for 'filename' with 'ctx' as
 * the talloc owner. */
notmuch_message_file_t *
_notmuch_message_file_open_ctx (void *ctx, const char *filename)
{
    notmuch_message_file_t *message;

    message = talloc_zero (ctx, notmuch_message_file_t);
    if (unlikely (message == NULL))
	return NULL;

    talloc_set_destructor (message, _notmuch_message_file_destructor);

    message->file = fopen (filename, "r");
    if (message->file == NULL)
	goto FAIL;

    message->headers = g_hash_table_new_full (strcase_hash,
					      strcase_equal,
					      free,
					      g_free);

    message->parsing_started = 0;
    message->parsing_finished = 0;

    return message;

  FAIL:
    fprintf (stderr, "Error opening %s: %s\n", filename, strerror (errno));
    notmuch_message_file_close (message);

    return NULL;
}

notmuch_message_file_t *
notmuch_message_file_open (const char *filename)
{
    return _notmuch_message_file_open_ctx (NULL, filename);
}

void
notmuch_message_file_close (notmuch_message_file_t *message)
{
    talloc_free (message);
}

void
notmuch_message_file_restrict_headersv (notmuch_message_file_t *message,
					va_list va_headers)
{
    char *header;

    if (message->parsing_started)
	INTERNAL_ERROR ("notmuch_message_file_restrict_headers called after parsing has started");

    while (1) {
	header = va_arg (va_headers, char*);
	if (header == NULL)
	    break;
	g_hash_table_insert (message->headers,
			     xstrdup (header), NULL);
    }

    message->restrict_headers = 1;
}

void
notmuch_message_file_restrict_headers (notmuch_message_file_t *message, ...)
{
    va_list va_headers;

    va_start (va_headers, message);

    notmuch_message_file_restrict_headersv (message, va_headers);
}

static void
copy_header_unfolding (header_value_closure_t *value,
		       const char *chunk)
{
    char *last;

    if (chunk == NULL)
	return;

    while (*chunk == ' ' || *chunk == '\t')
	chunk++;

    if (value->len + 1 + strlen (chunk) + 1 > value->size) {
	unsigned int new_size = value->size;
	if (value->size == 0)
	    new_size = strlen (chunk) + 1;
	else
	    while (value->len + 1 + strlen (chunk) + 1 > new_size)
		new_size *= 2;
	value->str = xrealloc (value->str, new_size);
	value->size = new_size;
    }

    last = value->str + value->len;
    if (value->len) {
	*last = ' ';
	last++;
	value->len++;
    }

    strcpy (last, chunk);
    value->len += strlen (chunk);

    last = value->str + value->len - 1;
    if (*last == '\n') {
	*last = '\0';
	value->len--;
    }
}

/* As a special-case, a value of NULL for header_desired will force
 * the entire header to be parsed if it is not parsed already. This is
 * used by the _notmuch_message_file_get_headers_end function.
 * Another special case is the Received: header. For this header we
 * want to concatenate all instances of the header instead of just
 * hashing the first instance as we use this when analyzing the path
 * the mail has taken from sender to recipient.
 */
const char *
notmuch_message_file_get_header (notmuch_message_file_t *message,
				 const char *header_desired)
{
    int contains;
    char *header, *decoded_value, *header_sofar, *combined_header;
    const char *s, *colon;
    int match, newhdr, hdrsofar, is_received;
    static int initialized = 0;

    is_received = (strcmp(header_desired,"received") == 0);

    if (! initialized) {
	g_mime_init (0);
	initialized = 1;
    }

    message->parsing_started = 1;

    if (header_desired == NULL)
	contains = 0;
    else
	contains = g_hash_table_lookup_extended (message->headers,
						 header_desired, NULL,
						 (gpointer *) &decoded_value);

    if (contains && decoded_value)
	return decoded_value;

    if (message->parsing_finished)
	return "";

#define NEXT_HEADER_LINE(closure)				\
    while (1) {							\
	ssize_t bytes_read = getline (&message->line,		\
				      &message->line_size,	\
				      message->file);		\
	if (bytes_read == -1) {					\
	    message->parsing_finished = 1;			\
	    break;						\
	}							\
	if (*message->line == '\n') {				\
	    message->parsing_finished = 1;			\
	    break;						\
	}							\
	if (closure &&						\
	    (*message->line == ' ' || *message->line == '\t'))	\
	{							\
	    copy_header_unfolding ((closure), message->line);	\
	}							\
	if (*message->line == ' ' || *message->line == '\t')	\
	    message->header_size += strlen (message->line);	\
	else							\
	    break;						\
    }

    if (message->line == NULL)
	NEXT_HEADER_LINE (NULL);

    while (1) {

	if (message->parsing_finished)
	    break;

	colon = strchr (message->line, ':');

	if (colon == NULL) {
	    message->broken_headers++;
	    /* A simple heuristic for giving up on things that just
	     * don't look like mail messages. */
	    if (message->broken_headers >= 10 &&
		message->good_headers < 5)
	    {
		message->parsing_finished = 1;
		break;
	    }
	    NEXT_HEADER_LINE (NULL);
	    continue;
	}

	message->header_size += strlen (message->line);

	message->good_headers++;

	header = xstrndup (message->line, colon - message->line);

	if (message->restrict_headers &&
	    ! g_hash_table_lookup_extended (message->headers,
					    header, NULL, NULL))
	{
	    free (header);
	    NEXT_HEADER_LINE (NULL);
	    continue;
	}

	s = colon + 1;
	while (*s == ' ' || *s == '\t')
	    s++;

	message->value.len = 0;
	copy_header_unfolding (&message->value, s);

	NEXT_HEADER_LINE (&message->value);

	if (header_desired == NULL)
	    match = 0;
	else
	    match = (strcasecmp (header, header_desired) == 0);

	decoded_value = g_mime_utils_header_decode_text (message->value.str);
	header_sofar = (char *)g_hash_table_lookup (message->headers, header);
	/* we treat the Received: header special - we want to concat ALL of 
	 * the Received: headers we encounter.
	 * for everything else we return the first instance of a header */
	if (strcasecmp(header, "received") == 0) {
	    if (header_sofar == NULL) {
		/* first Received: header we encountered; just add it */
		g_hash_table_insert (message->headers, header, decoded_value);
	    } else {
		/* we need to add the header to those we already collected */
		newhdr = strlen(decoded_value);
		hdrsofar = strlen(header_sofar);
		combined_header = g_malloc(hdrsofar + newhdr + 2);
		strncpy(combined_header,header_sofar,hdrsofar);
		*(combined_header+hdrsofar) = ' ';
		strncpy(combined_header+hdrsofar+1,decoded_value,newhdr+1);
		g_free (decoded_value);
		g_hash_table_insert (message->headers, header, combined_header);
	    }
	} else {
	    if (header_sofar == NULL) {
		/* Only insert if we don't have a value for this header, yet. */
		g_hash_table_insert (message->headers, header, decoded_value);
	    } else {
		free (header);
		g_free (decoded_value);
		decoded_value = header_sofar;
	    }
	}
	/* if we found a match we can bail - unless of course we are
	 * collecting all the Received: headers */
	if (match && !is_received)
	    return decoded_value;
    }

    if (message->parsing_finished) {
        fclose (message->file);
        message->file = NULL;
    }

    if (message->line)
	free (message->line);
    message->line = NULL;

    if (message->value.size) {
	free (message->value.str);
	message->value.str = NULL;
	message->value.size = 0;
	message->value.len = 0;
    }

    /* For the Received: header we actually might end up here even
     * though we found the header (as we force continued parsing
     * in that case). So let's check if that's the header we were
     * looking for and return the value that we found (if any)
     */
    if (is_received)
	return (char *)g_hash_table_lookup (message->headers, "received");

    /* We've parsed all headers and never found the one we're looking
     * for. It's probably just not there, but let's check that we
     * didn't make a mistake preventing us from seeing it. */
    if (message->restrict_headers && header_desired &&
	! g_hash_table_lookup_extended (message->headers,
					header_desired, NULL, NULL))
    {
	INTERNAL_ERROR ("Attempt to get header \"%s\" which was not\n"
			"included in call to notmuch_message_file_restrict_headers\n",
			header_desired);
    }

    return "";
}
