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

#include <glib.h> /* GHashTable */

struct _notmuch_message {
    /* File objects */
    int fd;
    void *map;

    /* Header storage */
    int restrict_headers;
    GHashTable *headers;

    /* Parsing state */
    char *start;
    size_t size;
    const char *next_line;
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

notmuch_message_t *
notmuch_message_open (const char *filename)
{
    notmuch_message_t *message;
    struct stat st;

    message = xcalloc (1, sizeof (notmuch_message_t));

    message->fd = open (filename, O_RDONLY);
    if (message->fd < 0)
	goto FAIL;

    if (fstat (message->fd, &st) < 0)
	goto FAIL;

    message->map = mmap (NULL, st.st_size, PROT_READ, MAP_PRIVATE,
			message->fd, 0);
    if (message->map == MAP_FAILED)
	goto FAIL;

    message->headers = g_hash_table_new_full (strcase_hash,
					      strcase_equal,
					      free,
					      free);

    message->start = (char *) message->map;
    message->size = st.st_size;
    message->next_line = message->start;
    message->parsing_started = 0;
    message->parsing_finished = 0;

    return message;

  FAIL:
    fprintf (stderr, "Error opening %s: %s\n", filename, strerror (errno));
    notmuch_message_close (message);

    return NULL;
}

void
notmuch_message_close (notmuch_message_t *message)
{
    if (message == NULL)
	return;

    if (message->headers)
	g_hash_table_unref (message->headers);

    if (message->map)
	munmap (message->map, message->size);
    if (message->fd)
	close (message->fd);

    free (message);
}

void
notmuch_message_restrict_headersv (notmuch_message_t *message,
				   va_list va_headers)
{
    char *header;

    if (message->parsing_started ) {
	fprintf (stderr, "Error: notmuch_message_restrict_headers called after parsing has started\n");
	exit (1);
    }

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
notmuch_message_restrict_headers (notmuch_message_t *message, ...)
{
    va_list va_headers;

    va_start (va_headers, message);

    notmuch_message_restrict_headersv (message, va_headers);
}

/* With our mmapped file, we don't get the benefit of terminated
 * strings, so we can't use things like strchr(). We don't even know
 * if there's a newline at the end of the file so we also have to be
 * careful of that. Basically, every time we advance a pointer while
 * parsing we must ensure we don't go beyond our buffer.
 */
#define WITHIN(s) (((s) - message->start) < (message->size -1))

/* In each of the macros below, "without overrunning the buffer" means
 * that the macro will never dereference a character beyond the end of
 * the buffer. However, all of the macros may return a pointer
 * pointing to the first character beyond the buffer. So callers
 * should test with WITHIN before dereferencing the result. */

/* Advance 'ptr' until pointing at a non-space character in the same
 * line, (without overrunning the buffer) */
#define SKIP_SPACE_IN_LINE(ptr)				      \
    while (WITHIN (ptr) && (*(ptr) == ' ' || *(ptr) == '\t')) \
	(ptr)++;

/* Advance 'ptr' until pointing at a non-space character, (without
 * overrunning the buffer) */
#define SKIP_SPACE(ptr)				\
    while (WITHIN (ptr) && isspace(*(ptr)))	\
	(ptr)++;

/* Advance 'ptr' to the first occurrence of 'c' within the same
 * line, (without overrunning the buffer). */
#define ADVANCE_TO(ptr, c)			\
    while (WITHIN (ptr) && *(ptr) != '\n' &&    \
	   *(ptr) != (c))			\
    {						\
	(ptr)++;				\
    }

/* Advance 'ptr' to the beginning of the next line not starting with
 * an initial tab character, (without overruning the buffer). */
#define ADVANCE_TO_NEXT_HEADER_LINE(ptr)	\
    do {					\
	ADVANCE_TO ((ptr), '\n');		\
	if (WITHIN (ptr))			\
	    (ptr)++;				\
    } while (WITHIN (ptr) &&			\
	     (*(ptr) == '\t' || *(ptr) == ' '));
        
char *
copy_header_value (const char *start, const char *end)
{
    const char *s;
    char *result, *r;
    int was_newline = 0;

    result = xmalloc (end - start + 1);

    s = start;
    r = result;

    while (s < end) {
	if (*s == '\n') {
	    was_newline = 1;
	} else {
	    if (*s == '\t' && was_newline)
		*r = ' ';
	    else
		*r = *s;
	    r++;
	    was_newline = 0;
	}
	s++;
    }

    *r = '\0';

    return result;
}

const char *
notmuch_message_get_header (notmuch_message_t *message,
			    const char *header_desired)
{
    int contains;
    const char *s, *colon;
    char *header, *value;
    int match;

    message->parsing_started = 1;

    contains = g_hash_table_lookup_extended (message->headers,
					     header_desired, NULL,
					     (gpointer *) &value);
    if (contains && value)
	return value;

    if (message->parsing_finished)
	return NULL;

    while (1) {
	s = message->next_line;

	if (*s == '\n') {
	    message->parsing_finished = 1;
	    return NULL;
	}

	if (*s == '\t') {
	    fprintf (stderr, "Warning: Unexpected continued value\n");
	    ADVANCE_TO_NEXT_HEADER_LINE (message->next_line);
	    continue;
	}

	colon = s;
	ADVANCE_TO (colon, ':');

	if (! WITHIN (colon) || *colon == '\n') {
	    fprintf (stderr, "Warning: Unexpected non-header line: %s\n", s);
	    ADVANCE_TO_NEXT_HEADER_LINE (message->next_line);
	    continue;
	}

	header = xstrndup (s, colon - s);

	if (message->restrict_headers &&
	    ! g_hash_table_lookup_extended (message->headers,
					    header, NULL, NULL))
	{
	    free (header);
	    message->next_line = colon;
	    ADVANCE_TO_NEXT_HEADER_LINE (message->next_line);
	    continue;
	}

	s = colon + 1;
	SKIP_SPACE_IN_LINE (s);

	message->next_line = s;
	ADVANCE_TO_NEXT_HEADER_LINE (message->next_line);

	value = copy_header_value (s, message->next_line);

	match = (strcasecmp (header, header_desired) == 0);

	g_hash_table_insert (message->headers, header, value);

	if (match)
	    return value;
    }
}
