/* notmuch-private.h - Internal interfaces for notmuch.
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

#ifndef NOTMUCH_PRIVATE_H
#define NOTMUCH_PRIVATE_H

#include "notmuch.h"

#ifndef _GNU_SOURCE
#define _GNU_SOURCE /* For getline */
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <ctype.h>

NOTMUCH_BEGIN_DECLS

/* xutil.c */
void *
xcalloc (size_t nmemb, size_t size);

void *
xmalloc (size_t size);

void *
xrealloc (void *ptrr, size_t size);

char *
xstrdup (const char *s);

char *
xstrndup (const char *s, size_t n);

/* message.c */

/* XXX: I haven't decided yet whether these will actually get exported
 * into the public interface in notmuch.h
 */

typedef struct _notmuch_message notmuch_message_t;

/* Open a file containing a single email message.
 *
 * The caller should call notmuch_message_close when done with this.
 *
 * Returns NULL if any error occurs.
 */
notmuch_message_t *
notmuch_message_open (const char *filename);

/* Close a notmuch message preivously opened with notmuch_message_open. */
void
notmuch_message_close (notmuch_message_t *message);

/* Restrict 'message' to only save the named headers.
 *
 * When the caller is only interested in a short list of headers,
 * known in advance, calling this function can avoid wasted time and
 * memory parsing/saving header values that will never be needed.
 *
 * The variable arguments should be a list of const char * with a
 * final '(const char *) NULL' to terminate the list.
 *
 * If this function is called, it must be called before any calls to
 * notmuch_message_get_header for this message.
 *
 * After calling this function, if notmuch_message_get_header is
 * called with a header name not in this list, then NULL will be
 * returned even if that header exists in the actual message.
 */
void
notmuch_message_restrict_headers (notmuch_message_t *message, ...);

/* Identical to notmuch_message_restrict_headers but accepting a va_list. */
void
notmuch_message_restrict_headersv (notmuch_message_t *message,
				   va_list va_headers);

/* Get the value of the specified header from the message.
 *
 * The header name is case insensitive.
 *
 * The returned value is owned by the notmuch message and is valid
 * only until the message is closed. The caller should copy it if
 * needing to modify the value or to hold onto it for longer.
 *
 * Returns NULL if the message does not contain a header line matching
 * 'header'.
 */
const char *
notmuch_message_get_header (notmuch_message_t *message,
			    const char *header);

/* date.c */

/* Parse an RFC 8222 date string to a time_t value.
 *
 * The tz_offset argument can be used to also obtain the time-zone
 * offset, (but can be NULL if the call is not interested in that).
 *
 * Returns 0 on error.
 */
time_t
notmuch_parse_date (const char *str, int *tz_offset);

NOTMUCH_END_DECLS

#endif
