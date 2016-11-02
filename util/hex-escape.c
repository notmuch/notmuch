/* hex-escape.c -  Manage encoding and decoding of byte strings into path names
 *
 * Copyright (c) 2011 David Bremner
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
 * Author: David Bremner <david@tethera.net>
 */

#include <assert.h>
#include <string.h>
#include <talloc.h>
#include <ctype.h>
#include "error_util.h"
#include "hex-escape.h"

static const char *output_charset =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-_@=.,";

static const char escape_char = '%';

static int
is_output (char c)
{
    return (strchr (output_charset, c) != NULL);
}

static int
maybe_realloc (void *ctx, size_t needed, char **out, size_t *out_size)
{
    if (*out_size < needed) {

	if (*out == NULL)
	    *out = talloc_size (ctx, needed);
	else
	    *out = talloc_realloc (ctx, *out, char, needed);

	if (*out == NULL)
	    return 0;

	*out_size = needed;
    }
    return 1;
}

hex_status_t
hex_encode (void *ctx, const char *in, char **out, size_t *out_size)
{

    const char *p;
    char *q;

    size_t needed = 1;  /* for the NUL */

    assert (ctx); assert (in); assert (out); assert (out_size);

    for (p = in; *p; p++) {
	needed += is_output (*p) ? 1 : 3;
    }

    if (*out == NULL)
	*out_size = 0;

    if (!maybe_realloc (ctx, needed, out, out_size))
	return HEX_OUT_OF_MEMORY;

    q = *out;
    p = in;

    while (*p) {
	if (is_output (*p)) {
	    *q++ = *p++;
	} else {
	    sprintf (q, "%%%02x", (unsigned char)*p++);
	    q += 3;
	}
    }

    *q = '\0';
    return HEX_SUCCESS;
}

/* Hex decode 'in' to 'out'.
 *
 * This must succeed for in == out to support hex_decode_inplace().
 */
static hex_status_t
hex_decode_internal (const char *in, unsigned char *out)
{
    char buf[3];

    while (*in) {
	if (*in == escape_char) {
	    char *endp;

	    /* This also handles unexpected end-of-string. */
	    if (!isxdigit ((unsigned char) in[1]) ||
		!isxdigit ((unsigned char) in[2]))
		return HEX_SYNTAX_ERROR;

	    buf[0] = in[1];
	    buf[1] = in[2];
	    buf[2] = '\0';

	    *out = strtoul (buf, &endp, 16);

	    if (endp != buf + 2)
		return HEX_SYNTAX_ERROR;

	    in += 3;
	    out++;
	} else {
	    *out++ = *in++;
	}
    }

    *out = '\0';

    return HEX_SUCCESS;
}

hex_status_t
hex_decode_inplace (char *s)
{
    /* A decoded string is never longer than the encoded one, so it is
     * safe to decode a string onto itself. */
    return hex_decode_internal (s, (unsigned char *) s);
}

hex_status_t
hex_decode (void *ctx, const char *in, char **out, size_t * out_size)
{
    const char *p;
    size_t needed = 1;	/* for the NUL */

    assert (ctx); assert (in); assert (out); assert (out_size);

    for (p = in; *p; p++)
	if ((p[0] == escape_char) && isxdigit (p[1]) && isxdigit (p[2]))
	    needed -= 1;
	else
	    needed += 1;

    if (!maybe_realloc (ctx, needed, out, out_size))
	return HEX_OUT_OF_MEMORY;

    return hex_decode_internal (in, (unsigned char *) *out);
}
