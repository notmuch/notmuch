/* string-util.c -  Extra or enhanced routines for null terminated strings.
 *
 * Copyright (c) 2012 Jani Nikula
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
 * Author: Jani Nikula <jani@nikula.org>
 */


#include "string-util.h"
#include "talloc.h"

#include <errno.h>

char *
strtok_len (char *s, const char *delim, size_t *len)
{
    /* skip initial delims */
    s += strspn (s, delim);

    /* length of token */
    *len = strcspn (s, delim);

    return *len ? s : NULL;
}

static int
is_unquoted_terminator (unsigned char c)
{
    return c == 0 || c <= ' ' || c == ')';
}

int
make_boolean_term (void *ctx, const char *prefix, const char *term,
		   char **buf, size_t *len)
{
    const char *in;
    char *out;
    size_t needed = 3;
    int need_quoting = 0;

    /* Do we need quoting?  To be paranoid, we quote anything
     * containing a quote, even though it only matters at the
     * beginning, and anything containing non-ASCII text. */
    for (in = term; *in && !need_quoting; in++)
	if (is_unquoted_terminator (*in) || *in == '"'
	    || (unsigned char)*in > 127)
	    need_quoting = 1;

    if (need_quoting)
	for (in = term; *in; in++)
	    needed += (*in == '"') ? 2 : 1;
    else
	needed = strlen (term) + 1;

    /* Reserve space for the prefix */
    if (prefix)
	needed += strlen (prefix) + 1;

    if ((*buf == NULL) || (needed > *len)) {
	*len = 2 * needed;
	*buf = talloc_realloc (ctx, *buf, char, *len);
    }

    if (! *buf) {
	errno = ENOMEM;
	return -1;
    }

    out = *buf;

    /* Copy in the prefix */
    if (prefix) {
	strcpy (out, prefix);
	out += strlen (prefix);
	*out++ = ':';
    }

    if (! need_quoting) {
	strcpy (out, term);
	return 0;
    }

    /* Quote term by enclosing it in double quotes and doubling any
     * internal double quotes. */
    *out++ = '"';
    in = term;
    while (*in) {
	if (*in == '"')
	    *out++ = '"';
	*out++ = *in++;
    }
    *out++ = '"';
    *out = '\0';

    return 0;
}
