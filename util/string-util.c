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
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Jani Nikula <jani@nikula.org>
 */


#include "string-util.h"
#include "talloc.h"

#include <ctype.h>
#include <errno.h>
#include <stdbool.h>

char *
strtok_len (char *s, const char *delim, size_t *len)
{
    /* skip initial delims */
    s += strspn (s, delim);

    /* length of token */
    *len = strcspn (s, delim);

    return *len ? s : NULL;
}

const char *
strsplit_len (const char *s, char delim, size_t *len)
{
    bool escaping = false;
    size_t count = 0;

    /* Skip initial unescaped delimiters */
    while (*s && *s == delim)
	s++;

    while (s[count] && (escaping || s[count] != delim)) {
	escaping = (s[count] == '\\');
	count++;
    }

    if (count==0)
	return NULL;

    *len = count;
    return s;
}

const char *
strtok_len_c (const char *s, const char *delim, size_t *len)
{
    /* strtok_len is already const-safe, but we can't express both
     * versions in the C type system. */
    return strtok_len ((char *) s, delim, len);
}

char *
sanitize_string (const void *ctx, const char *str)
{
    char *out, *loop;

    if (! str)
	return NULL;

    out = talloc_strdup (ctx, str);
    if (! out)
	return NULL;

    for (loop = out; *loop; loop++) {
	if (*loop == '\t' || *loop == '\n')
	    *loop = ' ';
	else if ((unsigned char) (*loop) < 32)
	    *loop = '?';
    }

    return out;
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
     * containing a quote or '(', even though these only matter at the
     * beginning, and anything containing non-ASCII text. */
    if (! term[0])
	need_quoting = 1;
    for (in = term; *in && ! need_quoting; in++)
	if (is_unquoted_terminator (*in) || *in == '"' || *in == '('
	    || (unsigned char) *in > 127)
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

const char *
skip_space (const char *str)
{
    while (*str && isspace ((unsigned char) *str))
	++str;
    return str;
}

int
parse_boolean_term (void *ctx, const char *str,
		    char **prefix_out, char **term_out)
{
    int err = EINVAL;

    *prefix_out = *term_out = NULL;

    /* Parse prefix */
    str = skip_space (str);
    const char *pos = strchr (str, ':');
    if (! pos || pos == str)
	goto FAIL;
    *prefix_out = talloc_strndup (ctx, str, pos - str);
    if (! *prefix_out) {
	err = ENOMEM;
	goto FAIL;
    }
    ++pos;

    /* Implement de-quoting compatible with make_boolean_term. */
    if (*pos == '"') {
	char *out = talloc_array (ctx, char, strlen (pos));
	int closed = 0;
	if (! out) {
	    err = ENOMEM;
	    goto FAIL;
	}
	*term_out = out;
	/* Skip the opening quote, find the closing quote, and
	 * un-double doubled internal quotes. */
	for (++pos; *pos; ) {
	    if (*pos == '"') {
		++pos;
		if (*pos != '"') {
		    /* Found the closing quote. */
		    closed = 1;
		    pos = skip_space (pos);
		    break;
		}
	    }
	    *out++ = *pos++;
	}
	/* Did the term terminate without a closing quote or is there
	 * trailing text after the closing quote? */
	if (! closed || *pos)
	    goto FAIL;
	*out = '\0';
    } else {
	const char *start = pos;
	/* Check for text after the boolean term. */
	while (! is_unquoted_terminator (*pos))
	    ++pos;
	if (*skip_space (pos)) {
	    err = EINVAL;
	    goto FAIL;
	}
	/* No trailing text; dup the string so the caller can free
	 * it. */
	*term_out = talloc_strndup (ctx, start, pos - start);
	if (! *term_out) {
	    err = ENOMEM;
	    goto FAIL;
	}
    }
    return 0;

  FAIL:
    talloc_free (*prefix_out);
    talloc_free (*term_out);
    errno = err;
    return -1;
}

int
strcmp_null (const char *s1, const char *s2)
{
    if (s1 && s2)
	return strcmp (s1, s2);
    else if (! s1 && ! s2)
	return 0;
    else if (s1)
	return 1;       /* s1 (non-NULL) is greater than s2 (NULL) */
    else
	return -1;      /* s1 (NULL) is less than s2 (non-NULL) */
}

int
strcase_equal (const void *a, const void *b)
{
    return strcasecmp (a, b) == 0;
}

unsigned int
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

void
strip_trailing (char *str, char ch)
{
    int i;

    for (i = strlen (str) - 1; i >= 0; i--) {
	if (str[i] == ch)
	    str[i] = '\0';
	else
	    break;
    }
}
