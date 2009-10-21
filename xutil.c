/* xutil.c - Various wrapper functions to abort on error.
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

#define _GNU_SOURCE /* For strndup */
#include "notmuch-private.h"

#include <stdio.h>

void *
xcalloc (size_t nmemb, size_t size)
{
    void *ret;

    ret = calloc (nmemb, size);
    if (ret == NULL) {
	fprintf (stderr, "Out of memory.\n");
	exit (1);
    }

    return ret;
}

void *
xmalloc (size_t size)
{
    void *ret;

    ret = malloc (size);
    if (ret == NULL) {
	fprintf (stderr, "Out of memory.\n");
	exit (1);
    }

    return ret;
}

void *
xrealloc (void *ptr, size_t size)
{
    void *ret;

    ret = realloc (ptr, size);
    if (ret == NULL) {
	fprintf (stderr, "Out of memory.\n");
	exit (1);
    }

    return ret;
}

char *
xstrdup (const char *s)
{
    char *ret;

    ret = strdup (s);
    if (ret == NULL) {
	fprintf (stderr, "Out of memory.\n");
	exit (1);
    }

    return ret;
}

char *
xstrndup (const char *s, size_t n)
{
    char *ret;

    ret = strndup (s, n);
    if (ret == NULL) {
	fprintf (stderr, "Out of memory.\n");
	exit (1);
    }

    return ret;
}

void
xregcomp (regex_t *preg, const char *regex, int cflags)
{
    int rerr;

    rerr = regcomp (preg, regex, cflags);
    if (rerr) {
	size_t error_size = regerror (rerr, preg, NULL, 0);
	char *error = xmalloc (error_size);

	regerror (rerr, preg, error, error_size);
	fprintf (stderr, "Internal error compiling regex %s: %s\n",
		 regex, error);
	free (error);
	exit (1);
    }
}

int
xregexec (const regex_t *preg, const char *string,
	  size_t nmatch, regmatch_t pmatch[], int eflags)
{
    int i, rerr;

    rerr = regexec (preg, string, nmatch, pmatch, eflags);
    if (rerr)
	return rerr;

    for (i = 0; i < nmatch; i++) {
	if (pmatch[i].rm_so == -1) {
	    fprintf (stderr, "Internal error matching regex against %s: Sub-match %d not found\n",
		     string, i);
	    exit (1);
	}
    }

    return 0;
}
