/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2012 Peter Feigl
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
 * Author: Peter Feigl <peter.feigl@gmx.at>
 */

#include <stdbool.h>
#include <stdio.h>
#include <talloc.h>
#include "sprinter.h"
#include <ctype.h>

struct sprinter_sexp {
    struct sprinter vtable;
    FILE *stream;
    /* Top of the state stack, or NULL if the printer is not currently
     * inside any aggregate types. */
    struct sexp_state *state;

    /* A flag to signify that a separator should be inserted in the
     * output as soon as possible. */
    notmuch_bool_t insert_separator;
};

struct sexp_state {
    struct sexp_state *parent;

    /* True if nothing has been printed in this aggregate yet.
     * Suppresses the space before a value. */
    notmuch_bool_t first;
};

/* Helper function to set up the stream to print a value.  If this
 * value follows another value, prints a space. */
static struct sprinter_sexp *
sexp_begin_value (struct sprinter *sp)
{
    struct sprinter_sexp *sps = (struct sprinter_sexp *) sp;

    if (sps->state) {
	if (! sps->state->first) {
	    if (sps->insert_separator) {
		fputc ('\n', sps->stream);
		sps->insert_separator = FALSE;
	    } else {
		fputc (' ', sps->stream);
	    }
	} else {
	    sps->state->first = FALSE;
	}
    }
    return sps;
}

/* Helper function to begin an aggregate type.  Prints the open
 * character and pushes a new state frame. */
static void
sexp_begin_aggregate (struct sprinter *sp)
{
    struct sprinter_sexp *sps = sexp_begin_value (sp);
    struct sexp_state *state = talloc (sps, struct sexp_state);

    fputc ('(', sps->stream);
    state->parent = sps->state;
    state->first = TRUE;
    sps->state = state;
}

static void
sexp_begin_map (struct sprinter *sp)
{
    sexp_begin_aggregate (sp);
}

static void
sexp_begin_list (struct sprinter *sp)
{
    sexp_begin_aggregate (sp);
}

static void
sexp_end (struct sprinter *sp)
{
    struct sprinter_sexp *sps = (struct sprinter_sexp *) sp;
    struct sexp_state *state = sps->state;

    fputc (')', sps->stream);
    sps->state = state->parent;
    talloc_free (state);
    if (sps->state == NULL)
	fputc ('\n', sps->stream);
}

static void
sexp_string_len (struct sprinter *sp, const char *val, size_t len)
{
    /* Some characters need escaping. " and \ work fine in all Lisps,
     * \n is not supported in CL, but all others work fine.
     * Characters below 32 are printed as \123o (three-digit
     * octals), which work fine in most Schemes and Emacs. */
    static const char *const escapes[] = {
	['\"'] = "\\\"", ['\\'] = "\\\\",  ['\n'] = "\\n"
    };
    struct sprinter_sexp *sps = sexp_begin_value (sp);

    fputc ('"', sps->stream);
    for (; len; ++val, --len) {
	unsigned char ch = *val;
	if (ch < ARRAY_SIZE (escapes) && escapes[ch])
	    fputs (escapes[ch], sps->stream);
	else if (ch >= 32)
	    fputc (ch, sps->stream);
	else
	    fprintf (sps->stream, "\\%03o", ch);
    }
    fputc ('"', sps->stream);
}

static void
sexp_string (struct sprinter *sp, const char *val)
{
    if (val == NULL)
	val = "";
    sexp_string_len (sp, val, strlen (val));
}

/* Prints a symbol, i.e. the name preceded by a colon. This should work
 * in all Lisps, at least as a symbol, if not as a proper keyword */
static void
sexp_keyword (struct sprinter *sp, const char *val)
{
    unsigned int i = 0;
    struct sprinter_sexp *sps = (struct sprinter_sexp *) sp;
    char ch;

    if (val == NULL)
	INTERNAL_ERROR ("illegal symbol NULL");

    for (i = 0; i < strlen (val); i++) {
	ch = val[i];
	if (! (isalnum (ch) || (ch == '-') || (ch == '_'))) {
	    INTERNAL_ERROR ("illegal character in symbol %s: %c", val, ch);
	}
    }
    fputc (':', sps->stream);
    fputs (val, sps->stream);
}

static void
sexp_integer (struct sprinter *sp, int val)
{
    struct sprinter_sexp *sps = sexp_begin_value (sp);

    fprintf (sps->stream, "%d", val);
}

static void
sexp_boolean (struct sprinter *sp, notmuch_bool_t val)
{
    struct sprinter_sexp *sps = sexp_begin_value (sp);

    fputs (val ? "t" : "nil", sps->stream);
}

static void
sexp_null (struct sprinter *sp)
{
    struct sprinter_sexp *sps = sexp_begin_value (sp);

    fputs ("nil", sps->stream);
}

static void
sexp_map_key (struct sprinter *sp, const char *key)
{
    sexp_begin_value (sp);

    sexp_keyword (sp, key);
}

static void
sexp_set_prefix (unused (struct sprinter *sp), unused (const char *name))
{
}

static void
sexp_separator (struct sprinter *sp)
{
    struct sprinter_sexp *sps = (struct sprinter_sexp *) sp;

    sps->insert_separator = TRUE;
}

struct sprinter *
sprinter_sexp_create (const void *ctx, FILE *stream)
{
    static const struct sprinter_sexp template = {
	.vtable = {
	    .begin_map = sexp_begin_map,
	    .begin_list = sexp_begin_list,
	    .end = sexp_end,
	    .string = sexp_string,
	    .string_len = sexp_string_len,
	    .integer = sexp_integer,
	    .boolean = sexp_boolean,
	    .null = sexp_null,
	    .map_key = sexp_map_key,
	    .separator = sexp_separator,
	    .set_prefix = sexp_set_prefix,
	    .is_text_printer = FALSE,
	}
    };
    struct sprinter_sexp *res;

    res = talloc (ctx, struct sprinter_sexp);
    if (! res)
	return NULL;

    *res = template;
    res->stream = stream;
    return &res->vtable;
}
