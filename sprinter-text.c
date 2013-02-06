#include <stdbool.h>
#include <stdio.h>
#include <talloc.h>
#include "sprinter.h"

/* "Structured printer" interface for unstructured text printing.
 * Note that --output=summary is dispatched and formatted in
 * notmuch-search.c, the code in this file is only used for all other
 * output types.
 */

struct sprinter_text {
    struct sprinter vtable;
    FILE *stream;

    /* The current prefix to be printed with string/integer/boolean
     * data.
     */
    const char *current_prefix;

    /* A flag to indicate if this is the first tag. Used in list of tags
     * for summary.
     */
    notmuch_bool_t first_tag;
};

static void
text_string_len (struct sprinter *sp, const char *val, size_t len)
{
    struct sprinter_text *sptxt = (struct sprinter_text *) sp;

    if (sptxt->current_prefix != NULL)
	fprintf (sptxt->stream, "%s:", sptxt->current_prefix);

    fwrite (val, len, 1, sptxt->stream);
}

static void
text_string (struct sprinter *sp, const char *val)
{
    if (val == NULL)
	val = "";
    text_string_len (sp, val, strlen (val));
}

static void
text_integer (struct sprinter *sp, int val)
{
    struct sprinter_text *sptxt = (struct sprinter_text *) sp;

    fprintf (sptxt->stream, "%d", val);
}

static void
text_boolean (struct sprinter *sp, notmuch_bool_t val)
{
    struct sprinter_text *sptxt = (struct sprinter_text *) sp;

    fputs (val ? "true" : "false", sptxt->stream);
}

static void
text_separator (struct sprinter *sp)
{
    struct sprinter_text *sptxt = (struct sprinter_text *) sp;

    fputc ('\n', sptxt->stream);
}

static void
text0_separator (struct sprinter *sp)
{
    struct sprinter_text *sptxt = (struct sprinter_text *) sp;

    fputc ('\0', sptxt->stream);
}

static void
text_set_prefix (struct sprinter *sp, const char *prefix)
{
    struct sprinter_text *sptxt = (struct sprinter_text *) sp;

    sptxt->current_prefix = prefix;
}

/* The structure functions begin_map, begin_list, end and map_key
 * don't do anything in the text formatter.
 */

static void
text_begin_map (unused (struct sprinter *sp))
{
}

static void
text_begin_list (unused (struct sprinter *sp))
{
}

static void
text_end (unused (struct sprinter *sp))
{
}

static void
text_null (unused (struct sprinter *sp))
{
}

static void
text_map_key (unused (struct sprinter *sp), unused (const char *key))
{
}

struct sprinter *
sprinter_text_create (const void *ctx, FILE *stream)
{
    static const struct sprinter_text template = {
	.vtable = {
	    .begin_map = text_begin_map,
	    .begin_list = text_begin_list,
	    .end = text_end,
	    .string = text_string,
	    .string_len = text_string_len,
	    .integer = text_integer,
	    .boolean = text_boolean,
	    .null = text_null,
	    .map_key = text_map_key,
	    .separator = text_separator,
	    .set_prefix = text_set_prefix,
	    .is_text_printer = TRUE,
	},
    };
    struct sprinter_text *res;

    res = talloc (ctx, struct sprinter_text);
    if (! res)
	return NULL;

    *res = template;
    res->stream = stream;
    return &res->vtable;
}

struct sprinter *
sprinter_text0_create (const void *ctx, FILE *stream)
{
    struct sprinter *sp;

    sp = sprinter_text_create (ctx, stream);
    if (! sp)
	return NULL;

    sp->separator = text0_separator;

    return sp;
}
