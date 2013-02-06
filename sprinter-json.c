#include <stdbool.h>
#include <stdio.h>
#include <talloc.h>
#include "sprinter.h"

struct sprinter_json {
    struct sprinter vtable;
    FILE *stream;
    /* Top of the state stack, or NULL if the printer is not currently
     * inside any aggregate types. */
    struct json_state *state;

    /* A flag to signify that a separator should be inserted in the
     * output as soon as possible.
     */
    notmuch_bool_t insert_separator;
};

struct json_state {
    struct json_state *parent;
    /* True if nothing has been printed in this aggregate yet.
     * Suppresses the comma before a value. */
    notmuch_bool_t first;
    /* The character that closes the current aggregate. */
    char close;
};

/* Helper function to set up the stream to print a value.  If this
 * value follows another value, prints a comma. */
static struct sprinter_json *
json_begin_value (struct sprinter *sp)
{
    struct sprinter_json *spj = (struct sprinter_json *) sp;

    if (spj->state) {
	if (! spj->state->first) {
	    fputc (',', spj->stream);
	    if (spj->insert_separator) {
		fputc ('\n', spj->stream);
		spj->insert_separator = FALSE;
	    } else {
		fputc (' ', spj->stream);
	    }
	} else {
	    spj->state->first = FALSE;
	}
    }
    return spj;
}

/* Helper function to begin an aggregate type.  Prints the open
 * character and pushes a new state frame. */
static void
json_begin_aggregate (struct sprinter *sp, char open, char close)
{
    struct sprinter_json *spj = json_begin_value (sp);
    struct json_state *state = talloc (spj, struct json_state);

    fputc (open, spj->stream);
    state->parent = spj->state;
    state->first = TRUE;
    state->close = close;
    spj->state = state;
}

static void
json_begin_map (struct sprinter *sp)
{
    json_begin_aggregate (sp, '{', '}');
}

static void
json_begin_list (struct sprinter *sp)
{
    json_begin_aggregate (sp, '[', ']');
}

static void
json_end (struct sprinter *sp)
{
    struct sprinter_json *spj = (struct sprinter_json *) sp;
    struct json_state *state = spj->state;

    fputc (spj->state->close, spj->stream);
    spj->state = state->parent;
    talloc_free (state);
    if (spj->state == NULL)
	fputc ('\n', spj->stream);
}

/* This implementation supports embedded NULs as allowed by the JSON
 * specification and Unicode.  Support for *parsing* embedded NULs
 * varies, but is generally not a problem outside of C-based parsers
 * (Python's json module and Emacs' json.el take embedded NULs in
 * stride). */
static void
json_string_len (struct sprinter *sp, const char *val, size_t len)
{
    static const char *const escapes[] = {
	['\"'] = "\\\"", ['\\'] = "\\\\", ['\b'] = "\\b",
	['\f'] = "\\f",  ['\n'] = "\\n",  ['\t'] = "\\t"
    };
    struct sprinter_json *spj = json_begin_value (sp);

    fputc ('"', spj->stream);
    for (; len; ++val, --len) {
	unsigned char ch = *val;
	if (ch < ARRAY_SIZE (escapes) && escapes[ch])
	    fputs (escapes[ch], spj->stream);
	else if (ch >= 32)
	    fputc (ch, spj->stream);
	else
	    fprintf (spj->stream, "\\u%04x", ch);
    }
    fputc ('"', spj->stream);
}

static void
json_string (struct sprinter *sp, const char *val)
{
    if (val == NULL)
	val = "";
    json_string_len (sp, val, strlen (val));
}

static void
json_integer (struct sprinter *sp, int val)
{
    struct sprinter_json *spj = json_begin_value (sp);

    fprintf (spj->stream, "%d", val);
}

static void
json_boolean (struct sprinter *sp, notmuch_bool_t val)
{
    struct sprinter_json *spj = json_begin_value (sp);

    fputs (val ? "true" : "false", spj->stream);
}

static void
json_null (struct sprinter *sp)
{
    struct sprinter_json *spj = json_begin_value (sp);

    fputs ("null", spj->stream);
}

static void
json_map_key (struct sprinter *sp, const char *key)
{
    struct sprinter_json *spj = (struct sprinter_json *) sp;

    json_string (sp, key);
    fputs (": ", spj->stream);
    spj->state->first = TRUE;
}

static void
json_set_prefix (unused (struct sprinter *sp), unused (const char *name))
{
}

static void
json_separator (struct sprinter *sp)
{
    struct sprinter_json *spj = (struct sprinter_json *) sp;

    spj->insert_separator = TRUE;
}

struct sprinter *
sprinter_json_create (const void *ctx, FILE *stream)
{
    static const struct sprinter_json template = {
	.vtable = {
	    .begin_map = json_begin_map,
	    .begin_list = json_begin_list,
	    .end = json_end,
	    .string = json_string,
	    .string_len = json_string_len,
	    .integer = json_integer,
	    .boolean = json_boolean,
	    .null = json_null,
	    .map_key = json_map_key,
	    .separator = json_separator,
	    .set_prefix = json_set_prefix,
	    .is_text_printer = FALSE,
	}
    };
    struct sprinter_json *res;

    res = talloc (ctx, struct sprinter_json);
    if (! res)
	return NULL;

    *res = template;
    res->stream = stream;
    return &res->vtable;
}
