#ifndef NOTMUCH_SPRINTER_H
#define NOTMUCH_SPRINTER_H

/* Necessary for notmuch_bool_t */
#include "notmuch-client.h"

/* Structure printer interface. This is used to create output
 * structured as maps (with key/value pairs), lists and primitives
 * (strings, integers and booleans).
 */
typedef struct sprinter {
    /* Start a new map/dictionary structure. This should be followed by
     * a sequence of alternating calls to map_key and one of the
     * value-printing functions until the map is ended by end.
     */
    void (*begin_map) (struct sprinter *);

    /* Start a new list/array structure.
     */
    void (*begin_list) (struct sprinter *);

    /* End the last opened list or map structure.
     */
    void (*end) (struct sprinter *);

    /* Print one string/integer/boolean/null element (possibly inside
     * a list or map, followed or preceded by separators).  For string
     * and string_len, the char * must be UTF-8 encoded.  string_len
     * allows non-terminated strings and strings with embedded NULs
     * (though the handling of the latter is format-dependent). For
     * string (but not string_len) the string pointer passed may be
     * NULL.
     */
    void (*string) (struct sprinter *, const char *);
    void (*string_len) (struct sprinter *, const char *, size_t);
    void (*integer) (struct sprinter *, int);
    void (*boolean) (struct sprinter *, notmuch_bool_t);
    void (*null) (struct sprinter *);

    /* Print the key of a map's key/value pair. The char * must be UTF-8
     * encoded.
     */
    void (*map_key) (struct sprinter *, const char *);

    /* Insert a separator (usually extra whitespace). For the text
     * printers, this is a syntactic separator. For the structured
     * printers, this is for improved readability without affecting
     * the abstract syntax of the structure being printed. For JSON,
     * this could simply be a line break.
     */
    void (*separator) (struct sprinter *);

    /* Set the current string prefix. This only affects the text
     * printer, which will print this string, followed by a colon,
     * before any string. For other printers, this does nothing.
     */
    void (*set_prefix) (struct sprinter *, const char *);

    /* True if this is the special-cased plain text printer.
     */
    notmuch_bool_t is_text_printer;
} sprinter_t;


/* Create a new unstructured printer that emits the default text format
 * for "notmuch search". */
struct sprinter *
sprinter_text_create (const void *ctx, FILE *stream);

/* Create a new unstructured printer that emits the text format for
 * "notmuch search", with each field separated by a null character
 * instead of the newline character. */
struct sprinter *
sprinter_text0_create (const void *ctx, FILE *stream);

/* Create a new structure printer that emits JSON. */
struct sprinter *
sprinter_json_create (const void *ctx, FILE *stream);

/* Create a new structure printer that emits S-Expressions. */
struct sprinter *
sprinter_sexp_create (const void *ctx, FILE *stream);

#endif // NOTMUCH_SPRINTER_H
