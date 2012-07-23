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

    /* Print one string/integer/boolean/null element (possibly inside a
     * list or map, followed or preceded by separators).
     * For string, the char * must be UTF-8 encoded.
     */
    void (*string) (struct sprinter *, const char *);
    void (*integer) (struct sprinter *, int);
    void (*boolean) (struct sprinter *, notmuch_bool_t);
    void (*null) (struct sprinter *);

    /* Print the key of a map's key/value pair. The char * must be UTF-8
     * encoded.
     */
    void (*map_key) (struct sprinter *, const char *);

    /* Insert a separator (usually extra whitespace) for improved
     * readability without affecting the abstract syntax of the
     * structure being printed.
     * For JSON, this could simply be a line break.
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

#endif // NOTMUCH_SPRINTER_H
