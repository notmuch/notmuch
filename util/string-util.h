#ifndef _STRING_UTIL_H
#define _STRING_UTIL_H

#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

/* like strtok(3), but without state, and doesn't modify s.  Return
 * value is indicated by pointer and length, not null terminator.
 *
 * Usage pattern:
 *
 * const char *tok = input;
 * const char *delim = " \t";
 * size_t tok_len = 0;
 *
 * while ((tok = strtok_len (tok + tok_len, delim, &tok_len)) != NULL) {
 *     // do stuff with string tok of length tok_len
 * }
 */

char *strtok_len (char *s, const char *delim, size_t *len);

/* Const version of strtok_len. */
const char *strtok_len_c (const char *s, const char *delim, size_t *len);

/* Simplified version of strtok_len, with a single delimiter.
 * Handles escaping delimiters with \
 * Usage pattern:
 *
 * const char *tok = input;
 * const char *delim = ';';
 * size_t tok_len = 0;
 *
 * while ((tok = strsplit_len (tok + tok_len, delim, &tok_len)) != NULL) {
 *     // do stuff with string tok of length tok_len
 * }
 */
const char *strsplit_len (const char *s, char delim, size_t *len);

/* Return a talloced string with str sanitized.
 *
 * Whitespace characters (tabs and newlines) are replaced with spaces,
 * non-printable characters with question marks.
 */
char *sanitize_string (const void *ctx, const char *str);

/* Construct a boolean term query with the specified prefix (e.g.,
 * "id") and search term, quoting term as necessary.  Specifically, if
 * term contains any non-printable ASCII characters, non-ASCII
 * characters, close parenthesis or double quotes, it will be enclosed
 * in double quotes and any internal double quotes will be doubled
 * (e.g. a"b -> "a""b").  The result will be a valid notmuch query and
 * can be parsed by parse_boolean_term.
 *
 * Output is into buf; it may be talloc_realloced.
 * Return: 0 on success, -1 on error.  errno will be set to ENOMEM if
 * there is an allocation failure.
 */
int make_boolean_term (void *talloc_ctx, const char *prefix, const char *term,
		       char **buf, size_t *len);

/* Parse a boolean term query consisting of a prefix, a colon, and a
 * term that may be quoted as described for make_boolean_term.  If the
 * term is not quoted, then it ends at the first whitespace or close
 * parenthesis.  str may containing leading or trailing whitespace,
 * but anything else is considered a parse error.  This is compatible
 * with anything produced by make_boolean_term, and supports a subset
 * of the quoting styles supported by Xapian (and hence notmuch).
 * *prefix_out and *term_out will be talloc'd with context ctx.
 *
 * Return: 0 on success, -1 on error.  errno will be set to EINVAL if
 * there is a parse error or ENOMEM if there is an allocation failure.
 */
int
parse_boolean_term (void *ctx, const char *str,
		    char **prefix_out, char **term_out);

/* strcmp that handles NULL strings; in strcmp terms a NULL string is
 * considered to be less than a non-NULL string.
 */
int strcmp_null (const char *s1, const char *s2);

/* GLib GEqualFunc compatible strcasecmp wrapper */
int strcase_equal (const void *a, const void *b);

/* GLib GHashFunc compatible case insensitive hash function */
unsigned int strcase_hash (const void *ptr);

void strip_trailing (char *str, char ch);

const char *skip_space (const char *str);

#ifdef __cplusplus
}
#endif

#endif
