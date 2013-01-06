#ifndef _STRING_UTIL_H
#define _STRING_UTIL_H

#include <string.h>

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

#endif
