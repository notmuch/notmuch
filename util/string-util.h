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

#endif
