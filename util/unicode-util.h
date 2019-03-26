#ifndef UNICODE_UTIL_H
#define UNICODE_UTIL_H

#include <stdbool.h>
#include <gmodule.h>

/* The utf8 encoded string would tokenize as a single word, according
 * to xapian. */
bool unicode_word_utf8 (const char *str);
typedef gunichar notmuch_unichar;

#endif
