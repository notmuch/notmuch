#include "unicode-util.h"

/* Based on Xapian::Unicode::is_wordchar, to avoid forcing clients to
 * link directly to libxapian.
 */

static bool
unicode_is_wordchar (notmuch_unichar ch)
{
    switch (g_unichar_type (ch)) {
    case G_UNICODE_UPPERCASE_LETTER:
    case G_UNICODE_LOWERCASE_LETTER:
    case G_UNICODE_TITLECASE_LETTER:
    case G_UNICODE_MODIFIER_LETTER:
    case G_UNICODE_OTHER_LETTER:
    case G_UNICODE_NON_SPACING_MARK:
    case G_UNICODE_ENCLOSING_MARK:
    case G_UNICODE_SPACING_MARK:
    case G_UNICODE_DECIMAL_NUMBER:
    case G_UNICODE_LETTER_NUMBER:
    case G_UNICODE_OTHER_NUMBER:
    case G_UNICODE_CONNECT_PUNCTUATION:
	return true;
    default:
	return false;
    }
}

bool
unicode_word_utf8 (const char *utf8_str)
{
    gunichar *decoded = g_utf8_to_ucs4_fast (utf8_str, -1, NULL);
    const gunichar *p = decoded;
    bool ret;

    while (*p && unicode_is_wordchar (*p))
	p++;

    ret =  (*p == '\0');

    g_free (decoded);
    return ret;
}
