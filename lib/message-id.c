#include "notmuch-private.h"
#include "string-util.h"

/* Advance 'str' past any whitespace or RFC 822 comments. A comment is
 * a (potentially nested) parenthesized sequence with '\' used to
 * escape any character (including parentheses).
 *
 * If the sequence to be skipped continues to the end of the string,
 * then 'str' will be left pointing at the final terminating '\0'
 * character.
 */
static void
skip_space_and_comments (const char **str)
{
    const char *s;

    s = *str;
    while (*s && (isspace (*s) || *s == '(')) {
	while (*s && isspace (*s))
	    s++;
	if (*s == '(') {
	    int nesting = 1;
	    s++;
	    while (*s && nesting) {
		if (*s == '(') {
		    nesting++;
		} else if (*s == ')') {
		    nesting--;
		} else if (*s == '\\') {
		    if (*(s + 1))
			s++;
		}
		s++;
	    }
	}
    }

    *str = s;
}

char *
_notmuch_message_id_parse (void *ctx, const char *message_id, const char **next)
{
    const char *s, *end;
    char *result;

    if (message_id == NULL || *message_id == '\0')
	return NULL;

    s = message_id;

    skip_space_and_comments (&s);

    /* Skip any unstructured text as well. */
    while (*s && *s != '<')
	s++;

    if (*s == '<') {
	s++;
    } else {
	if (next)
	    *next = s;
	return NULL;
    }

    skip_space_and_comments (&s);

    end = s;
    while (*end && *end != '>')
	end++;
    if (next) {
	if (*end)
	    *next = end + 1;
	else
	    *next = end;
    }

    if (end > s && *end == '>')
	end--;
    if (end <= s)
	return NULL;

    result = talloc_strndup (ctx, s, end - s + 1);

    /* Finally, collapse any whitespace that is within the message-id
     * itself. */
    {
	char *r;
	int len;

	for (r = result, len = strlen (r); *r; r++, len--)
	    if (*r == ' ' || *r == '\t')
		memmove (r, r + 1, len);
    }

    return result;
}

char *
_notmuch_message_id_parse_strict (void *ctx, const char *message_id)
{
    const char *s, *end;

    if (message_id == NULL || *message_id == '\0')
	return NULL;

    s = skip_space (message_id);
    if (*s == '<')
	s++;
    else
	return NULL;

    for (end = s; *end && *end != '>'; end++)
	if (isspace (*end))
	    return NULL;

    if (*end != '>')
	return NULL;
    else {
	const char *last = skip_space (end + 1);
	if (*last != '\0')
	    return NULL;
    }

    return talloc_strndup (ctx, s, end - s);
}
