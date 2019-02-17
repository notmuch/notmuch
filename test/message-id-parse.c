#include <stdio.h>
#include <talloc.h>
#include "notmuch-private.h"

int
main (unused (int argc), unused (char **argv))
{
    char *line = NULL;
    size_t len = 0;
    ssize_t nread;
    void *local = talloc_new (NULL);

    while ((nread = getline (&line, &len, stdin)) != -1) {
	int last = strlen (line) - 1;
	if (line[last] == '\n')
	    line[last] = '\0';

	char *mid = _notmuch_message_id_parse_strict (local, line);
	if (mid)
	    printf ("GOOD: %s\n", mid);
	else
	    printf ("BAD: %s\n", line);
    }

    talloc_free (local);
}
