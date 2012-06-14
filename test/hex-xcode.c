/* No, nothing to to with IDE from Apple Inc.
 * testbed for ../util/hex-escape.c.
 *
 * usage:
 * hex-xcode [--direction=(encode|decode)] [--omit-newline] < file
 * hex-xcode [--direction=(encode|decode)] [--omit-newline] [--in-place] arg1 arg2 arg3 ...
 *
 */

#include "notmuch-client.h"
#include "hex-escape.h"
#include <assert.h>

enum direction {
    ENCODE,
    DECODE
};

static int inplace = FALSE;

static int
xcode (void *ctx, enum direction dir, char *in, char **buf_p, size_t *size_p)
{
    hex_status_t status;

    if (dir == ENCODE)
	status = hex_encode (ctx, in, buf_p, size_p);
    else
	if (inplace) {
	    status = hex_decode_inplace (in);
	    *buf_p = in;
	    *size_p = strlen(in);
	} else {
	    status = hex_decode (ctx, in, buf_p, size_p);
	}

    if (status == HEX_SUCCESS)
	fputs (*buf_p, stdout);

    return status;
}

int
main (int argc, char **argv)
{

    enum direction dir = DECODE;
    int omit_newline = FALSE;

    notmuch_opt_desc_t options[] = {
	{ NOTMUCH_OPT_KEYWORD, &dir, "direction", 'd',
	  (notmuch_keyword_t []){ { "encode", ENCODE },
				  { "decode", DECODE },
				  { 0, 0 } } },
	{ NOTMUCH_OPT_BOOLEAN, &omit_newline, "omit-newline", 'n', 0 },
	{ NOTMUCH_OPT_BOOLEAN, &inplace, "in-place", 'i', 0 },
	{ 0, 0, 0, 0, 0 }
    };

    int opt_index = parse_arguments (argc, argv, options, 1);

    if (opt_index < 0)
	exit (1);

    void *ctx = talloc_new (NULL);

    char *line = NULL;
    size_t line_size;
    ssize_t line_len;

    char *buffer = NULL;
    size_t buf_size = 0;

    notmuch_bool_t read_stdin = TRUE;

    for (; opt_index < argc; opt_index++) {

	if (xcode (ctx, dir, argv[opt_index],
		   &buffer, &buf_size) != HEX_SUCCESS)
	    return 1;

	if (! omit_newline)
	    putchar ('\n');

	read_stdin = FALSE;
    }

    if (! read_stdin)
	return 0;

    while ((line_len = getline (&line, &line_size, stdin)) != -1) {

	chomp_newline (line);

	if (xcode (ctx, dir, line, &buffer, &buf_size) != HEX_SUCCESS)
	    return 1;

	if (! omit_newline)
	    putchar ('\n');

    }

    if (line)
	free (line);

    talloc_free (ctx);

    return 0;
}
