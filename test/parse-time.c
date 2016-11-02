/*
 * parse time string - user friendly date and time parser
 * Copyright © 2012 Jani Nikula
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * Author: Jani Nikula <jani@nikula.org>
 */

#include <assert.h>
#include <ctype.h>
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "parse-time-string.h"

#define ARRAY_SIZE(a) (sizeof (a) / sizeof (a[0]))

static const char *parse_time_error_strings[] = {
    [PARSE_TIME_OK]			= "OK",
    [PARSE_TIME_ERR]			= "ERR",
    [PARSE_TIME_ERR_LIB]		= "LIB",
    [PARSE_TIME_ERR_ALREADYSET]		= "ALREADYSET",
    [PARSE_TIME_ERR_FORMAT]		= "FORMAT",
    [PARSE_TIME_ERR_DATEFORMAT]		= "DATEFORMAT",
    [PARSE_TIME_ERR_TIMEFORMAT]		= "TIMEFORMAT",
    [PARSE_TIME_ERR_INVALIDDATE]	= "INVALIDDATE",
    [PARSE_TIME_ERR_INVALIDTIME]	= "INVALIDTIME",
    [PARSE_TIME_ERR_KEYWORD]		= "KEYWORD",
};

static const char *
parse_time_strerror (unsigned int errnum)
{
    if (errnum < ARRAY_SIZE (parse_time_error_strings))
	return parse_time_error_strings[errnum];
    else
	return NULL;
}

/*
 * concat argv[start]...argv[end - 1], separating them by a single
 * space, to a malloced string
 */
static char *
concat_args (int start, int end, char *argv[])
{
    int i;
    size_t len = 1;
    char *p;

    for (i = start; i < end; i++)
	len += strlen (argv[i]) + 1;

    p = malloc (len);
    if (!p)
	return NULL;

    *p = 0;

    for (i = start; i < end; i++) {
	if (i != start)
	    strcat (p, " ");
	strcat (p, argv[i]);
    }

    return p;
}

#define DEFAULT_FORMAT "%a %b %d %T %z %Y"

static void
usage (const char *name)
{
    printf ("Usage: %s [options ...] [<date/time>]\n\n", name);
    printf (
	"Parse <date/time> and display it in given format. If <date/time> is\n"
	"not given, parse each line in stdin according to:\n\n"
	"  <date/time> [(==>|==_>|==^>|==^^>)<ignored>] [#<comment>]\n\n"
	"and produce output:\n\n"
	"  <date/time> (==>|==_>|==^>|==^^>) <time in --format=FMT> [#<comment>]\n\n"
	"preserving whitespace and comment in input. The operators ==>, ==_>,\n"
	"==^>, and ==^^> define rounding as no rounding, round down, round up\n"
	"inclusive, and round up, respectively.\n\n"

	"  -f, --format=FMT output format, FMT according to strftime(3)\n"
	"                   (default: \"%s\")\n"
	"  -r, --ref=N      use N seconds since epoch as reference time\n"
	"                   (default: now)\n"
	"  -u, --^          round result up inclusive (default: no rounding)\n"
	"  -U, --^^         round result up (default: no rounding)\n"
	"  -d, --_          round result down (default: no rounding)\n"
	"  -h, --help       print this help\n",
	DEFAULT_FORMAT);
}

struct {
    const char *operator;
    int round;
} operators[] = {
    { "==>",	PARSE_TIME_NO_ROUND },
    { "==_>",	PARSE_TIME_ROUND_DOWN },
    { "==^>",	PARSE_TIME_ROUND_UP_INCLUSIVE },
    { "==^^>",	PARSE_TIME_ROUND_UP },
};

static const char *
find_operator_in_string (char *str, char **ptr, int *round)
{
    const char *oper = NULL;
    unsigned int i;

    for (i = 0; i < ARRAY_SIZE (operators); i++) {
	char *p = strstr (str, operators[i].operator);
	if (p) {
	    if (round)
		*round = operators[i].round;
	    if (ptr)
		*ptr = p;

	    oper = operators[i].operator;
	    break;
	}
    }

    return oper;
}

static const char *
get_operator (int round)
{
    const char *oper = NULL;
    unsigned int i;

    for (i = 0; i < ARRAY_SIZE(operators); i++) {
	if (round == operators[i].round) {
	    oper = operators[i].operator;
	    break;
	}
    }

    return oper;
}

static int
parse_stdin (FILE *infile, time_t *ref, int round, const char *format)
{
    char *input = NULL;
    char result[1024];
    size_t inputsize;
    ssize_t len;
    struct tm tm;
    time_t t;
    int r;

    while ((len = getline (&input, &inputsize, infile)) != -1) {
	const char *oper;
	char *trail, *tmp;

	/* trail is trailing whitespace and (optional) comment */
	trail = strchr (input, '#');
	if (!trail)
	    trail = input + len;

	while (trail > input && isspace ((unsigned char) *(trail-1)))
	    trail--;

	if (trail == input) {
	    printf ("%s", input);
	    continue;
	}

	tmp = strdup (trail);
	if (!tmp) {
	    fprintf (stderr, "strdup() failed\n");
	    continue;
	}
	*trail = '\0';
	trail = tmp;

	/* operator */
	oper = find_operator_in_string (input, &tmp, &round);
	if (oper) {
	    *tmp = '\0';
	} else {
	    oper = get_operator (round);
	    assert (oper);
	}

	r = parse_time_string (input, &t, ref, round);
	if (!r) {
	    if (!localtime_r (&t, &tm)) {
		fprintf (stderr, "localtime_r() failed\n");
		free (trail);
		continue;
	    }

	    strftime (result, sizeof (result), format, &tm);
	} else {
	    const char *errstr = parse_time_strerror (r);
	    if (errstr)
		snprintf (result, sizeof (result), "ERROR: %s", errstr);
	    else
		snprintf (result, sizeof (result), "ERROR: %d", r);
	}

	printf ("%s%s %s%s", input, oper, result, trail);
	free (trail);
    }

    free (input);

    return 0;
}

int
main (int argc, char *argv[])
{
    int r;
    struct tm tm;
    time_t result;
    time_t now;
    time_t *nowp = NULL;
    char *argstr;
    int round = PARSE_TIME_NO_ROUND;
    char buf[1024];
    const char *format = DEFAULT_FORMAT;
    struct option options[] = {
	{ "help",	no_argument,		NULL,	'h' },
	{ "^",		no_argument,		NULL,	'u' },
	{ "^^",		no_argument,		NULL,	'U' },
	{ "_",		no_argument,		NULL,	'd' },
	{ "format",	required_argument,	NULL,	'f' },
	{ "ref",	required_argument,	NULL,	'r' },
	{ NULL, 0, NULL, 0 },
    };

    for (;;) {
	int c;

	c = getopt_long (argc, argv, "huUdf:r:", options, NULL);
	if (c == -1)
	    break;

	switch (c) {
	case 'f':
	    /* output format */
	    format = optarg;
	    break;
	case 'u':
	    round = PARSE_TIME_ROUND_UP_INCLUSIVE;
	    break;
	case 'U':
	    round = PARSE_TIME_ROUND_UP;
	    break;
	case 'd':
	    round = PARSE_TIME_ROUND_DOWN;
	    break;
	case 'r':
	    /* specify now in seconds since epoch */
	    now = (time_t) strtol (optarg, NULL, 10);
	    if (now >= (time_t) 0)
		nowp = &now;
	    break;
	case 'h':
	case '?':
	default:
	    usage (argv[0]);
	    return 1;
	}
    }

    if (optind == argc)
	return parse_stdin (stdin, nowp, round, format);

    argstr = concat_args (optind, argc, argv);
    if (!argstr)
	return 1;

    r = parse_time_string (argstr, &result, nowp, round);

    free (argstr);

    if (r) {
	const char *errstr = parse_time_strerror (r);
	if (errstr)
	    fprintf (stderr, "ERROR: %s\n", errstr);
	else
	    fprintf (stderr, "ERROR: %d\n", r);

	return r;
    }

    if (!localtime_r (&result, &tm))
	return 1;

    strftime (buf, sizeof (buf), format, &tm);
    printf ("%s\n", buf);

    return 0;
}
