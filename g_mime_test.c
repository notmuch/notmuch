/*
 * Copyright © 2009 Carl Worth
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include <gmime/gmime.h>

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))

static void
print_header (const char *name, const char *value)
{
    printf ("%s:", name);
    if (value)
	printf ("\t%s", value);
    printf ("\n");
}

int
main (int argc, char **argv)
{
    GMimeStream *stream;
    GMimeParser *parser;
    GMimeMessage *message;

    const char *filename;
    FILE *file;

    const char *sup_entry_headers[] = {
	"From",
	"Subject",
	"Date",
	"References",
	"CC",
	"To",
	"In-Reply-To"
    };
    const char *value;
    int i;

    if (argc < 2) {
	fprintf (stderr, "Usage: %s <mail-message>\n",
		 argv[0]);
	exit (1);
    }

    filename = argv[1];

    file = fopen (filename, "r");
    if (! file) {
	fprintf (stderr, "Error opening %s: %s\n", filename, strerror (errno));
	exit (1);
    }
    
    g_mime_init (0);
    
    stream = g_mime_stream_file_new (file);

    parser = g_mime_parser_new_with_stream (stream);

    message = g_mime_parser_construct_message (parser);

    value = g_mime_message_get_message_id (message);
    print_header ("message_id", value);

    for (i = 0; i < ARRAY_SIZE (sup_entry_headers); i++) {
	value = g_mime_object_get_header (GMIME_OBJECT (message),
					  sup_entry_headers[i]);
	print_header (sup_entry_headers[i], value);
    }

    g_object_unref (message);
    g_object_unref (parser);
    g_object_unref (stream);
    
    return 0;
}
