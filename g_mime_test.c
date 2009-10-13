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
#include <time.h>

#include <gmime/gmime.h>

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))

/* These prefix values are specifically chosen to be compatible
 * with sup, (http://sup.rubyforge.org), written by
 * William Morgan <wmorgan-sup@masanjin.net>, and released
 * under the GNU GPL v2.
 */

typedef struct {
    const char *name;
    const char *prefix;
} prefix_t;

prefix_t NORMAL_PREFIX[] = {
    { "subject", "S" },
    { "body", "B" },
    { "from_name", "FN" },
    { "to_name", "TN" },
    { "name", "N" },
    { "attachment", "A" }
};

prefix_t BOOLEAN_PREFIX[] = {
    { "type", "K" },
    { "from_email", "FE" },
    { "to_email", "TE" },
    { "email", "E" },
    { "date", "D" },
    { "label", "L" },
    { "source_id", "I" },
    { "attachment_extension", "O" },
    { "msgid", "Q" },
    { "thread", "H" },
    { "ref", "R" }
};

static const char *
find_prefix (const char *name)
{
    int i;

    for (i = 0; i < ARRAY_SIZE (NORMAL_PREFIX); i++)
	if (strcmp (name, NORMAL_PREFIX[i].name) == 0)
	    return NORMAL_PREFIX[i].prefix;

    for (i = 0; i < ARRAY_SIZE (BOOLEAN_PREFIX); i++)
	if (strcmp (name, BOOLEAN_PREFIX[i].name) == 0)
	    return BOOLEAN_PREFIX[i].prefix;

    return "";
}

int TERM_COMBINED = 0;

static void
print_term (const char *prefix_name, const char *value)
{
    const char *prefix;

    if (value == NULL)
	return;

    prefix = find_prefix (prefix_name);

    if (TERM_COMBINED)
	printf ("\"%s%s\", ", prefix, value);
    else
	printf ("[\"%s\", \"%s\"], ", value, prefix);
}

static void
add_address_name (InternetAddress *address, const char *prefix_name)
{
    const char *name;

    name = internet_address_get_name (address);

    if (name)
	print_term (prefix_name, name);
}

static void
add_address_names (InternetAddressList *addresses, const char *address_type)
{
    int i;
    InternetAddress *address;

    for (i = 0; i < internet_address_list_length (addresses); i++) {
	address = internet_address_list_get_address (addresses, i);
	add_address_name (address, address_type);
	add_address_name (address, "name");
	add_address_name (address, "body");
    }
}

static void
add_address_addr (InternetAddress *address, const char *prefix_name)
{
    InternetAddressMailbox *mailbox = INTERNET_ADDRESS_MAILBOX (address);
    const char *addr;

    addr = internet_address_mailbox_get_addr (mailbox);

    if (addr)
	print_term (prefix_name, addr);
}

static void
add_address_addrs (InternetAddressList *addresses, const char *address_type)
{
    int i;
    InternetAddress *address;

    for (i = 0; i < internet_address_list_length (addresses); i++) {
	address = internet_address_list_get_address (addresses, i);
	add_address_addr (address, address_type);
	add_address_addr (address, "email");
    }
}

int
main (int argc, char **argv)
{
    GMimeStream *stream;
    GMimeParser *parser;
    GMimeMessage *message;
    InternetAddressList *addresses;

    const char *filename;
    FILE *file;

    const char *value, *from;
    int i;

    time_t time;
    struct tm gm_time_tm;
    char time_str[16]; /* YYYYMMDDHHMMSS + 1 for Y100k compatibility ;-) */

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

    printf ("text is:\n[");
    from = g_mime_message_get_sender (message);
    addresses = internet_address_list_parse_string (from);

    add_address_names (addresses, "from_name");

    add_address_names (g_mime_message_get_all_recipients (message),
		       "to_name");

    value = g_mime_message_get_subject (message);
    print_term ("subject", value);
    print_term ("body", value);

    printf ("]\nterms is:\n[");

    TERM_COMBINED = 1;

    from = g_mime_message_get_sender (message);
    addresses = internet_address_list_parse_string (from);

    add_address_addrs (addresses, "from_email");

    add_address_addrs (g_mime_message_get_all_recipients (message),
		       "to_email");

    g_mime_message_get_date (message, &time, NULL);

    gmtime_r (&time, &gm_time_tm);

    if (strftime (time_str, sizeof (time_str),
		  "%Y%m%d%H%M%S", &gm_time_tm) == 0) {
	fprintf (stderr, "Internal error formatting time\n");
	exit (1);
    }

    print_term ("date", time_str);

    print_term ("label", "inbox");
    print_term ("label", "unread");
    print_term ("type", "mail");

    value = g_mime_message_get_message_id (message);
    print_term ("msgid", value);

    print_term ("source_id", "1");

    value = g_mime_message_get_message_id (message);
    print_term ("thread", value);

    printf ("]\n");

    g_object_unref (message);
    g_object_unref (parser);
    g_object_unref (stream);
    
    return 0;
}
