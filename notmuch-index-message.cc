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

#include <iostream>

#include <gmime/gmime.h>

#include <xapian.h>

using namespace std;

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

/* Similarly, these value numbers are also chosen to be sup
 * compatible. */

typedef enum {
    NOTMUCH_VALUE_MESSAGE_ID = 0,
    NOTMUCH_VALUE_THREAD = 1,
    NOTMUCH_VALUE_DATE = 2
} notmuch_value_t;

static const char *
find_prefix (const char *name)
{
    unsigned int i;

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
add_term (Xapian::Document doc,
	  const char *prefix_name,
	  const char *value)
{
    const char *prefix;
    char *term;

    if (value == NULL)
	return;

    prefix = find_prefix (prefix_name);

    term = g_strdup_printf ("%s%s", prefix, value);

    doc.add_term (term);

    g_free (term);
}

static void
gen_terms (Xapian::TermGenerator term_gen,
	   const char *prefix_name,
	   const char *text)
{
    const char *prefix;

    if (text == NULL)
	return;

    prefix = find_prefix (prefix_name);

    term_gen.index_text (text, 1, prefix);
}

static void
gen_terms_address_name (Xapian::TermGenerator term_gen,
			InternetAddress *address,
			const char *prefix_name)
{
    const char *name;

    name = internet_address_get_name (address);

    if (name)
	gen_terms (term_gen, prefix_name, name);
}

static void
gen_terms_address_names (Xapian::TermGenerator term_gen,
			 InternetAddressList *addresses,
			 const char *address_type)
{
    int i;
    InternetAddress *address;

    for (i = 0; i < internet_address_list_length (addresses); i++) {
	address = internet_address_list_get_address (addresses, i);
	gen_terms_address_name (term_gen, address, address_type);
	gen_terms_address_name (term_gen, address, "name");
	gen_terms_address_name (term_gen, address, "body");
    }
}

static void
add_term_address_addr (Xapian::Document doc,
		       InternetAddress *address,
		       const char *prefix_name)
{
    InternetAddressMailbox *mailbox = INTERNET_ADDRESS_MAILBOX (address);
    const char *addr;

    addr = internet_address_mailbox_get_addr (mailbox);

    if (addr)
	add_term (doc, prefix_name, addr);
}

static void
add_terms_address_addrs (Xapian::Document doc,
			 InternetAddressList *addresses,
			 const char *address_type)
{
    int i;
    InternetAddress *address;

    for (i = 0; i < internet_address_list_length (addresses); i++) {
	address = internet_address_list_get_address (addresses, i);
	add_term_address_addr (doc, address, address_type);
	add_term_address_addr (doc, address, "email");
    }
}

static const char *
skip_re_in_subject (const char *subject)
{
    const char *s = subject;

    while (*s) {
	while (*s && isspace (*s))
	    s++;
	if (strncasecmp (s, "re:", 3) == 0)
	    s += 3;
	else
	    break;
    }

    return s;
}

/* Add a term for each message-id in the References header of the
 * message. */
static void
add_terms_references (Xapian::Document doc,
		      GMimeMessage *message)
{
    const char *refs, *end, *next;
    char *term;

    refs = g_mime_object_get_header (GMIME_OBJECT (message), "references");

    while (*refs) {
	while (*refs && isspace (*refs))
	    refs++;
	if (*refs == '<')
	    refs++;
	end = refs;
	while (*end && !isspace (*end))
	    end++;
	next = end;
	end--;
	if (end > refs && *end == '>')
	    end--;
	if (end > refs) {
	    term = g_strndup (refs, end - refs + 1);
	    add_term (doc, "ref", term);
	    g_free (term);
	}
	refs = next;
    }
}

/* Generate terms for the body of a message, given the filename of the
 * message and the offset at which the headers of the message end,
 * (and hence the body begins). */
static void
gen_terms_body (Xapian::TermGenerator term_gen,
		const char * filename,
		gint64 body_offset)
{
    GIOChannel *channel;
    GIOStatus gio_status;
    GError *error = NULL;
    char *p, *body_line = NULL, *prev_line = NULL;

    channel = g_io_channel_new_file (filename, "r", &error);
    if (channel == NULL) {
	fprintf (stderr, "Error: %s\n", error->message);
	exit (1);
    }

    gio_status = g_io_channel_seek_position (channel, body_offset,
					     G_SEEK_SET, &error);
    if (gio_status != G_IO_STATUS_NORMAL) {
	fprintf (stderr, "Error: %s\n", error->message);
	exit (1);
    }

    while (1) {
	if (body_line)
	    g_free (body_line);

	gio_status = g_io_channel_read_line (channel, &body_line,
					     NULL, NULL, &error);
	if (gio_status == G_IO_STATUS_EOF)
	    break;
	if (gio_status != G_IO_STATUS_NORMAL) {
	    fprintf (stderr, "Error: %s\n", error->message);
	    exit (1);
	}

	if (strlen (body_line) == 0)
	    continue;

	/* If the line looks like it might be introducing a quote,
	 * save it until we see if the next line begins a quote. */
	p = body_line + strlen (body_line) - 1;
	while (p > body_line and isspace (*p))
	    p--;
	if (*p == ':') {
	    prev_line = body_line;
	    body_line = NULL;
	    continue;
	}

	/* Skip quoted lines, (and previous lines that introduced them) */
	if (body_line[0] == '>') {
	    if (prev_line) {
		g_free (prev_line);
		prev_line = NULL;
	    }
	    continue;
	}

	/* Now that we're not looking at a quote we can add the prev_line */
	if (prev_line) {
	    gen_terms (term_gen, "body", prev_line);
	    g_free (prev_line);
	    prev_line = NULL;
	}

	/* Skip signatures */
	/* XXX: Should only do this if "near" the end of the message. */
	if (strncmp (body_line, "-- ", 3) == 0)
	    break;

	gen_terms (term_gen, "body", body_line);
    }

    if (body_line)
	g_free (body_line);

    g_io_channel_close (channel);
}


int
main (int argc, char **argv)
{
    GMimeStream *stream;
    GMimeParser *parser;
    GMimeMessage *message;
    InternetAddressList *addresses;

    const char *database_path, *filename;
    FILE *file;

    const char *value, *from;

    time_t time;
    struct tm gm_time_tm;
    char date_str[16]; /* YYYYMMDDHHMMSS + 1 for Y100k compatibility ;-) */

    if (argc < 3) {
	fprintf (stderr, "Usage: %s <path-to-xapian-database> <mail-message>\n",
		 argv[0]);
	exit (1);
    }

    database_path = argv[1];
    filename = argv[2];

    file = fopen (filename, "r");
    if (! file) {
	fprintf (stderr, "Error opening %s: %s\n", filename, strerror (errno));
	exit (1);
    }

    g_mime_init (0);

    stream = g_mime_stream_file_new (file);

    parser = g_mime_parser_new_with_stream (stream);

    message = g_mime_parser_construct_message (parser);

    try {
	Xapian::WritableDatabase db;
	Xapian::TermGenerator term_gen;
	Xapian::Document doc;

	doc = Xapian::Document ();

	doc.set_data (filename);

	db = Xapian::WritableDatabase (database_path,
				       Xapian::DB_CREATE_OR_OPEN);

	term_gen = Xapian::TermGenerator ();
	term_gen.set_stemmer (Xapian::Stem ("english"));

	term_gen.set_document (doc);

	from = g_mime_message_get_sender (message);
	addresses = internet_address_list_parse_string (from);

	gen_terms_address_names (term_gen, addresses, "from_name");

	addresses = g_mime_message_get_all_recipients (message);
	gen_terms_address_names (term_gen, addresses, "to_name");

	value = g_mime_message_get_subject (message);
	value = skip_re_in_subject (value);
	gen_terms (term_gen, "subject", value);
	gen_terms (term_gen, "body", value);

	gen_terms_body (term_gen, filename,
			g_mime_parser_get_headers_end (parser));

	add_terms_references (doc, message);

	from = g_mime_message_get_sender (message);
	addresses = internet_address_list_parse_string (from);

	add_terms_address_addrs (doc, addresses, "from_email");

	add_terms_address_addrs (doc,
				 g_mime_message_get_all_recipients (message),
				 "to_email");

	g_mime_message_get_date (message, &time, NULL);

	gmtime_r (&time, &gm_time_tm);

	if (strftime (date_str, sizeof (date_str),
		      "%Y%m%d%H%M%S", &gm_time_tm) == 0) {
	    fprintf (stderr, "Internal error formatting time\n");
	    exit (1);
	}

	add_term (doc, "date", date_str);

	add_term (doc, "label", "inbox");
	add_term (doc, "label", "unread");
	add_term (doc, "type", "mail");
	add_term (doc, "source_id", "1");

	value = g_mime_message_get_message_id (message);
	add_term (doc, "msgid", value);
	add_term (doc, "thread", value);

	doc.add_value (NOTMUCH_VALUE_MESSAGE_ID, value);
	doc.add_value (NOTMUCH_VALUE_THREAD, value);

	doc.add_value (NOTMUCH_VALUE_DATE, Xapian::sortable_serialise (time));

	db.add_document (doc);

    } catch (const Xapian::Error &error) {
	cerr << "A Xapian exception occurred: " << error.get_msg () << endl;
	exit (1);
    }

    g_object_unref (message);
    g_object_unref (parser);
    g_object_unref (stream);

    return 0;
}
