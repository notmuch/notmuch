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

#include "notmuch-private.h"

#include <gmime/gmime.h>

#include <xapian.h>

/* We're finally down to a single (NAME + address) email "mailbox". */
static void
_index_address_mailbox (notmuch_message_t *message,
			const char *prefix_name,
			InternetAddress *address)
{
    InternetAddressMailbox *mailbox = INTERNET_ADDRESS_MAILBOX (address);
    const char *name, *addr;
    void *local = talloc_new (NULL);

    name = internet_address_get_name (address);
    addr = internet_address_mailbox_get_addr (mailbox);

    /* In the absence of a name, we'll strip the part before the @
     * from the address. */
    if (! name) {
	const char *at;

	at = strchr (addr, '@');
	if (at)
	    name = talloc_strndup (local, addr, at - addr);
    }

    if (name)
	_notmuch_message_gen_terms (message, prefix_name, name);
    if (addr)
	_notmuch_message_gen_terms (message, prefix_name, addr);

    talloc_free (local);
}

static void
_index_address_list (notmuch_message_t *message,
		     const char *prefix_name,
		     InternetAddressList *addresses);

/* The outer loop over the InternetAddressList wasn't quite enough.
 * There can actually be a tree here where a single member of the list
 * is a "group" containing another list. Recurse please.
 */
static void
_index_address_group (notmuch_message_t *message,
		      const char *prefix_name,
		      InternetAddress *address)
{
    InternetAddressGroup *group;
    InternetAddressList *list;

    group = INTERNET_ADDRESS_GROUP (address);
    list = internet_address_group_get_members (group);

    if (! list)
	return;

    _index_address_list (message, prefix_name, list);
}

static void
_index_address_list (notmuch_message_t *message,
		     const char *prefix_name,
		     InternetAddressList *addresses)
{
    int i;
    InternetAddress *address;

    if (addresses == NULL)
	return;

    for (i = 0; i < internet_address_list_length (addresses); i++) {
	address = internet_address_list_get_address (addresses, i);
	if (INTERNET_ADDRESS_IS_MAILBOX (address)) {
	    _index_address_mailbox (message, prefix_name, address);
	} else if (INTERNET_ADDRESS_IS_GROUP (address)) {
	    _index_address_group (message, prefix_name, address);
	} else {
	    INTERNAL_ERROR ("GMime InternetAddress is neither a mailbox nor a group.\n");
	}
    }
}

static const char *
skip_re_in_subject (const char *subject)
{
    const char *s = subject;

    if (subject == NULL)
	return NULL;

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

/* Given a string representing the body of a message, generate terms
 * for it, (skipping quoted portions and signatures).
 *
 * This function is evil in that it modifies the string passed to it,
 * (changing some newlines into '\0').
 */
static void
_index_body_text (notmuch_message_t *message, char *body)
{
    char *line, *line_end, *next_line;

    if (body == NULL)
	return;

    next_line = body;

    while (1) {
	line = next_line;
	if (*line == '\0')
	    break;

	next_line = strchr (line, '\n');
	if (next_line == NULL) {
	    next_line = line + strlen (line);
	}
	line_end = next_line - 1;

	/* Get to the next non-blank line. */
	while (*next_line == '\n')
	    next_line++;

	/* Skip blank lines. */
	if (line_end < line)
	    continue;

	/* Skip lines that are quotes. */
	if (*line == '>')
	    continue;

	/* Also skip lines introducing a quote on the next line. */
	if (*line_end == ':' && *next_line == '>')
	    continue;

	/* Finally, bail as soon as we see a signature. */
	/* XXX: Should only do this if "near" the end of the message. */
	if (strncmp (line, "-- ", 3) == 0)
	    break;

	*(line_end + 1) = '\0';

	_notmuch_message_gen_terms (message, NULL, line);
    }
}

/* Callback to generate terms for each mime part of a message. */
static void
_index_mime_part (notmuch_message_t *message,
		  GMimeObject *part)
{
    GMimeStream *stream;
    GMimeDataWrapper *wrapper;
    GByteArray *byte_array;
    GMimeContentDisposition *disposition;
    char *body;

    if (GMIME_IS_MULTIPART (part)) {
	GMimeMultipart *multipart = GMIME_MULTIPART (part);
	int i;

	for (i = 0; i < g_mime_multipart_get_count (multipart); i++) {
	    if (GMIME_IS_MULTIPART_SIGNED (multipart)) {
		/* Don't index the signature. */
		if (i == 1)
		    continue;
		if (i > 1)
		    fprintf (stderr, "Warning: Unexpected extra parts of multipart/signed. Indexing anyway.\n");
	    }
	    _index_mime_part (message,
			      g_mime_multipart_get_part (multipart, i));
	}
	return;
    }

    if (GMIME_IS_MESSAGE_PART (part)) {
	GMimeMessage *mime_message;

	mime_message = g_mime_message_part_get_message (GMIME_MESSAGE_PART (part));

	_index_mime_part (message, g_mime_message_get_mime_part (mime_message));

	return;
    }

    if (! (GMIME_IS_PART (part))) {
	fprintf (stderr, "Warning: Not indexing unknown mime part: %s.\n",
		 g_type_name (G_OBJECT_TYPE (part)));
	return;
    }

    disposition = g_mime_object_get_content_disposition (part);
    if (disposition &&
	strcmp (disposition->disposition, GMIME_DISPOSITION_ATTACHMENT) == 0)
    {
	const char *filename = g_mime_part_get_filename (GMIME_PART (part));

	_notmuch_message_add_term (message, "tag", "attachment");
	_notmuch_message_gen_terms (message, "attachment", filename);

	/* XXX: Would be nice to call out to something here to parse
	 * the attachment into text and then index that. */
	return;
    }

    byte_array = g_byte_array_new ();

    stream = g_mime_stream_mem_new_with_byte_array (byte_array);
    g_mime_stream_mem_set_owner (GMIME_STREAM_MEM (stream), FALSE);
    wrapper = g_mime_part_get_content_object (GMIME_PART (part));
    if (wrapper)
	g_mime_data_wrapper_write_to_stream (wrapper, stream);

    g_object_unref (stream);

    g_byte_array_append (byte_array, (guint8 *) "\0", 1);
    body = (char *) g_byte_array_free (byte_array, FALSE);

    _index_body_text (message, body);

    free (body);
}

notmuch_status_t
_notmuch_message_index_file (notmuch_message_t *message,
			     const char *filename)
{
    GMimeStream *stream = NULL;
    GMimeParser *parser = NULL;
    GMimeMessage *mime_message = NULL;
    InternetAddressList *addresses;
    FILE *file = NULL;
    const char *from, *subject;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;
    static int initialized = 0;

    if (! initialized) {
	g_mime_init (0);
	initialized = 1;
    }

    file = fopen (filename, "r");
    if (! file) {
	fprintf (stderr, "Error opening %s: %s\n", filename, strerror (errno));
	ret = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    /* Evil GMime steals my FILE* here so I won't fclose it. */
    stream = g_mime_stream_file_new (file);

    parser = g_mime_parser_new_with_stream (stream);

    mime_message = g_mime_parser_construct_message (parser);

    from = g_mime_message_get_sender (mime_message);
    addresses = internet_address_list_parse_string (from);

    _index_address_list (message, "from", addresses);

    addresses = g_mime_message_get_all_recipients (mime_message);
    _index_address_list (message, "to", addresses);

    subject = g_mime_message_get_subject (mime_message);
    subject = skip_re_in_subject (subject);
    _notmuch_message_gen_terms (message, "subject", subject);

    _index_mime_part (message, g_mime_message_get_mime_part (mime_message));

  DONE:
    if (mime_message)
	g_object_unref (mime_message);

    if (parser)
	g_object_unref (parser);

    if (stream)
	g_object_unref (stream);

    return ret;
}
