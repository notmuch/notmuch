/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2009 Carl Worth
 * Copyright © 2009 Keith Packard
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
 * Authors: Carl Worth <cworth@cworth.org>
 *	    Keith Packard <keithp@keithp.com>
 */

#include "notmuch-client.h"
#include "gmime-filter-reply.h"

static void
reply_part(GMimeObject *part, int *part_count)
{
    GMimeContentDisposition *disposition;
    GMimeContentType *content_type;
    GMimeDataWrapper *wrapper;

    (void) part_count;
    disposition = g_mime_object_get_content_disposition (part);
    if (disposition &&
	strcmp (disposition->disposition, GMIME_DISPOSITION_ATTACHMENT) == 0)
    {
	const char *filename = g_mime_part_get_filename (GMIME_PART (part));
	content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

	printf ("Attachment: %s (%s)\n", filename,
		g_mime_content_type_to_string (content_type));
	return;
    }

    content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

    if (g_mime_content_type_is_type (content_type, "text", "*") &&
	!g_mime_content_type_is_type (content_type, "text", "html"))
    {
	GMimeStream *stream_stdout = NULL, *stream_filter = NULL;
	stream_stdout = g_mime_stream_file_new (stdout);
	if (stream_stdout) {
	    g_mime_stream_file_set_owner (GMIME_STREAM_FILE (stream_stdout), FALSE);
	    stream_filter = g_mime_stream_filter_new(stream_stdout);
	}
	g_mime_stream_filter_add(GMIME_STREAM_FILTER(stream_filter),
				 g_mime_filter_reply_new(TRUE));
	wrapper = g_mime_part_get_content_object (GMIME_PART (part));
	if (wrapper && stream_filter)
	    g_mime_data_wrapper_write_to_stream (wrapper, stream_filter);
	if (stream_filter)
	    g_object_unref(stream_filter);
	if (stream_stdout)
	    g_object_unref(stream_stdout);
    }
    else
    {
	printf ("Non-text part: %s\n",
		g_mime_content_type_to_string (content_type));
    }
}

static void
add_recipients_for_address_list (GMimeMessage *message,
				 GMimeRecipientType type,
				 InternetAddressList *list)
{
    InternetAddress *address;
    int i;

    for (i = 0; i < internet_address_list_length (list); i++) {
	address = internet_address_list_get_address (list, i);
	if (INTERNET_ADDRESS_IS_GROUP (address)) {
	    InternetAddressGroup *group;
	    InternetAddressList *group_list;

	    group = INTERNET_ADDRESS_GROUP (address);
	    group_list = internet_address_group_get_members (group);
	    if (group_list == NULL)
		continue;

	    add_recipients_for_address_list (message, type, group_list);
	} else {
	    InternetAddressMailbox *mailbox;
	    const char *name;
	    const char *addr;

	    mailbox = INTERNET_ADDRESS_MAILBOX (address);

	    name = internet_address_get_name (address);
	    addr = internet_address_mailbox_get_addr (mailbox);

	    g_mime_message_add_recipient (message, type, name, addr);
	}
    }
}

static void
add_recipients_for_string (GMimeMessage *message,
			   GMimeRecipientType type,
			   const char *recipients)
{
    InternetAddressList *list;

    list = internet_address_list_parse_string (recipients);
    if (list == NULL)
	return;

    add_recipients_for_address_list (message, type, list);
}

int
notmuch_reply_command (void *ctx, int argc, char *argv[])
{
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    notmuch_query_t *query;
    GMimeMessage *reply;
    char *query_string;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    int ret = 0;
    const char *subject, *recipients;
    const char *in_reply_to, *orig_references, *references;
    char *reply_headers;
    struct {
	const char *header;
	GMimeRecipientType recipient_type;
    } reply_to_map[] = {
	{ "from", GMIME_RECIPIENT_TYPE_TO  },
	{ "to",   GMIME_RECIPIENT_TYPE_TO  },
	{ "cc",   GMIME_RECIPIENT_TYPE_CC  },
	{ "bcc",  GMIME_RECIPIENT_TYPE_BCC }
    };
    unsigned int i;

    config = notmuch_config_open (ctx, NULL, NULL);
    if (config == NULL)
	return 1;

    notmuch = notmuch_database_open (notmuch_config_get_database_path (config));
    if (notmuch == NULL)
	return 1;

    query_string = query_string_from_args (ctx, argc, argv);
    if (query_string == NULL) {
	fprintf (stderr, "Out of memory\n");
	return 1;
    }

    query = notmuch_query_create (notmuch, query_string);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	return 1;
    }

    for (messages = notmuch_query_search_messages (query);
	 notmuch_messages_has_more (messages);
	 notmuch_messages_advance (messages))
    {
	message = notmuch_messages_get (messages);

	/* The 1 means we want headers in a "pretty" order. */
	reply = g_mime_message_new (1);
	if (reply == NULL) {
	    fprintf (stderr, "Out of memory\n");
	    return 1;
	}

	/* XXX: We need a configured email address (or addresses) for
	 * the user here, so that we can prevent replying to the user,
	 * and also call _mime_message_set_sender to set From: (either
	 * from the first "owned" address mentioned as a recipient in
	 * the original message, or else some default address).
	 */

	subject = notmuch_message_get_header (message, "subject");

	if (strncasecmp (subject, "Re:", 3))
	    subject = talloc_asprintf (ctx, "Re: %s", subject);
	g_mime_message_set_subject (reply, subject);

	for (i = 0; i < ARRAY_SIZE (reply_to_map); i++) {
	    recipients = notmuch_message_get_header (message,
						     reply_to_map[i].header);
	    add_recipients_for_string (reply,
				       reply_to_map[i].recipient_type,
				       recipients);
	}

	in_reply_to = talloc_asprintf (ctx, "<%s>",
			     notmuch_message_get_message_id (message));

	g_mime_object_set_header (GMIME_OBJECT (reply),
				  "In-Reply-To", in_reply_to);

	orig_references = notmuch_message_get_header (message, "references");
	references = talloc_asprintf (ctx, "%s%s%s",
				      orig_references ? orig_references : "",
				      orig_references ? " " : "",
				      in_reply_to);
	g_mime_object_set_header (GMIME_OBJECT (reply),
				  "References", references);

	reply_headers = g_mime_object_to_string (GMIME_OBJECT (reply));
	printf ("%s", reply_headers);
	free (reply_headers);

	g_object_unref (G_OBJECT (reply));
	reply = NULL;

	printf ("On %s, %s wrote:\n",
		notmuch_message_get_header (message, "date"),
		notmuch_message_get_header (message, "from"));

	show_message_body (notmuch_message_get_filename (message), reply_part);

	notmuch_message_destroy (message);
    }

    notmuch_query_destroy (query);
    notmuch_database_close (notmuch);

    return ret;
}
