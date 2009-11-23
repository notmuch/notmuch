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
reply_part_content (GMimeObject *part)
{
    GMimeStream *stream_stdout = NULL, *stream_filter = NULL;
    GMimeDataWrapper *wrapper;

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

static void
reply_part (GMimeObject *part, int *part_count)
{
    GMimeContentDisposition *disposition;
    GMimeContentType *content_type;

    (void) part_count;
    disposition = g_mime_object_get_content_disposition (part);
    if (disposition &&
	strcmp (disposition->disposition, GMIME_DISPOSITION_ATTACHMENT) == 0)
    {
	const char *filename = g_mime_part_get_filename (GMIME_PART (part));
	content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

	if (g_mime_content_type_is_type (content_type, "text", "*") &&
	    !g_mime_content_type_is_type (content_type, "text", "html"))
	{
	    reply_part_content (part);
	}
	else
	{
	    printf ("Attachment: %s (%s)\n", filename,
		    g_mime_content_type_to_string (content_type));
	}

	return;
    }

    content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

    if (g_mime_content_type_is_type (content_type, "text", "*") &&
	!g_mime_content_type_is_type (content_type, "text", "html"))
    {
	reply_part_content (part);
    }
    else
    {
	printf ("Non-text part: %s\n",
		g_mime_content_type_to_string (content_type));
    }
}

/* Is the given address configured as one of the user's "personal" or
 * "other" addresses. */
static int
address_is_users (const char *address, notmuch_config_t *config)
{
    const char *primary;
    char **other;
    size_t i, other_len;

    primary = notmuch_config_get_user_primary_email (config);
    if (strcasecmp (primary, address) == 0)
	return 1;

    other = notmuch_config_get_user_other_email (config, &other_len);
    for (i = 0; i < other_len; i++)
	if (strcasecmp (other[i], address) == 0)
	    return 1;

    return 0;
}

/* For each address in 'list' that is not configured as one of the
 * user's addresses in 'config', add that address to 'message' as an
 * address of 'type'.
 *
 * The first address encountered that *is* the user's address will be
 * returned, (otherwise NULL is returned).
 */
static const char *
add_recipients_for_address_list (GMimeMessage *message,
				 notmuch_config_t *config,
				 GMimeRecipientType type,
				 InternetAddressList *list)
{
    InternetAddress *address;
    int i;
    const char *ret = NULL;

    for (i = 0; i < internet_address_list_length (list); i++) {
	address = internet_address_list_get_address (list, i);
	if (INTERNET_ADDRESS_IS_GROUP (address)) {
	    InternetAddressGroup *group;
	    InternetAddressList *group_list;

	    group = INTERNET_ADDRESS_GROUP (address);
	    group_list = internet_address_group_get_members (group);
	    if (group_list == NULL)
		continue;

	    add_recipients_for_address_list (message, config,
					     type, group_list);
	} else {
	    InternetAddressMailbox *mailbox;
	    const char *name;
	    const char *addr;

	    mailbox = INTERNET_ADDRESS_MAILBOX (address);

	    name = internet_address_get_name (address);
	    addr = internet_address_mailbox_get_addr (mailbox);

	    if (address_is_users (addr, config)) {
		if (ret == NULL)
		    ret = addr;
	    } else {
		g_mime_message_add_recipient (message, type, name, addr);
	    }
	}
    }

    return ret;
}

/* For each address in 'recipients' that is not configured as one of
 * the user's addresses in 'config', add that address to 'message' as
 * an address of 'type'.
 *
 * The first address encountered that *is* the user's address will be
 * returned, (otherwise NULL is returned).
 */
static const char *
add_recipients_for_string (GMimeMessage *message,
			   notmuch_config_t *config,
			   GMimeRecipientType type,
			   const char *recipients)
{
    InternetAddressList *list;

    list = internet_address_list_parse_string (recipients);
    if (list == NULL)
	return NULL;

    return add_recipients_for_address_list (message, config, type, list);
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
    const char *subject, *recipients, *from_addr = NULL;
    const char *in_reply_to, *orig_references, *references;
    char *reply_headers;
    struct {
	const char *header;
	const char *fallback;
	GMimeRecipientType recipient_type;
    } reply_to_map[] = {
	{ "reply-to", "from", GMIME_RECIPIENT_TYPE_TO  },
	{ "to",         NULL, GMIME_RECIPIENT_TYPE_TO  },
	{ "cc",         NULL, GMIME_RECIPIENT_TYPE_CC  },
	{ "bcc",        NULL, GMIME_RECIPIENT_TYPE_BCC }
    };
    unsigned int i;

    config = notmuch_config_open (ctx, NULL, NULL);
    if (config == NULL)
	return 1;

    query_string = query_string_from_args (ctx, argc, argv);
    if (query_string == NULL) {
	fprintf (stderr, "Out of memory\n");
	return 1;
    }

    if (*query_string == '\0') {
	fprintf (stderr, "Error: notmuch reply requires at least one search term.\n");
	return 1;
    }

    notmuch = notmuch_database_open (notmuch_config_get_database_path (config),
				     NOTMUCH_DATABASE_MODE_READ_ONLY);
    if (notmuch == NULL)
	return 1;

    query = notmuch_query_create (notmuch, query_string);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	return 1;
    }

    for (messages = notmuch_query_search_messages (query, 0, -1);
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

	subject = notmuch_message_get_header (message, "subject");

	if (strncasecmp (subject, "Re:", 3))
	    subject = talloc_asprintf (ctx, "Re: %s", subject);
	g_mime_message_set_subject (reply, subject);

	for (i = 0; i < ARRAY_SIZE (reply_to_map); i++) {
	    const char *addr;

	    recipients = notmuch_message_get_header (message,
						     reply_to_map[i].header);
	    if ((recipients == NULL || recipients[0] == '\0') && reply_to_map[i].fallback)
		recipients = notmuch_message_get_header (message,
							 reply_to_map[i].fallback);

	    addr = add_recipients_for_string (reply, config,
					      reply_to_map[i].recipient_type,
					      recipients);
	    if (from_addr == NULL)
		from_addr = addr;
	}

	if (from_addr == NULL)
	    from_addr = notmuch_config_get_user_primary_email (config);

	from_addr = talloc_asprintf (ctx, "%s <%s>",
				     notmuch_config_get_user_name (config),
				     from_addr);
	g_mime_object_set_header (GMIME_OBJECT (reply),
				  "From", from_addr);

	g_mime_object_set_header (GMIME_OBJECT (reply), "Bcc",
			   notmuch_config_get_user_primary_email (config));

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
