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
#include "gmime-filter-headers.h"

static void
show_reply_headers (GMimeMessage *message)
{
    GMimeStream *stream_stdout = NULL, *stream_filter = NULL;

    stream_stdout = g_mime_stream_file_new (stdout);
    if (stream_stdout) {
	g_mime_stream_file_set_owner (GMIME_STREAM_FILE (stream_stdout), FALSE);
	stream_filter = g_mime_stream_filter_new(stream_stdout);
	if (stream_filter) {
		g_mime_stream_filter_add(GMIME_STREAM_FILTER(stream_filter),
					 g_mime_filter_headers_new());
		g_mime_object_write_to_stream(GMIME_OBJECT(message), stream_filter);
		g_object_unref(stream_filter);
	}
	g_object_unref(stream_stdout);
    }
}

static void
format_part_reply (mime_node_t *node)
{
    int i;

    if (node->envelope_file) {
	printf ("On %s, %s wrote:\n",
		notmuch_message_get_header (node->envelope_file, "date"),
		notmuch_message_get_header (node->envelope_file, "from"));
    } else if (GMIME_IS_MESSAGE (node->part)) {
	GMimeMessage *message = GMIME_MESSAGE (node->part);
	InternetAddressList *recipients;
	const char *recipients_string;

	printf ("> From: %s\n", g_mime_message_get_sender (message));
	recipients = g_mime_message_get_recipients (message, GMIME_RECIPIENT_TYPE_TO);
	recipients_string = internet_address_list_to_string (recipients, 0);
	if (recipients_string)
	    printf ("> To: %s\n",
		    recipients_string);
	recipients = g_mime_message_get_recipients (message, GMIME_RECIPIENT_TYPE_CC);
	recipients_string = internet_address_list_to_string (recipients, 0);
	if (recipients_string)
	    printf ("> Cc: %s\n",
		    recipients_string);
	printf ("> Subject: %s\n", g_mime_message_get_subject (message));
	printf ("> Date: %s\n", g_mime_message_get_date_as_string (message));
	printf (">\n");
    } else if (GMIME_IS_PART (node->part)) {
	GMimeContentType *content_type = g_mime_object_get_content_type (node->part);
	GMimeContentDisposition *disposition = g_mime_object_get_content_disposition (node->part);

	if (g_mime_content_type_is_type (content_type, "application", "pgp-encrypted") ||
	    g_mime_content_type_is_type (content_type, "application", "pgp-signature")) {
	    /* Ignore PGP/MIME cruft parts */
	} else if (g_mime_content_type_is_type (content_type, "text", "*") &&
		   !g_mime_content_type_is_type (content_type, "text", "html")) {
	    GMimeStream *stream_stdout = g_mime_stream_file_new (stdout);
	    g_mime_stream_file_set_owner (GMIME_STREAM_FILE (stream_stdout), FALSE);
	    show_text_part_content (node->part, stream_stdout, NOTMUCH_SHOW_TEXT_PART_REPLY);
	    g_object_unref(stream_stdout);
	} else if (disposition &&
		   strcmp (disposition->disposition, GMIME_DISPOSITION_ATTACHMENT) == 0) {
	    const char *filename = g_mime_part_get_filename (GMIME_PART (node->part));
	    printf ("Attachment: %s (%s)\n", filename,
		    g_mime_content_type_to_string (content_type));
	} else {
	    printf ("Non-text part: %s\n",
		    g_mime_content_type_to_string (content_type));
	}
    }

    for (i = 0; i < node->nchildren; i++)
	format_part_reply (mime_node_child (node, i));
}

typedef enum {
    USER_ADDRESS_IN_STRING,
    STRING_IN_USER_ADDRESS,
    STRING_IS_USER_ADDRESS,
} address_match_t;

/* Match given string against given address according to mode. */
static notmuch_bool_t
match_address (const char *str, const char *address, address_match_t mode)
{
    switch (mode) {
    case USER_ADDRESS_IN_STRING:
	return strcasestr (str, address) != NULL;
    case STRING_IN_USER_ADDRESS:
	return strcasestr (address, str) != NULL;
    case STRING_IS_USER_ADDRESS:
	return strcasecmp (address, str) == 0;
    }

    return FALSE;
}

/* Match given string against user's configured "primary" and "other"
 * addresses according to mode. */
static const char *
address_match (const char *str, notmuch_config_t *config, address_match_t mode)
{
    const char *primary;
    const char **other;
    size_t i, other_len;

    if (!str || *str == '\0')
	return NULL;

    primary = notmuch_config_get_user_primary_email (config);
    if (match_address (str, primary, mode))
	return primary;

    other = notmuch_config_get_user_other_email (config, &other_len);
    for (i = 0; i < other_len; i++) {
	if (match_address (str, other[i], mode))
	    return other[i];
    }

    return NULL;
}

/* Does the given string contain an address configured as one of the
 * user's "primary" or "other" addresses. If so, return the matching
 * address, NULL otherwise. */
static const char *
user_address_in_string (const char *str, notmuch_config_t *config)
{
    return address_match (str, config, USER_ADDRESS_IN_STRING);
}

/* Do any of the addresses configured as one of the user's "primary"
 * or "other" addresses contain the given string. If so, return the
 * matching address, NULL otherwise. */
static const char *
string_in_user_address (const char *str, notmuch_config_t *config)
{
    return address_match (str, config, STRING_IN_USER_ADDRESS);
}

/* Is the given address configured as one of the user's "primary" or
 * "other" addresses. */
static notmuch_bool_t
address_is_users (const char *address, notmuch_config_t *config)
{
    return address_match (address, config, STRING_IS_USER_ADDRESS) != NULL;
}

/* Scan addresses in 'list'.
 *
 * If 'message' is non-NULL, then for each address in 'list' that is
 * not configured as one of the user's addresses in 'config', add that
 * address to 'message' as an address of 'type'.
 *
 * If 'user_from' is non-NULL and *user_from is NULL, *user_from will
 * be set to the first address encountered in 'list' that is the
 * user's address.
 *
 * Return the number of addresses added to 'message'. (If 'message' is
 * NULL, the function returns 0 by definition.)
 */
static unsigned int
scan_address_list (InternetAddressList *list,
		   notmuch_config_t *config,
		   GMimeMessage *message,
		   GMimeRecipientType type,
		   const char **user_from)
{
    InternetAddress *address;
    int i;
    unsigned int n = 0;

    for (i = 0; i < internet_address_list_length (list); i++) {
	address = internet_address_list_get_address (list, i);
	if (INTERNET_ADDRESS_IS_GROUP (address)) {
	    InternetAddressGroup *group;
	    InternetAddressList *group_list;

	    group = INTERNET_ADDRESS_GROUP (address);
	    group_list = internet_address_group_get_members (group);
	    if (group_list == NULL)
		continue;

	    n += scan_address_list (group_list, config, message, type, user_from);
	} else {
	    InternetAddressMailbox *mailbox;
	    const char *name;
	    const char *addr;

	    mailbox = INTERNET_ADDRESS_MAILBOX (address);

	    name = internet_address_get_name (address);
	    addr = internet_address_mailbox_get_addr (mailbox);

	    if (address_is_users (addr, config)) {
		if (user_from && *user_from == NULL)
		    *user_from = addr;
	    } else if (message) {
		g_mime_message_add_recipient (message, type, name, addr);
		n++;
	    }
	}
    }

    return n;
}

/* Scan addresses in 'recipients'.
 *
 * See the documentation of scan_address_list() above. This function
 * does exactly the same, but converts 'recipients' to an
 * InternetAddressList first.
 */
static unsigned int
scan_address_string (const char *recipients,
		     notmuch_config_t *config,
		     GMimeMessage *message,
		     GMimeRecipientType type,
		     const char **user_from)
{
    InternetAddressList *list;

    if (recipients == NULL)
	return 0;

    list = internet_address_list_parse_string (recipients);
    if (list == NULL)
	return 0;

    return scan_address_list (list, config, message, type, user_from);
}

/* Does the address in the Reply-To header of 'message' already appear
 * in either the 'To' or 'Cc' header of the message?
 */
static int
reply_to_header_is_redundant (notmuch_message_t *message)
{
    const char *reply_to, *to, *cc, *addr;
    InternetAddressList *list;
    InternetAddress *address;
    InternetAddressMailbox *mailbox;

    reply_to = notmuch_message_get_header (message, "reply-to");
    if (reply_to == NULL || *reply_to == '\0')
	return 0;

    list = internet_address_list_parse_string (reply_to);

    if (internet_address_list_length (list) != 1)
	return 0;

    address = internet_address_list_get_address (list, 0);
    if (INTERNET_ADDRESS_IS_GROUP (address))
	return 0;

    mailbox = INTERNET_ADDRESS_MAILBOX (address);
    addr = internet_address_mailbox_get_addr (mailbox);

    to = notmuch_message_get_header (message, "to");
    cc = notmuch_message_get_header (message, "cc");

    if ((to && strstr (to, addr) != 0) ||
	(cc && strstr (cc, addr) != 0))
    {
	return 1;
    }

    return 0;
}

/* Augment the recipients of 'reply' from the "Reply-to:", "From:",
 * "To:", "Cc:", and "Bcc:" headers of 'message'.
 *
 * If 'reply_all' is true, use sender and all recipients, otherwise
 * scan the headers for the first that contains something other than
 * the user's addresses and add the recipients from this header
 * (typically this would be reply-to-sender, but also handles reply to
 * user's own message in a sensible way).
 *
 * If any of the user's addresses were found in these headers, the
 * first of these returned, otherwise NULL is returned.
 */
static const char *
add_recipients_from_message (GMimeMessage *reply,
			     notmuch_config_t *config,
			     notmuch_message_t *message,
			     notmuch_bool_t reply_all)
{
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
    const char *from_addr = NULL;
    unsigned int i;
    unsigned int n = 0;

    /* Some mailing lists munge the Reply-To header despite it being A Bad
     * Thing, see http://www.unicom.com/pw/reply-to-harmful.html
     *
     * The munging is easy to detect, because it results in a
     * redundant reply-to header, (with an address that already exists
     * in either To or Cc). So in this case, we ignore the Reply-To
     * field and use the From header. This ensures the original sender
     * will get the reply even if not subscribed to the list. Note
     * that the address in the Reply-To header will always appear in
     * the reply.
     */
    if (reply_to_header_is_redundant (message)) {
	reply_to_map[0].header = "from";
	reply_to_map[0].fallback = NULL;
    }

    for (i = 0; i < ARRAY_SIZE (reply_to_map); i++) {
	const char *recipients;

	recipients = notmuch_message_get_header (message,
						 reply_to_map[i].header);
	if ((recipients == NULL || recipients[0] == '\0') && reply_to_map[i].fallback)
	    recipients = notmuch_message_get_header (message,
						     reply_to_map[i].fallback);

	n += scan_address_string (recipients, config, reply,
				  reply_to_map[i].recipient_type, &from_addr);

	if (!reply_all && n) {
	    /* Stop adding new recipients in reply-to-sender mode if
	     * we have added some recipient(s) above.
	     *
	     * This also handles the case of user replying to his own
	     * message, where reply-to/from is not a recipient. In
	     * this case there may be more than one recipient even if
	     * not replying to all.
	     */
	    reply = NULL;

	    /* From address and some recipients are enough, bail out. */
	    if (from_addr)
		break;
	}
    }

    return from_addr;
}

static const char *
guess_from_received_header (notmuch_config_t *config, notmuch_message_t *message)
{
    const char *addr, *received, *by;
    char *mta,*ptr,*token;
    char *domain=NULL;
    char *tld=NULL;
    const char *delim=". \t";
    size_t i;

    const char *to_headers[] = {
	"Envelope-to",
	"X-Original-To",
	"Delivered-To",
    };

    /* sadly, there is no standard way to find out to which email
     * address a mail was delivered - what is in the headers depends
     * on the MTAs used along the way. So we are trying a number of
     * heuristics which hopefully will answer this question.

     * We only got here if none of the users email addresses are in
     * the To: or Cc: header. From here we try the following in order:
     * 1) check for an Envelope-to: header
     * 2) check for an X-Original-To: header
     * 3) check for a Delivered-To: header
     * 4) check for a (for <email@add.res>) clause in Received: headers
     * 5) check for the domain part of known email addresses in the
     *    'by' part of Received headers
     * If none of these work, we give up and return NULL
     */
    for (i = 0; i < ARRAY_SIZE (to_headers); i++) {
	const char *tohdr = notmuch_message_get_header (message, to_headers[i]);

	/* Note: tohdr potentially contains a list of email addresses. */
	addr = user_address_in_string (tohdr, config);
	if (addr)
	    return addr;
    }

    /* We get the concatenated Received: headers and search from the
     * front (last Received: header added) and try to extract from
     * them indications to which email address this message was
     * delivered.
     * The Received: header is special in our get_header function
     * and is always concatenated.
     */
    received = notmuch_message_get_header (message, "received");
    if (received == NULL)
	return NULL;

    /* First we look for a " for <email@add.res>" in the received
     * header
     */
    ptr = strstr (received, " for ");

    /* Note: ptr potentially contains a list of email addresses. */
    addr = user_address_in_string (ptr, config);
    if (addr)
	return addr;

    /* Finally, we parse all the " by MTA ..." headers to guess the
     * email address that this was originally delivered to.
     * We extract just the MTA here by removing leading whitespace and
     * assuming that the MTA name ends at the next whitespace.
     * We test for *(by+4) to be non-'\0' to make sure there's
     * something there at all - and then assume that the first
     * whitespace delimited token that follows is the receiving
     * system in this step of the receive chain
     */
    by = received;
    while((by = strstr (by, " by ")) != NULL) {
	by += 4;
	if (*by == '\0')
	    break;
	mta = xstrdup (by);
	token = strtok(mta," \t");
	if (token == NULL) {
	    free (mta);
	    break;
	}
	/* Now extract the last two components of the MTA host name
	 * as domain and tld.
	 */
	domain = tld = NULL;
	while ((ptr = strsep (&token, delim)) != NULL) {
	    if (*ptr == '\0')
		continue;
	    domain = tld;
	    tld = ptr;
	}

	if (domain) {
	    /* Recombine domain and tld and look for it among the configured
	     * email addresses.
	     * This time we have a known domain name and nothing else - so
	     * the test is the other way around: we check if this is a
	     * substring of one of the email addresses.
	     */
	    *(tld-1) = '.';

	    addr = string_in_user_address (domain, config);
	    if (addr) {
		free (mta);
		return addr;
	    }
	}
	free (mta);
    }

    return NULL;
}

static GMimeMessage *
create_reply_message(void *ctx,
		     notmuch_config_t *config,
		     notmuch_message_t *message,
		     notmuch_bool_t reply_all)
{
    const char *subject, *from_addr = NULL;
    const char *in_reply_to, *orig_references, *references;

    /* The 1 means we want headers in a "pretty" order. */
    GMimeMessage *reply = g_mime_message_new (1);
    if (reply == NULL) {
	fprintf (stderr, "Out of memory\n");
	return NULL;
    }

    subject = notmuch_message_get_header (message, "subject");
    if (subject) {
	if (strncasecmp (subject, "Re:", 3))
	    subject = talloc_asprintf (ctx, "Re: %s", subject);
	g_mime_message_set_subject (reply, subject);
    }

    from_addr = add_recipients_from_message (reply, config,
					     message, reply_all);

    if (from_addr == NULL)
	from_addr = guess_from_received_header (config, message);

    if (from_addr == NULL)
	from_addr = notmuch_config_get_user_primary_email (config);

    from_addr = talloc_asprintf (ctx, "%s <%s>",
				 notmuch_config_get_user_name (config),
				 from_addr);
    g_mime_object_set_header (GMIME_OBJECT (reply),
			      "From", from_addr);

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

    return reply;
}

static int
notmuch_reply_format_default(void *ctx,
			     notmuch_config_t *config,
			     notmuch_query_t *query,
			     notmuch_show_params_t *params,
			     notmuch_bool_t reply_all)
{
    GMimeMessage *reply;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    mime_node_t *root;

    for (messages = notmuch_query_search_messages (query);
	 notmuch_messages_valid (messages);
	 notmuch_messages_move_to_next (messages))
    {
	message = notmuch_messages_get (messages);

	reply = create_reply_message (ctx, config, message, reply_all);

	/* If reply creation failed, we're out of memory, so don't
	 * bother trying any more messages.
	 */
	if (!reply) {
	    notmuch_message_destroy (message);
	    return 1;
	}

	show_reply_headers (reply);

	g_object_unref (G_OBJECT (reply));
	reply = NULL;

	if (mime_node_open (ctx, message, &(params->crypto), &root) == NOTMUCH_STATUS_SUCCESS) {
	    format_part_reply (root);
	    talloc_free (root);
	}

	notmuch_message_destroy (message);
    }
    return 0;
}

static int
notmuch_reply_format_json(void *ctx,
			  notmuch_config_t *config,
			  notmuch_query_t *query,
			  notmuch_show_params_t *params,
			  notmuch_bool_t reply_all)
{
    GMimeMessage *reply;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    mime_node_t *node;

    if (notmuch_query_count_messages (query) != 1) {
	fprintf (stderr, "Error: search term did not match precisely one message.\n");
	return 1;
    }

    messages = notmuch_query_search_messages (query);
    message = notmuch_messages_get (messages);
    if (mime_node_open (ctx, message, &(params->crypto), &node) != NOTMUCH_STATUS_SUCCESS)
	return 1;

    reply = create_reply_message (ctx, config, message, reply_all);
    if (!reply)
	return 1;

    /* The headers of the reply message we've created */
    printf ("{\"reply-headers\": ");
    format_headers_json (ctx, reply, TRUE);
    g_object_unref (G_OBJECT (reply));
    reply = NULL;

    /* Start the original */
    printf (", \"original\": ");

    format_part_json (ctx, node, TRUE, TRUE);

    /* End */
    printf ("}\n");
    notmuch_message_destroy (message);

    return 0;
}

/* This format is currently tuned for a git send-email --notmuch hook */
static int
notmuch_reply_format_headers_only(void *ctx,
				  notmuch_config_t *config,
				  notmuch_query_t *query,
				  unused (notmuch_show_params_t *params),
				  notmuch_bool_t reply_all)
{
    GMimeMessage *reply;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    const char *in_reply_to, *orig_references, *references;
    char *reply_headers;

    for (messages = notmuch_query_search_messages (query);
	 notmuch_messages_valid (messages);
	 notmuch_messages_move_to_next (messages))
    {
	message = notmuch_messages_get (messages);

	/* The 0 means we do not want headers in a "pretty" order. */
	reply = g_mime_message_new (0);
	if (reply == NULL) {
	    fprintf (stderr, "Out of memory\n");
	    return 1;
	}

	in_reply_to = talloc_asprintf (ctx, "<%s>",
			     notmuch_message_get_message_id (message));

        g_mime_object_set_header (GMIME_OBJECT (reply),
				  "In-Reply-To", in_reply_to);


	orig_references = notmuch_message_get_header (message, "references");

	/* We print In-Reply-To followed by References because git format-patch treats them
         * specially.  Git does not interpret the other headers specially
	 */
	references = talloc_asprintf (ctx, "%s%s%s",
				      orig_references ? orig_references : "",
				      orig_references ? " " : "",
				      in_reply_to);
	g_mime_object_set_header (GMIME_OBJECT (reply),
				  "References", references);

	(void)add_recipients_from_message (reply, config, message, reply_all);

	reply_headers = g_mime_object_to_string (GMIME_OBJECT (reply));
	printf ("%s", reply_headers);
	free (reply_headers);

	g_object_unref (G_OBJECT (reply));
	reply = NULL;

	notmuch_message_destroy (message);
    }
    return 0;
}

enum {
    FORMAT_DEFAULT,
    FORMAT_JSON,
    FORMAT_HEADERS_ONLY,
};

int
notmuch_reply_command (void *ctx, int argc, char *argv[])
{
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    notmuch_query_t *query;
    char *query_string;
    int opt_index, ret = 0;
    int (*reply_format_func)(void *ctx, notmuch_config_t *config, notmuch_query_t *query, notmuch_show_params_t *params, notmuch_bool_t reply_all);
    notmuch_show_params_t params = {
	.part = -1,
	.crypto = {
	    .verify = FALSE,
	    .decrypt = FALSE
	}
    };
    int format = FORMAT_DEFAULT;
    int reply_all = TRUE;

    notmuch_opt_desc_t options[] = {
	{ NOTMUCH_OPT_KEYWORD, &format, "format", 'f',
	  (notmuch_keyword_t []){ { "default", FORMAT_DEFAULT },
				  { "json", FORMAT_JSON },
				  { "headers-only", FORMAT_HEADERS_ONLY },
				  { 0, 0 } } },
	{ NOTMUCH_OPT_KEYWORD, &reply_all, "reply-to", 'r',
	  (notmuch_keyword_t []){ { "all", TRUE },
				  { "sender", FALSE },
				  { 0, 0 } } },
	{ NOTMUCH_OPT_BOOLEAN, &params.crypto.decrypt, "decrypt", 'd', 0 },
	{ 0, 0, 0, 0, 0 }
    };

    opt_index = parse_arguments (argc, argv, options, 1);
    if (opt_index < 0) {
	/* diagnostics already printed */
	return 1;
    }

    if (format == FORMAT_HEADERS_ONLY)
	reply_format_func = notmuch_reply_format_headers_only;
    else if (format == FORMAT_JSON)
	reply_format_func = notmuch_reply_format_json;
    else
	reply_format_func = notmuch_reply_format_default;

    config = notmuch_config_open (ctx, NULL, NULL);
    if (config == NULL)
	return 1;

    query_string = query_string_from_args (ctx, argc-opt_index, argv+opt_index);
    if (query_string == NULL) {
	fprintf (stderr, "Out of memory\n");
	return 1;
    }

    if (*query_string == '\0') {
	fprintf (stderr, "Error: notmuch reply requires at least one search term.\n");
	return 1;
    }

    if (notmuch_database_open (notmuch_config_get_database_path (config),
			       NOTMUCH_DATABASE_MODE_READ_ONLY, &notmuch))
	return 1;

    query = notmuch_query_create (notmuch, query_string);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	return 1;
    }

    if (reply_format_func (ctx, config, query, &params, reply_all) != 0)
	return 1;

    notmuch_crypto_cleanup (&params.crypto);
    notmuch_query_destroy (query);
    notmuch_database_destroy (notmuch);

    return ret;
}
