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
#include "gmime-filter-headers.h"

static void
reply_part_content (GMimeObject *part)
{
    GMimeStream *stream_stdout = NULL, *stream_filter = NULL;
    GMimeDataWrapper *wrapper;
    const char *charset;

    charset = g_mime_object_get_content_type_parameter (part, "charset");
    stream_stdout = g_mime_stream_file_new (stdout);
    if (stream_stdout) {
	g_mime_stream_file_set_owner (GMIME_STREAM_FILE (stream_stdout), FALSE);
	stream_filter = g_mime_stream_filter_new(stream_stdout);
        if (charset) {
          g_mime_stream_filter_add(GMIME_STREAM_FILTER(stream_filter),
                                   g_mime_filter_charset_new(charset, "UTF-8"));
        }
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

    if (recipients == NULL)
	return NULL;

    list = internet_address_list_parse_string (recipients);
    if (list == NULL)
	return NULL;

    return add_recipients_for_address_list (message, config, type, list);
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

/* Augments the recipients of reply from the headers of message.
 *
 * If any of the user's addresses were found in these headers, the first
 * of these returned, otherwise NULL is returned.
 */
static const char *
add_recipients_from_message (GMimeMessage *reply,
			     notmuch_config_t *config,
			     notmuch_message_t *message)
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

    /* Some mailing lists munge the Reply-To header despite it being A Bad
     * Thing, see http://www.unicom.com/pw/reply-to-harmful.html
     *
     * The munging is easy to detect, because it results in a
     * redundant reply-to header, (with an address that already exists
     * in either To or Cc). So in this case, we ignore the Reply-To
     * field and use the From header. Thie ensures the original sender
     * will get the reply even if not subscribed to the list. Note
     * that the address in the Reply-To header will always appear in
     * the reply.
     */
    if (reply_to_header_is_redundant (message)) {
	reply_to_map[0].header = "from";
	reply_to_map[0].fallback = NULL;
    }

    for (i = 0; i < ARRAY_SIZE (reply_to_map); i++) {
	const char *addr, *recipients;

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

    return from_addr;
}

static const char *
guess_from_received_header (notmuch_config_t *config, notmuch_message_t *message)
{
    const char *received,*primary,*by;
    char **other,*tohdr;
    char *mta,*ptr,*token;
    char *domain=NULL;
    char *tld=NULL;
    const char *delim=". \t";
    size_t i,j,other_len;

    const char *to_headers[] = {"Envelope-to", "X-Original-To"};

    primary = notmuch_config_get_user_primary_email (config);
    other = notmuch_config_get_user_other_email (config, &other_len);

    /* sadly, there is no standard way to find out to which email
     * address a mail was delivered - what is in the headers depends
     * on the MTAs used along the way. So we are trying a number of
     * heuristics which hopefully will answer this question.

     * We only got here if none of the users email addresses are in
     * the To: or Cc: header. From here we try the following in order:
     * 1) check for an Envelope-to: header
     * 2) check for an X-Original-To: header
     * 3) check for a (for <email@add.res>) clause in Received: headers
     * 4) check for the domain part of known email addresses in the
     *    'by' part of Received headers
     * If none of these work, we give up and return NULL
     */
    for (i = 0; i < sizeof(to_headers)/sizeof(*to_headers); i++) {
	tohdr = xstrdup(notmuch_message_get_header (message, to_headers[i]));
	if (tohdr && *tohdr) {
	    /* tohdr is potentialy a list of email addresses, so here we
	     * check if one of the email addresses is a substring of tohdr
	     */
	    if (strcasestr(tohdr, primary)) {
		free(tohdr);
		return primary;
	    }
	    for (j = 0; j < other_len; j++)
		if (strcasestr (tohdr, other[j])) {
		    free(tohdr);
		    return other[j];
		}
	    free(tohdr);
	}
    }

    /* We get the concatenated Received: headers and search from the
     * front (last Received: header added) and try to extract from
     * them indications to which email address this message was
     * delivered.
     * The Received: header is special in our get_header function
     * and is always concated.
     */
    received = notmuch_message_get_header (message, "received");
    if (received == NULL)
	return NULL;

    /* First we look for a " for <email@add.res>" in the received
     * header
     */
    ptr = strstr (received, " for ");
    if (ptr) {
	/* the text following is potentialy a list of email addresses,
	 * so again we check if one of the email addresses is a
	 * substring of ptr
	 */
	if (strcasestr(ptr, primary)) {
	    return primary;
	}
	for (i = 0; i < other_len; i++)
	    if (strcasestr (ptr, other[i])) {
		return other[i];
	    }
    }
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
	if (token == NULL)
	    break;
	/* Now extract the last two components of the MTA host name
	 * as domain and tld.
	 */
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

	    if (strcasestr(primary, domain)) {
		free(mta);
	    return primary;
	}
	for (i = 0; i < other_len; i++)
	    if (strcasestr (other[i],domain)) {
		free(mta);
		return other[i];
	    }
	}
	free (mta);
    }

    return NULL;
}

static int
notmuch_reply_format_default(void *ctx, notmuch_config_t *config, notmuch_query_t *query)
{
    GMimeMessage *reply;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    const char *subject, *from_addr = NULL;
    const char *in_reply_to, *orig_references, *references;

    for (messages = notmuch_query_search_messages (query);
	 notmuch_messages_valid (messages);
	 notmuch_messages_move_to_next (messages))
    {
	message = notmuch_messages_get (messages);

	/* The 1 means we want headers in a "pretty" order. */
	reply = g_mime_message_new (1);
	if (reply == NULL) {
	    fprintf (stderr, "Out of memory\n");
	    return 1;
	}

	subject = notmuch_message_get_header (message, "subject");
	if (subject) {
	    if (strncasecmp (subject, "Re:", 3))
		subject = talloc_asprintf (ctx, "Re: %s", subject);
	    g_mime_message_set_subject (reply, subject);
	}

	from_addr = add_recipients_from_message (reply, config, message);

	if (from_addr == NULL)
	    from_addr = guess_from_received_header (config, message);

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

	show_reply_headers (reply);

	g_object_unref (G_OBJECT (reply));
	reply = NULL;

	printf ("On %s, %s wrote:\n",
		notmuch_message_get_header (message, "date"),
		notmuch_message_get_header (message, "from"));

	show_message_body (notmuch_message_get_filename (message), reply_part);

	notmuch_message_destroy (message);
    }
    return 0;
}

/* This format is currently tuned for a git send-email --notmuch hook */
static int
notmuch_reply_format_headers_only(void *ctx, notmuch_config_t *config, notmuch_query_t *query)
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

	(void)add_recipients_from_message (reply, config, message);

	g_mime_object_set_header (GMIME_OBJECT (reply), "Bcc",
			   notmuch_config_get_user_primary_email (config));

	reply_headers = g_mime_object_to_string (GMIME_OBJECT (reply));
	printf ("%s", reply_headers);
	free (reply_headers);

	g_object_unref (G_OBJECT (reply));
	reply = NULL;

	notmuch_message_destroy (message);
    }
    return 0;
}

int
notmuch_reply_command (void *ctx, int argc, char *argv[])
{
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    notmuch_query_t *query;
    char *opt, *query_string;
    int i, ret = 0;
    int (*reply_format_func)(void *ctx, notmuch_config_t *config, notmuch_query_t *query);

    reply_format_func = notmuch_reply_format_default;

    for (i = 0; i < argc && argv[i][0] == '-'; i++) {
	if (strcmp (argv[i], "--") == 0) {
	    i++;
	    break;
	}
        if (STRNCMP_LITERAL (argv[i], "--format=") == 0) {
	    opt = argv[i] + sizeof ("--format=") - 1;
	    if (strcmp (opt, "default") == 0) {
		reply_format_func = notmuch_reply_format_default;
	    } else if (strcmp (opt, "headers-only") == 0) {
		reply_format_func = notmuch_reply_format_headers_only;
	    } else {
		fprintf (stderr, "Invalid value for --format: %s\n", opt);
		return 1;
	    }
	} else {
	    fprintf (stderr, "Unrecognized option: %s\n", argv[i]);
	    return 1;
	}
    }

    argc -= i;
    argv += i;

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

    if (reply_format_func (ctx, config, query) != 0)
	return 1;

    notmuch_query_destroy (query);
    notmuch_database_close (notmuch);

    return ret;
}
