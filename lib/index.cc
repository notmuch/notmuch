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
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-private.h"

#include <gmime/gmime.h>
#include <gmime/gmime-filter.h>

#include <xapian.h>


typedef struct {
    int state;
    int a;
    int b;
    int next_if_match;
    int next_if_not_match;
} scanner_state_t;

/* Simple, linear state-transition diagram for the uuencode filter.
 *
 * If the character being processed is within the range of [a, b]
 * for the current state then we transition next_if_match
 * state. If not, we transition to the next_if_not_match state.
 *
 * The final two states are special in that they are the states in
 * which we discard data. */
static const int first_uuencode_skipping_state = 11;
static const scanner_state_t uuencode_states[] = {
    {0,  'b',  'b',  1,  0},
    {1,  'e',  'e',  2,  0},
    {2,  'g',  'g',  3,  0},
    {3,  'i',  'i',  4,  0},
    {4,  'n',  'n',  5,  0},
    {5,  ' ',  ' ',  6,  0},
    {6,  '0',  '7',  7,  0},
    {7,  '0',  '7',  8,  0},
    {8,  '0',  '7',  9,  0},
    {9,  ' ',  ' ',  10, 0},
    {10, '\n', '\n', 11, 10},
    {11, 'M',  'M',  12, 0},
    {12, ' ',  '`',  12, 11}
};

/* The following table is intended to implement this DFA (in 'dot'
   format). Note that 2 and 3 are "hidden" states used to step through
   the possible out edges of state 1.

digraph html_filter {
       0 -> 1  [label="<"];
       0 -> 0;
       1 -> 4 [label="'"];
       1 -> 5 [label="\""];
       1 -> 0 [label=">"];
       1 -> 1;
       4 -> 1 [label="'"];
       4 -> 4;
       5 -> 1 [label="\""];
       5 -> 5;
}
*/
static const int first_html_skipping_state = 1;
static const scanner_state_t html_states[] = {
    {0,  '<',  '<',  1,  0},
    {1,  '\'', '\'', 4,  2},  /* scanning for quote or > */
    {1,  '"',  '"',  5,  3},
    {1,  '>',  '>',  0,  1},
    {4,  '\'', '\'', 1,  4},  /* inside single quotes */
    {5,  '"', '"',   1,  5},  /* inside double quotes */
};

/* Oh, how I wish that gobject didn't require so much noisy boilerplate!
 * (Though I have at least eliminated some of the stock set...) */
typedef struct _NotmuchFilterDiscardNonTerm NotmuchFilterDiscardNonTerm;
typedef struct _NotmuchFilterDiscardNonTermClass NotmuchFilterDiscardNonTermClass;

/**
 * NotmuchFilterDiscardNonTerm:
 *
 * @parent_object: parent #GMimeFilter
 * @encode: encoding vs decoding
 * @state: State of the parser
 *
 * A filter to discard uuencoded portions of an email.
 *
 * A uuencoded portion is identified as beginning with a line
 * matching:
 *
 *	begin [0-7][0-7][0-7] .*
 *
 * After that detection, and beginning with the following line,
 * characters will be discarded as long as the first character of each
 * line begins with M and subsequent characters on the line are within
 * the range of ASCII characters from ' ' to '`'.
 *
 * This is not a perfect UUencode filter. It's possible to have a
 * message that will legitimately match that pattern, (so that some
 * legitimate content is discarded). And for most UUencoded files, the
 * final line of encoded data (the line not starting with M) will be
 * indexed.
 **/
struct _NotmuchFilterDiscardNonTerm {
    GMimeFilter parent_object;
    GMimeContentType *content_type;
    int state;
    int first_skipping_state;
    const scanner_state_t *states;
};

struct _NotmuchFilterDiscardNonTermClass {
    GMimeFilterClass parent_class;
};

static GMimeFilter *notmuch_filter_discard_non_term_new (GMimeContentType *content);

static void notmuch_filter_discard_non_term_finalize (GObject *object);

static GMimeFilter *filter_copy (GMimeFilter *filter);
static void filter_filter (GMimeFilter *filter, char *in, size_t len, size_t prespace,
			   char **out, size_t *outlen, size_t *outprespace);
static void filter_complete (GMimeFilter *filter, char *in, size_t len, size_t prespace,
			     char **out, size_t *outlen, size_t *outprespace);
static void filter_reset (GMimeFilter *filter);


static GMimeFilterClass *parent_class = NULL;

static void
notmuch_filter_discard_non_term_class_init (NotmuchFilterDiscardNonTermClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GMimeFilterClass *filter_class = GMIME_FILTER_CLASS (klass);

    parent_class = (GMimeFilterClass *) g_type_class_ref (GMIME_TYPE_FILTER);

    object_class->finalize = notmuch_filter_discard_non_term_finalize;

    filter_class->copy = filter_copy;
    filter_class->filter = filter_filter;
    filter_class->complete = filter_complete;
    filter_class->reset = filter_reset;
}

static void
notmuch_filter_discard_non_term_finalize (GObject *object)
{
    G_OBJECT_CLASS (parent_class)->finalize (object);
}

static GMimeFilter *
filter_copy (GMimeFilter *gmime_filter)
{
    NotmuchFilterDiscardNonTerm *filter = (NotmuchFilterDiscardNonTerm *) gmime_filter;
    return notmuch_filter_discard_non_term_new (filter->content_type);
}

static void
filter_filter (GMimeFilter *gmime_filter, char *inbuf, size_t inlen, size_t prespace,
	       char **outbuf, size_t *outlen, size_t *outprespace)
{
    NotmuchFilterDiscardNonTerm *filter = (NotmuchFilterDiscardNonTerm *) gmime_filter;
    const scanner_state_t *states = filter->states;
    register const char *inptr = inbuf;
    const char *inend = inbuf + inlen;
    char *outptr;

    (void) prespace;

    int next;

    g_mime_filter_set_size (gmime_filter, inlen, FALSE);
    outptr = gmime_filter->outbuf;

    next = filter->state;
    while (inptr < inend) {
	 /* Each state is defined by a contiguous set of rows of the
	 * state table marked by a common value for '.state'. The
	 * state numbers must be equal to the index of the first row
	 * in a given state; thus the loop condition here looks for a
	 * jump to a first row of a state, which is a real transition
	 * in the underlying DFA.
	 */
	do {
	    if (*inptr >= states[next].a && *inptr <= states[next].b)  {
		next = states[next].next_if_match;
	    } else  {
		next = states[next].next_if_not_match;
	    }

	} while (next != states[next].state);

	if (filter->state < filter->first_skipping_state)
	    *outptr++ = *inptr;

	filter->state = next;
	inptr++;
    }

    *outlen = outptr - gmime_filter->outbuf;
    *outprespace = gmime_filter->outpre;
    *outbuf = gmime_filter->outbuf;
}

static void
filter_complete (GMimeFilter *filter, char *inbuf, size_t inlen, size_t prespace,
		 char **outbuf, size_t *outlen, size_t *outprespace)
{
    if (inbuf && inlen)
	filter_filter (filter, inbuf, inlen, prespace, outbuf, outlen, outprespace);
}

static void
filter_reset (GMimeFilter *gmime_filter)
{
    NotmuchFilterDiscardNonTerm *filter = (NotmuchFilterDiscardNonTerm *) gmime_filter;

    filter->state = 0;
}

/**
 * notmuch_filter_discard_non_term_new:
 *
 * Returns: a new #NotmuchFilterDiscardNonTerm filter.
 **/
static GMimeFilter *
notmuch_filter_discard_non_term_new (GMimeContentType *content_type)
{
    static GType type = 0;
    NotmuchFilterDiscardNonTerm *filter;

    if (!type) {
	static const GTypeInfo info = {
	    sizeof (NotmuchFilterDiscardNonTermClass),
	    NULL, /* base_class_init */
	    NULL, /* base_class_finalize */
	    (GClassInitFunc) notmuch_filter_discard_non_term_class_init,
	    NULL, /* class_finalize */
	    NULL, /* class_data */
	    sizeof (NotmuchFilterDiscardNonTerm),
	    0,    /* n_preallocs */
	    NULL, /* instance_init */
	    NULL  /* value_table */
	};

	type = g_type_register_static (GMIME_TYPE_FILTER, "NotmuchFilterDiscardNonTerm", &info, (GTypeFlags) 0);
    }

    filter = (NotmuchFilterDiscardNonTerm *) g_object_newv (type, 0, NULL);
    filter->content_type = content_type;
    filter->state = 0;
    if (g_mime_content_type_is_type (content_type, "text", "html")) {
      filter->states = html_states;
      filter->first_skipping_state = first_html_skipping_state;
    } else {
      filter->states = uuencode_states;
      filter->first_skipping_state = first_uuencode_skipping_state;
    }

    return (GMimeFilter *) filter;
}

/* We're finally down to a single (NAME + address) email "mailbox". */
static void
_index_address_mailbox (notmuch_message_t *message,
			const char *prefix_name,
			InternetAddress *address)
{
    InternetAddressMailbox *mailbox = INTERNET_ADDRESS_MAILBOX (address);
    const char *name, *addr, *combined;
    void *local = talloc_new (message);

    name = internet_address_get_name (address);
    addr = internet_address_mailbox_get_addr (mailbox);

    /* Combine the name and address and index them as a phrase. */
    if (name && addr)
	combined = talloc_asprintf (local, "%s %s", name, addr);
    else if (name)
	combined = name;
    else
	combined = addr;

    if (combined)
	_notmuch_message_gen_terms (message, prefix_name, combined);

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

/* Callback to generate terms for each mime part of a message. */
static void
_index_mime_part (notmuch_message_t *message,
		  GMimeObject *part)
{
    GMimeStream *stream, *filter;
    GMimeFilter *discard_non_term_filter;
    GMimeDataWrapper *wrapper;
    GByteArray *byte_array;
    GMimeContentDisposition *disposition;
    char *body;
    const char *charset;

    if (! part) {
	_notmuch_database_log (_notmuch_message_database (message),
			      "Warning: Not indexing empty mime part.\n");
	return;
    }

    GMimeContentType *content_type = g_mime_object_get_content_type(part);
    if (content_type) {
	char *mime_string = g_mime_content_type_to_string(content_type);
	if (mime_string)
	{
	    _notmuch_message_gen_terms (message, "mimetype", mime_string);
	    g_free(mime_string);
	}
    }

    if (GMIME_IS_MULTIPART (part)) {
	GMimeMultipart *multipart = GMIME_MULTIPART (part);
	int i;

	if (GMIME_IS_MULTIPART_SIGNED (multipart))
	  _notmuch_message_add_term (message, "tag", "signed");

	if (GMIME_IS_MULTIPART_ENCRYPTED (multipart))
	  _notmuch_message_add_term (message, "tag", "encrypted");

	for (i = 0; i < g_mime_multipart_get_count (multipart); i++) {
	    if (GMIME_IS_MULTIPART_SIGNED (multipart)) {
		/* Don't index the signature. */
		if (i == 1)
		    continue;
		if (i > 1)
		    _notmuch_database_log (_notmuch_message_database (message),
					  "Warning: Unexpected extra parts of multipart/signed. Indexing anyway.\n");
	    }
	    if (GMIME_IS_MULTIPART_ENCRYPTED (multipart)) {
		/* Don't index encrypted parts. */
		continue;
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
	_notmuch_database_log (_notmuch_message_database (message),
			      "Warning: Not indexing unknown mime part: %s.\n",
			      g_type_name (G_OBJECT_TYPE (part)));
	return;
    }

    disposition = g_mime_object_get_content_disposition (part);
    if (disposition &&
	strcasecmp (g_mime_content_disposition_get_disposition (disposition),
		    GMIME_DISPOSITION_ATTACHMENT) == 0)
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

    filter = g_mime_stream_filter_new (stream);
    discard_non_term_filter = notmuch_filter_discard_non_term_new (content_type);

    g_mime_stream_filter_add (GMIME_STREAM_FILTER (filter),
			      discard_non_term_filter);

    charset = g_mime_object_get_content_type_parameter (part, "charset");
    if (charset) {
	GMimeFilter *charset_filter;
	charset_filter = g_mime_filter_charset_new (charset, "UTF-8");
	/* This result can be NULL for things like "unknown-8bit".
	 * Don't set a NULL filter as that makes GMime print
	 * annoying assertion-failure messages on stderr. */
	if (charset_filter) {
	    g_mime_stream_filter_add (GMIME_STREAM_FILTER (filter),
				      charset_filter);
	    g_object_unref (charset_filter);
	}
    }

    wrapper = g_mime_part_get_content_object (GMIME_PART (part));
    if (wrapper)
	g_mime_data_wrapper_write_to_stream (wrapper, filter);

    g_object_unref (stream);
    g_object_unref (filter);
    g_object_unref (discard_non_term_filter);

    g_byte_array_append (byte_array, (guint8 *) "\0", 1);
    body = (char *) g_byte_array_free (byte_array, FALSE);

    if (body) {
	_notmuch_message_gen_terms (message, NULL, body);

	free (body);
    }
}

notmuch_status_t
_notmuch_message_index_file (notmuch_message_t *message,
			     notmuch_message_file_t *message_file)
{
    GMimeMessage *mime_message;
    InternetAddressList *addresses;
    const char *subject;
    notmuch_status_t status;

    status = _notmuch_message_file_get_mime_message (message_file,
						     &mime_message);
    if (status)
	return status;

    addresses = g_mime_message_get_from (mime_message);
    if (addresses) {
	_index_address_list (message, "from", addresses);
	g_mime_2_6_unref (addresses);
    }

    addresses = g_mime_message_get_all_recipients (mime_message);
    if (addresses) {
	_index_address_list (message, "to", addresses);
	g_object_unref (addresses);
    }

    subject = g_mime_message_get_subject (mime_message);
    _notmuch_message_gen_terms (message, "subject", subject);

    _index_mime_part (message, g_mime_message_get_mime_part (mime_message));

    return NOTMUCH_STATUS_SUCCESS;
}
