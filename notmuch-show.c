/* notmuch - Not much of an email program, (just index and search)
 *
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

#include "notmuch-client.h"

typedef struct show_format {
    const char *message_set_start;
    const char *message_start;
    void (*message) (const void *ctx,
		     notmuch_message_t *message,
		     int indent);
    const char *header_start;
    void (*header) (const void *ctx,
		    notmuch_message_t *message);
    const char *header_end;
    const char *body_start;
    void (*part) (GMimeObject *part,
		  int *part_count);
    const char *body_end;
    const char *message_end;
    const char *message_set_sep;
    const char *message_set_end;
} show_format_t;

static void
format_message_text (unused (const void *ctx),
		     notmuch_message_t *message,
		     int indent);
static void
format_headers_text (const void *ctx,
		     notmuch_message_t *message);
static void
format_part_text (GMimeObject *part,
		  int *part_count);
static const show_format_t format_text = {
    "",
	"\fmessage{ ", format_message_text,
	    "\fheader{\n", format_headers_text, "\fheader}\n",
	    "\fbody{\n", format_part_text, "\fbody}\n",
	"\fmessage}\n", "",
    ""
};

static void
format_message_json (const void *ctx,
		     notmuch_message_t *message,
		     unused (int indent));
static void
format_headers_json (const void *ctx,
		     notmuch_message_t *message);
static void
format_part_json (GMimeObject *part,
		  int *part_count);
static const show_format_t format_json = {
    "[",
	"{", format_message_json,
	    ", \"headers\": {", format_headers_json, "}",
	    ", \"body\": [", format_part_json, "]",
	"}", ", ",
    "]"
};

static const char *
_get_tags_as_string (const void *ctx, notmuch_message_t *message)
{
    notmuch_tags_t *tags;
    int first = 1;
    const char *tag;
    char *result;

    result = talloc_strdup (ctx, "");
    if (result == NULL)
	return NULL;

    for (tags = notmuch_message_get_tags (message);
	 notmuch_tags_valid (tags);
	 notmuch_tags_move_to_next (tags))
    {
	tag = notmuch_tags_get (tags);

	result = talloc_asprintf_append (result, "%s%s",
					 first ? "" : " ", tag);
	first = 0;
    }

    return result;
}

/* Get a nice, single-line summary of message. */
static const char *
_get_one_line_summary (const void *ctx, notmuch_message_t *message)
{
    const char *from;
    time_t date;
    const char *relative_date;
    const char *tags;

    from = notmuch_message_get_header (message, "from");

    date = notmuch_message_get_date (message);
    relative_date = notmuch_time_relative_date (ctx, date);

    tags = _get_tags_as_string (ctx, message);

    return talloc_asprintf (ctx, "%s (%s) (%s)",
			    from, relative_date, tags);
}

static void
format_message_text (unused (const void *ctx), notmuch_message_t *message, int indent)
{
    printf ("id:%s depth:%d match:%d filename:%s\n",
	    notmuch_message_get_message_id (message),
	    indent,
	    notmuch_message_get_flag (message, NOTMUCH_MESSAGE_FLAG_MATCH),
	    notmuch_message_get_filename (message));
}

static void
format_message_json (const void *ctx, notmuch_message_t *message, unused (int indent))
{
    void *ctx_quote = talloc_new (ctx);

    printf ("\"id\": %s, \"match\": %s, \"filename\": %s",
	    json_quote_str (ctx_quote, notmuch_message_get_message_id (message)),
	    notmuch_message_get_flag (message, NOTMUCH_MESSAGE_FLAG_MATCH) ? "true" : "false",
	    json_quote_str (ctx_quote, notmuch_message_get_filename (message)));

    talloc_free (ctx_quote);
}

static void
format_headers_text (const void *ctx, notmuch_message_t *message)
{
    const char *headers[] = {
	"Subject", "From", "To", "Cc", "Bcc", "Date"
    };
    const char *name, *value;
    unsigned int i;

    printf ("%s\n", _get_one_line_summary (ctx, message));

    for (i = 0; i < ARRAY_SIZE (headers); i++) {
	name = headers[i];
	value = notmuch_message_get_header (message, name);
	if (value && strlen (value))
	    printf ("%s: %s\n", name, value);
    }
}

static void
format_headers_json (const void *ctx, notmuch_message_t *message)
{
    const char *headers[] = {
	"Subject", "From", "To", "Cc", "Bcc", "Date"
    };
    const char *name, *value;
    unsigned int i;
    int first_header = 1;
    void *ctx_quote = talloc_new (ctx);

    for (i = 0; i < ARRAY_SIZE (headers); i++) {
	name = headers[i];
	value = notmuch_message_get_header (message, name);
	if (value)
	{
	    if (!first_header)
		fputs (", ", stdout);
	    first_header = 0;

	    printf ("%s: %s",
		    json_quote_str (ctx_quote, name),
		    json_quote_str (ctx_quote, value));
	}
    }

    talloc_free (ctx_quote);
}

static void
show_part_content (GMimeObject *part, GMimeStream *stream_out)
{
    GMimeStream *stream_filter = NULL;
    GMimeDataWrapper *wrapper;
    const char *charset;

    charset = g_mime_object_get_content_type_parameter (part, "charset");

    if (stream_out) {
	stream_filter = g_mime_stream_filter_new(stream_out);
	g_mime_stream_filter_add(GMIME_STREAM_FILTER(stream_filter),
				 g_mime_filter_crlf_new(FALSE, FALSE));
        if (charset) {
          g_mime_stream_filter_add(GMIME_STREAM_FILTER(stream_filter),
                                   g_mime_filter_charset_new(charset, "UTF-8"));
        }
    }

    wrapper = g_mime_part_get_content_object (GMIME_PART (part));
    if (wrapper && stream_filter)
	g_mime_data_wrapper_write_to_stream (wrapper, stream_filter);
    if (stream_filter)
	g_object_unref(stream_filter);
}

static void
format_part_text (GMimeObject *part, int *part_count)
{
    GMimeContentDisposition *disposition;
    GMimeContentType *content_type;
    GMimeStream *stream_stdout = g_mime_stream_file_new (stdout);

    g_mime_stream_file_set_owner (GMIME_STREAM_FILE (stream_stdout), FALSE);

    disposition = g_mime_object_get_content_disposition (part);
    if (disposition &&
	strcmp (disposition->disposition, GMIME_DISPOSITION_ATTACHMENT) == 0)
    {
	const char *filename = g_mime_part_get_filename (GMIME_PART (part));
	content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

	printf ("\fattachment{ ID: %d, Content-type: %s\n",
		*part_count,
		g_mime_content_type_to_string (content_type));
	printf ("Attachment: %s (%s)\n", filename,
		g_mime_content_type_to_string (content_type));

	if (g_mime_content_type_is_type (content_type, "text", "*") &&
	    !g_mime_content_type_is_type (content_type, "text", "html"))
	{
	    show_part_content (part, stream_stdout);
	}

	printf ("\fattachment}\n");

	if (stream_stdout)
	    g_object_unref(stream_stdout);

	return;
    }

    content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

    printf ("\fpart{ ID: %d, Content-type: %s\n",
	    *part_count,
	    g_mime_content_type_to_string (content_type));

    if (g_mime_content_type_is_type (content_type, "text", "*") &&
	!g_mime_content_type_is_type (content_type, "text", "html"))
    {
	show_part_content (part, stream_stdout);
    }
    else
    {
	printf ("Non-text part: %s\n",
		g_mime_content_type_to_string (content_type));
    }

    printf ("\fpart}\n");

    if (stream_stdout)
	g_object_unref(stream_stdout);
}

static void
format_part_json (GMimeObject *part, int *part_count)
{
    GMimeContentType *content_type;
    GMimeContentDisposition *disposition;
    void *ctx = talloc_new (NULL);
    GMimeStream *stream_memory = g_mime_stream_mem_new ();
    GByteArray *part_content;

    content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

    if (*part_count > 1)
	fputs (", ", stdout);

    printf ("{\"id\": %d, \"content-type\": %s",
	    *part_count,
	    json_quote_str (ctx, g_mime_content_type_to_string (content_type)));

    disposition = g_mime_object_get_content_disposition (part);
    if (disposition &&
	strcmp (disposition->disposition, GMIME_DISPOSITION_ATTACHMENT) == 0)
    {
	const char *filename = g_mime_part_get_filename (GMIME_PART (part));

	printf (", \"filename\": %s", json_quote_str (ctx, filename));
    }

    if (g_mime_content_type_is_type (content_type, "text", "*") &&
	!g_mime_content_type_is_type (content_type, "text", "html"))
    {
	show_part_content (part, stream_memory);
	part_content = g_mime_stream_mem_get_byte_array (GMIME_STREAM_MEM (stream_memory));

	printf (", \"content\": %s", json_quote_str (ctx, (char *) part_content->data));
    }

    fputs ("}", stdout);

    talloc_free (ctx);
    if (stream_memory)
	g_object_unref (stream_memory);
}

static void
show_message (void *ctx, const show_format_t *format, notmuch_message_t *message, int indent)
{
    fputs (format->message_start, stdout);
    if (format->message)
	format->message(ctx, message, indent);

    fputs (format->header_start, stdout);
    if (format->header)
	format->header(ctx, message);
    fputs (format->header_end, stdout);

    fputs (format->body_start, stdout);
    if (format->part)
	show_message_body (notmuch_message_get_filename (message), format->part);
    fputs (format->body_end, stdout);

    fputs (format->message_end, stdout);
}


static void
show_messages (void *ctx, const show_format_t *format, notmuch_messages_t *messages, int indent,
	       notmuch_bool_t entire_thread)
{
    notmuch_message_t *message;
    notmuch_bool_t match;
    int first_set = 1;
    int next_indent;

    fputs (format->message_set_start, stdout);

    for (;
	 notmuch_messages_valid (messages);
	 notmuch_messages_move_to_next (messages))
    {
	if (!first_set)
	    fputs (format->message_set_sep, stdout);
	first_set = 0;

	fputs (format->message_set_start, stdout);

	message = notmuch_messages_get (messages);

	match = notmuch_message_get_flag (message, NOTMUCH_MESSAGE_FLAG_MATCH);

	next_indent = indent;

	if (match || entire_thread) {
	    show_message (ctx, format, message, indent);
	    next_indent = indent + 1;

	    fputs (format->message_set_sep, stdout);
	}

	show_messages (ctx, format, notmuch_message_get_replies (message),
		       next_indent, entire_thread);

	notmuch_message_destroy (message);

	fputs (format->message_set_end, stdout);
    }

    fputs (format->message_set_end, stdout);
}

int
notmuch_show_command (void *ctx, unused (int argc), unused (char *argv[]))
{
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    notmuch_query_t *query;
    notmuch_threads_t *threads;
    notmuch_thread_t *thread;
    notmuch_messages_t *messages;
    char *query_string;
    char *opt;
    const show_format_t *format = &format_text;
    int entire_thread = 0;
    int i;
    int first_toplevel = 1;

    for (i = 0; i < argc && argv[i][0] == '-'; i++) {
	if (strcmp (argv[i], "--") == 0) {
	    i++;
	    break;
	}
	if (STRNCMP_LITERAL (argv[i], "--format=") == 0) {
	    opt = argv[i] + sizeof ("--format=") - 1;
	    if (strcmp (opt, "text") == 0) {
		format = &format_text;
	    } else if (strcmp (opt, "json") == 0) {
		format = &format_json;
		entire_thread = 1;
	    } else {
		fprintf (stderr, "Invalid value for --format: %s\n", opt);
		return 1;
	    }
	} else if (STRNCMP_LITERAL (argv[i], "--entire-thread") == 0) {
	    entire_thread = 1;
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
	fprintf (stderr, "Error: notmuch show requires at least one search term.\n");
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

    fputs (format->message_set_start, stdout);

    for (threads = notmuch_query_search_threads (query);
	 notmuch_threads_valid (threads);
	 notmuch_threads_move_to_next (threads))
    {
	thread = notmuch_threads_get (threads);

	messages = notmuch_thread_get_toplevel_messages (thread);

	if (messages == NULL)
	    INTERNAL_ERROR ("Thread %s has no toplevel messages.\n",
			    notmuch_thread_get_thread_id (thread));

	if (!first_toplevel)
	    fputs (format->message_set_sep, stdout);
	first_toplevel = 0;

	show_messages (ctx, format, messages, 0, entire_thread);

	notmuch_thread_destroy (thread);

    }

    fputs (format->message_set_end, stdout);

    notmuch_query_destroy (query);
    notmuch_database_close (notmuch);

    return 0;
}
