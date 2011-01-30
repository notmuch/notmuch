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

typedef enum {
    OUTPUT_SUMMARY,
    OUTPUT_THREADS,
    OUTPUT_MESSAGES,
    OUTPUT_FILES,
    OUTPUT_TAGS
} output_t;

typedef struct search_format {
    const char *results_start;
    const char *item_start;
    void (*item_id) (const void *ctx,
		     const char *item_type,
		     const char *item_id);
    void (*thread_summary) (const void *ctx,
			    const char *thread_id,
			    const time_t date,
			    const int matched,
			    const int total,
			    const char *authors,
			    const char *subject);
    const char *tag_start;
    const char *tag;
    const char *tag_sep;
    const char *tag_end;
    const char *item_sep;
    const char *item_end;
    const char *results_end;
} search_format_t;

static void
format_item_id_text (const void *ctx,
		     const char *item_type,
		     const char *item_id);

static void
format_thread_text (const void *ctx,
		    const char *thread_id,
		    const time_t date,
		    const int matched,
		    const int total,
		    const char *authors,
		    const char *subject);
static const search_format_t format_text = {
    "",
	"",
	    format_item_id_text,
	    format_thread_text,
	    " (",
		"%s", " ",
	    ")", "\n",
	"",
    "",
};

static void
format_item_id_json (const void *ctx,
		     const char *item_type,
		     const char *item_id);

static void
format_thread_json (const void *ctx,
		    const char *thread_id,
		    const time_t date,
		    const int matched,
		    const int total,
		    const char *authors,
		    const char *subject);
static const search_format_t format_json = {
    "[",
	"{",
	    format_item_id_json,
	    format_thread_json,
	    "\"tags\": [",
		"\"%s\"", ", ",
	    "]", ",\n",
	"}",
    "]\n",
};

static void
format_item_id_text (unused (const void *ctx),
		     const char *item_type,
		     const char *item_id)
{
    printf ("%s%s", item_type, item_id);
}

static void
format_thread_text (const void *ctx,
		    const char *thread_id,
		    const time_t date,
		    const int matched,
		    const int total,
		    const char *authors,
		    const char *subject)
{
    printf ("thread:%s %12s [%d/%d] %s; %s",
	    thread_id,
	    notmuch_time_relative_date (ctx, date),
	    matched,
	    total,
	    authors,
	    subject);
}

static void
format_item_id_json (const void *ctx,
		     unused (const char *item_type),
		     const char *item_id)
{
    void *ctx_quote = talloc_new (ctx);

    printf ("%s", json_quote_str (ctx_quote, item_id));

    talloc_free (ctx_quote);
    
}

static void
format_thread_json (const void *ctx,
		    const char *thread_id,
		    const time_t date,
		    const int matched,
		    const int total,
		    const char *authors,
		    const char *subject)
{
    void *ctx_quote = talloc_new (ctx);

    printf ("\"thread\": %s,\n"
	    "\"timestamp\": %ld,\n"
	    "\"matched\": %d,\n"
	    "\"total\": %d,\n"
	    "\"authors\": %s,\n"
	    "\"subject\": %s,\n",
	    json_quote_str (ctx_quote, thread_id),
	    date,
	    matched,
	    total,
	    json_quote_str (ctx_quote, authors),
	    json_quote_str (ctx_quote, subject));

    talloc_free (ctx_quote);
}

static int
do_search_threads (const search_format_t *format,
		   notmuch_query_t *query,
		   notmuch_sort_t sort,
		   output_t output)
{
    notmuch_thread_t *thread;
    notmuch_threads_t *threads;
    notmuch_tags_t *tags;
    time_t date;
    int first_thread = 1;

    threads = notmuch_query_search_threads (query);
    if (threads == NULL)
	return 1;

    fputs (format->results_start, stdout);

    for (;
	 notmuch_threads_valid (threads);
	 notmuch_threads_move_to_next (threads))
    {
	int first_tag = 1;

	if (! first_thread)
	    fputs (format->item_sep, stdout);

	thread = notmuch_threads_get (threads);

	if (output == OUTPUT_THREADS) {
	    format->item_id (thread, "thread:",
			     notmuch_thread_get_thread_id (thread));
	} else { /* output == OUTPUT_SUMMARY */
	    fputs (format->item_start, stdout);

	    if (sort == NOTMUCH_SORT_OLDEST_FIRST)
		date = notmuch_thread_get_oldest_date (thread);
	    else
		date = notmuch_thread_get_newest_date (thread);

	    format->thread_summary (thread,
				    notmuch_thread_get_thread_id (thread),
				    date,
				    notmuch_thread_get_matched_messages (thread),
				    notmuch_thread_get_total_messages (thread),
				    notmuch_thread_get_authors (thread),
				    notmuch_thread_get_subject (thread));

	    fputs (format->tag_start, stdout);

	    for (tags = notmuch_thread_get_tags (thread);
		 notmuch_tags_valid (tags);
		 notmuch_tags_move_to_next (tags))
	    {
		if (! first_tag)
		    fputs (format->tag_sep, stdout);
		printf (format->tag, notmuch_tags_get (tags));
		first_tag = 0;
	    }

	    fputs (format->tag_end, stdout);

	    fputs (format->item_end, stdout);
	}

	first_thread = 0;

	notmuch_thread_destroy (thread);
    }

    fputs (format->results_end, stdout);

    return 0;
}

static int
do_search_messages (const search_format_t *format,
		    notmuch_query_t *query,
		    output_t output)
{
    notmuch_message_t *message;
    notmuch_messages_t *messages;
    int first_message = 1;

    messages = notmuch_query_search_messages (query);
    if (messages == NULL)
	return 1;

    fputs (format->results_start, stdout);

    for (;
	 notmuch_messages_valid (messages);
	 notmuch_messages_move_to_next (messages))
    {
	message = notmuch_messages_get (messages);

	if (! first_message)
	    fputs (format->item_sep, stdout);

	if (output == OUTPUT_FILES) {
	    format->item_id (message, "",
			     notmuch_message_get_filename (message));
	} else { /* output == OUTPUT_MESSAGES */
	    format->item_id (message, "id:",
			     notmuch_message_get_message_id (message));
	}

	first_message = 0;

	notmuch_message_destroy (message);
    }

    notmuch_messages_destroy (messages);

    fputs (format->results_end, stdout);

    return 0;
}

static int
do_search_tags (notmuch_database_t *notmuch,
		const search_format_t *format,
		notmuch_query_t *query)
{
    notmuch_messages_t *messages = NULL;
    notmuch_tags_t *tags;
    const char *tag;
    int first_tag = 1;

    /* Special-case query of "*" for better performance. */
    if (strcmp (notmuch_query_get_query_string (query), "*") == 0) {
	tags = notmuch_database_get_all_tags (notmuch);
    } else {
	messages = notmuch_query_search_messages (query);
	if (messages == NULL)
	    return 1;

	tags = notmuch_messages_collect_tags (messages);
    }
    if (tags == NULL)
	return 1;

    fputs (format->results_start, stdout);

    for (;
	 notmuch_tags_valid (tags);
	 notmuch_tags_move_to_next (tags))
    {
	tag = notmuch_tags_get (tags);

	if (! first_tag)
	    fputs (format->item_sep, stdout);

	format->item_id (tags, "", tag);

	first_tag = 0;
    }

    notmuch_tags_destroy (tags);

    if (messages)
	notmuch_messages_destroy (messages);

    fputs (format->results_end, stdout);

    return 0;
}

int
notmuch_search_command (void *ctx, int argc, char *argv[])
{
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    notmuch_query_t *query;
    char *query_str;
    char *opt;
    notmuch_sort_t sort = NOTMUCH_SORT_NEWEST_FIRST;
    const search_format_t *format = &format_text;
    int i, ret;
    output_t output = OUTPUT_SUMMARY;

    for (i = 0; i < argc && argv[i][0] == '-'; i++) {
	if (strcmp (argv[i], "--") == 0) {
	    i++;
	    break;
	}
        if (STRNCMP_LITERAL (argv[i], "--sort=") == 0) {
	    opt = argv[i] + sizeof ("--sort=") - 1;
	    if (strcmp (opt, "oldest-first") == 0) {
		sort = NOTMUCH_SORT_OLDEST_FIRST;
	    } else if (strcmp (opt, "newest-first") == 0) {
		sort = NOTMUCH_SORT_NEWEST_FIRST;
	    } else {
		fprintf (stderr, "Invalid value for --sort: %s\n", opt);
		return 1;
	    }
	} else if (STRNCMP_LITERAL (argv[i], "--format=") == 0) {
	    opt = argv[i] + sizeof ("--format=") - 1;
	    if (strcmp (opt, "text") == 0) {
		format = &format_text;
	    } else if (strcmp (opt, "json") == 0) {
		format = &format_json;
	    } else {
		fprintf (stderr, "Invalid value for --format: %s\n", opt);
		return 1;
	    }
	} else if (STRNCMP_LITERAL (argv[i], "--output=") == 0) {
	    opt = argv[i] + sizeof ("--output=") - 1;
	    if (strcmp (opt, "summary") == 0) {
		output = OUTPUT_SUMMARY;
	    } else if (strcmp (opt, "threads") == 0) {
		output = OUTPUT_THREADS;
	    } else if (strcmp (opt, "messages") == 0) {
		output = OUTPUT_MESSAGES;
	    } else if (strcmp (opt, "files") == 0) {
		output = OUTPUT_FILES;
	    } else if (strcmp (opt, "tags") == 0) {
		output = OUTPUT_TAGS;
	    } else {
		fprintf (stderr, "Invalid value for --output: %s\n", opt);
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

    notmuch = notmuch_database_open (notmuch_config_get_database_path (config),
				     NOTMUCH_DATABASE_MODE_READ_ONLY);
    if (notmuch == NULL)
	return 1;

    query_str = query_string_from_args (notmuch, argc, argv);
    if (query_str == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }
    if (*query_str == '\0') {
	fprintf (stderr, "Error: notmuch search requires at least one search term.\n");
	return 1;
    }

    query = notmuch_query_create (notmuch, query_str);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	return 1;
    }

    notmuch_query_set_sort (query, sort);

    switch (output) {
    default:
    case OUTPUT_SUMMARY:
    case OUTPUT_THREADS:
	ret = do_search_threads (format, query, sort, output);
	break;
    case OUTPUT_MESSAGES:
    case OUTPUT_FILES:
	ret = do_search_messages (format, query, output);
	break;
    case OUTPUT_TAGS:
	ret = do_search_tags (notmuch, format, query);
	break;
    }

    notmuch_query_destroy (query);
    notmuch_database_close (notmuch);

    return ret;
}
