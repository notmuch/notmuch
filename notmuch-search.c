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

typedef struct search_format {
    const char *results_start;
    const char *thread_start;
    void (*thread) (const void *ctx,
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
    const char *thread_sep;
    const char *thread_end;
    const char *results_end;
} search_format_t;

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
	    format_thread_text,
	    " (",
		"%s", " ",
	    ")", "",
	"\n",
    "",
};

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
	    format_thread_json,
	    "\"tags\": [",
		"\"%s\"", ", ",
	    "]", ",\n",
	"}",
    "]\n",
};

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
format_thread_json (const void *ctx,
		    const char *thread_id,
		    const time_t date,
		    const int matched,
		    const int total,
		    const char *authors,
		    const char *subject)
{
    struct tm *tm;
    char timestamp[40];
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

static void
do_search_threads (const void *ctx,
		   const search_format_t *format,
		   notmuch_query_t *query,
		   notmuch_sort_t sort)
{
    notmuch_thread_t *thread;
    notmuch_threads_t *threads;
    notmuch_tags_t *tags;
    time_t date;
    int first_thread = 1;

    fputs (format->results_start, stdout);

    for (threads = notmuch_query_search_threads (query);
	 notmuch_threads_valid (threads);
	 notmuch_threads_move_to_next (threads))
    {
	int first_tag = 1;

	if (! first_thread)
	    fputs (format->thread_sep, stdout);

	thread = notmuch_threads_get (threads);

	if (sort == NOTMUCH_SORT_OLDEST_FIRST)
	    date = notmuch_thread_get_oldest_date (thread);
	else
	    date = notmuch_thread_get_newest_date (thread);

	fputs (format->thread_start, stdout);

	format->thread (ctx,
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
	fputs (format->thread_end, stdout);

	first_thread = 0;

	notmuch_thread_destroy (thread);
    }

    fputs (format->results_end, stdout);
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
    int i;

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

    query_str = query_string_from_args (ctx, argc, argv);
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

    do_search_threads (ctx, format, query, sort);

    notmuch_query_destroy (query);
    notmuch_database_close (notmuch);

    return 0;
}
