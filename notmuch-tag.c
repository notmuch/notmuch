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

static volatile sig_atomic_t interrupted;

static void
handle_sigint (unused (int sig))
{
    static char msg[] = "Stopping...         \n";

    /* This write is "opportunistic", so it's okay to ignore the
     * result.  It is not required for correctness, and if it does
     * fail or produce a short write, we want to get out of the signal
     * handler as quickly as possible, not retry it. */
    IGNORE_RESULT (write (2, msg, sizeof (msg) - 1));
    interrupted = 1;
}

static char *
_escape_tag (char *buf, const char *tag)
{
    const char *in = tag;
    char *out = buf;

    /* Boolean terms surrounded by double quotes can contain any
     * character.  Double quotes are quoted by doubling them. */
    *out++ = '"';
    while (*in) {
	if (*in == '"')
	    *out++ = '"';
	*out++ = *in++;
    }
    *out++ = '"';
    *out = 0;
    return buf;
}

typedef struct {
    const char *tag;
    notmuch_bool_t remove;
} tag_operation_t;

static char *
_optimize_tag_query (void *ctx, const char *orig_query_string,
		     const tag_operation_t *tag_ops)
{
    /* This is subtler than it looks.  Xapian ignores the '-' operator
     * at the beginning both queries and parenthesized groups and,
     * furthermore, the presence of a '-' operator at the beginning of
     * a group can inhibit parsing of the previous operator.  Hence,
     * the user-provided query MUST appear first, but it is safe to
     * parenthesize and the exclusion part of the query must not use
     * the '-' operator (though the NOT operator is fine). */

    char *escaped, *query_string;
    const char *join = "";
    int i;
    unsigned int max_tag_len = 0;

    /* Don't optimize if there are no tag changes. */
    if (tag_ops[0].tag == NULL)
	return talloc_strdup (ctx, orig_query_string);

    /* Allocate a buffer for escaping tags.  This is large enough to
     * hold a fully escaped tag with every character doubled plus
     * enclosing quotes and a NUL. */
    for (i = 0; tag_ops[i].tag; i++)
	if (strlen (tag_ops[i].tag) > max_tag_len)
	    max_tag_len = strlen (tag_ops[i].tag);
    escaped = talloc_array (ctx, char, max_tag_len * 2 + 3);
    if (! escaped)
	return NULL;

    /* Build the new query string */
    if (strcmp (orig_query_string, "*") == 0)
	query_string = talloc_strdup (ctx, "(");
    else
	query_string = talloc_asprintf (ctx, "( %s ) and (", orig_query_string);

    for (i = 0; tag_ops[i].tag && query_string; i++) {
	query_string = talloc_asprintf_append_buffer (
	    query_string, "%s%stag:%s", join,
	    tag_ops[i].remove ? "" : "not ",
	    _escape_tag (escaped, tag_ops[i].tag));
	join = " or ";
    }

    if (query_string)
	query_string = talloc_strdup_append_buffer (query_string, ")");

    talloc_free (escaped);
    return query_string;
}

/* Tag messages matching 'query_string' according to 'tag_ops', which
 * must be an array of tagging operations terminated with an empty
 * element. */
static int
tag_query (void *ctx, notmuch_database_t *notmuch, const char *query_string,
	   tag_operation_t *tag_ops, notmuch_bool_t synchronize_flags)
{
    notmuch_query_t *query;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    int i;

    /* Optimize the query so it excludes messages that already have
     * the specified set of tags. */
    query_string = _optimize_tag_query (ctx, query_string, tag_ops);
    if (query_string == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    query = notmuch_query_create (notmuch, query_string);
    if (query == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    /* tagging is not interested in any special sort order */
    notmuch_query_set_sort (query, NOTMUCH_SORT_UNSORTED);

    for (messages = notmuch_query_search_messages (query);
	 notmuch_messages_valid (messages) && ! interrupted;
	 notmuch_messages_move_to_next (messages)) {
	message = notmuch_messages_get (messages);

	notmuch_message_freeze (message);

	for (i = 0; tag_ops[i].tag; i++) {
	    if (tag_ops[i].remove)
		notmuch_message_remove_tag (message, tag_ops[i].tag);
	    else
		notmuch_message_add_tag (message, tag_ops[i].tag);
	}

	notmuch_message_thaw (message);

	if (synchronize_flags)
	    notmuch_message_tags_to_maildir_flags (message);

	notmuch_message_destroy (message);
    }

    notmuch_query_destroy (query);

    return interrupted;
}

int
notmuch_tag_command (void *ctx, int argc, char *argv[])
{
    tag_operation_t *tag_ops;
    int tag_ops_count = 0;
    char *query_string;
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    struct sigaction action;
    notmuch_bool_t synchronize_flags;
    int i;
    int ret;

    /* Setup our handler for SIGINT */
    memset (&action, 0, sizeof (struct sigaction));
    action.sa_handler = handle_sigint;
    sigemptyset (&action.sa_mask);
    action.sa_flags = SA_RESTART;
    sigaction (SIGINT, &action, NULL);

    argc--; argv++; /* skip subcommand argument */

    /* Array of tagging operations (add or remove), terminated with an
     * empty element. */
    tag_ops = talloc_array (ctx, tag_operation_t, argc + 1);
    if (tag_ops == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    for (i = 0; i < argc; i++) {
	if (strcmp (argv[i], "--") == 0) {
	    i++;
	    break;
	}
	if (argv[i][0] == '+' || argv[i][0] == '-') {
	    if (argv[i][0] == '+' && argv[i][1] == '\0') {
		fprintf (stderr, "Error: tag names cannot be empty.\n");
		return 1;
	    }
	    if (argv[i][0] == '+' && argv[i][1] == '-') {
		/* This disallows adding the non-removable tag "-" and
		 * enables notmuch tag to take long options in the
		 * future. */
		fprintf (stderr, "Error: tag names must not start with '-'.\n");
		return 1;
	    }
	    tag_ops[tag_ops_count].tag = argv[i] + 1;
	    tag_ops[tag_ops_count].remove = (argv[i][0] == '-');
	    tag_ops_count++;
	} else {
	    break;
	}
    }

    tag_ops[tag_ops_count].tag = NULL;

    if (tag_ops_count == 0) {
	fprintf (stderr, "Error: 'notmuch tag' requires at least one tag to add or remove.\n");
	return 1;
    }

    query_string = query_string_from_args (ctx, argc - i, &argv[i]);

    if (*query_string == '\0') {
	fprintf (stderr, "Error: notmuch tag requires at least one search term.\n");
	return 1;
    }

    config = notmuch_config_open (ctx, NULL, NULL);
    if (config == NULL)
	return 1;

    if (notmuch_database_open (notmuch_config_get_database_path (config),
			       NOTMUCH_DATABASE_MODE_READ_WRITE, &notmuch))
	return 1;

    synchronize_flags = notmuch_config_get_maildir_synchronize_flags (config);

    ret = tag_query (ctx, notmuch, query_string, tag_ops, synchronize_flags);

    notmuch_database_destroy (notmuch);

    return ret;
}
