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
#include "dump-restore-private.h"
#include "tag-util.h"
#include "string-util.h"

static regex_t regex;

/* Non-zero return indicates an error in retrieving the message,
 * or in applying the tags.
 */
static int
tag_message (unused (void *ctx),
	     notmuch_database_t *notmuch,
	     const char *message_id,
	     tag_op_list_t *tag_ops,
	     tag_op_flag_t flags)
{
    notmuch_status_t status;
    notmuch_message_t *message = NULL;
    int ret = 0;

    status = notmuch_database_find_message (notmuch, message_id, &message);
    if (status || message == NULL) {
	fprintf (stderr, "Warning: cannot apply tags to %smessage: %s\n",
		 message ? "" : "missing ", message_id);
	if (status)
	    fprintf (stderr, "%s\n", notmuch_status_to_string (status));
	return 1;
    }

    /* In order to detect missing messages, this check/optimization is
     * intentionally done *after* first finding the message. */
    if ((flags & TAG_FLAG_REMOVE_ALL) || tag_op_list_size (tag_ops))
	ret = tag_op_list_apply (message, tag_ops, flags);

    notmuch_message_destroy (message);

    return ret;
}

/* Sup dump output is one line per message. We match a sequence of
 * non-space characters for the message-id, then one or more
 * spaces, then a list of space-separated tags as a sequence of
 * characters within literal '(' and ')'. */

static int
parse_sup_line (void *ctx, char *line,
		char **query_str, tag_op_list_t *tag_ops)
{

    regmatch_t match[3];
    char *file_tags;
    int rerr;

    tag_op_list_reset (tag_ops);

    chomp_newline (line);

    /* Silently ignore blank lines */
    if (line[0] == '\0') {
	return 1;
    }

    rerr = xregexec (&regex, line, 3, match, 0);
    if (rerr == REG_NOMATCH) {
	fprintf (stderr, "Warning: Ignoring invalid sup format line: %s\n",
		 line);
	return 1;
    }

    *query_str = talloc_strndup (ctx, line + match[1].rm_so,
				 match[1].rm_eo - match[1].rm_so);
    file_tags = talloc_strndup (ctx, line + match[2].rm_so,
				match[2].rm_eo - match[2].rm_so);

    char *tok = file_tags;
    size_t tok_len = 0;

    tag_op_list_reset (tag_ops);

    while ((tok = strtok_len (tok + tok_len, " ", &tok_len)) != NULL) {

	if (*(tok + tok_len) != '\0') {
	    *(tok + tok_len) = '\0';
	    tok_len++;
	}

	if (tag_op_list_append (tag_ops, tok, FALSE))
	    return -1;
    }

    return 0;

}

int
notmuch_restore_command (unused (void *ctx), int argc, char *argv[])
{
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    notmuch_bool_t accumulate = FALSE;
    tag_op_flag_t flags = 0;
    tag_op_list_t *tag_ops;

    char *input_file_name = NULL;
    FILE *input = stdin;
    char *line = NULL;
    void *line_ctx = NULL;
    size_t line_size;
    ssize_t line_len;

    int ret = 0;
    int opt_index;
    int input_format = DUMP_FORMAT_AUTO;

    config = notmuch_config_open (ctx, NULL, NULL);
    if (config == NULL)
	return 1;

    if (notmuch_database_open (notmuch_config_get_database_path (config),
			       NOTMUCH_DATABASE_MODE_READ_WRITE, &notmuch))
	return 1;

    if (notmuch_config_get_maildir_synchronize_flags (config))
	flags |= TAG_FLAG_MAILDIR_SYNC;

    notmuch_opt_desc_t options[] = {
	{ NOTMUCH_OPT_KEYWORD, &input_format, "format", 'f',
	  (notmuch_keyword_t []){ { "auto", DUMP_FORMAT_AUTO },
				  { "batch-tag", DUMP_FORMAT_BATCH_TAG },
				  { "sup", DUMP_FORMAT_SUP },
				  { 0, 0 } } },
	{ NOTMUCH_OPT_STRING, &input_file_name, "input", 'i', 0 },
	{ NOTMUCH_OPT_BOOLEAN,  &accumulate, "accumulate", 'a', 0 },
	{ 0, 0, 0, 0, 0 }
    };

    opt_index = parse_arguments (argc, argv, options, 1);

    if (opt_index < 0) {
	/* diagnostics already printed */
	return 1;
    }

    if (! accumulate)
	flags |= TAG_FLAG_REMOVE_ALL;

    if (input_file_name) {
	input = fopen (input_file_name, "r");
	if (input == NULL) {
	    fprintf (stderr, "Error opening %s for reading: %s\n",
		     input_file_name, strerror (errno));
	    return 1;
	}
    }

    if (opt_index < argc) {
	fprintf (stderr,
		 "Unused positional parameter: %s\n",
		 argv[opt_index]);
	return 1;
    }
    char *p;

    line_len = getline (&line, &line_size, input);
    if (line_len == 0)
	return 0;

    tag_ops = tag_op_list_create (ctx);
    if (tag_ops == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }

    for (p = line; (input_format == DUMP_FORMAT_AUTO) && *p; p++) {
	if (*p == '(')
	    input_format = DUMP_FORMAT_SUP;
    }

    if (input_format == DUMP_FORMAT_AUTO)
	input_format = DUMP_FORMAT_BATCH_TAG;

    if (input_format == DUMP_FORMAT_SUP)
	if ( xregcomp (&regex,
		       "^([^ ]+) \\(([^)]*)\\)$",
		       REG_EXTENDED) )
	    INTERNAL_ERROR ("compile time constant regex failed.");

    do {
	char *query_string;

	if (line_ctx != NULL)
	    talloc_free (line_ctx);

	line_ctx = talloc_new (ctx);
	if (input_format == DUMP_FORMAT_SUP) {
	    ret = parse_sup_line (line_ctx, line, &query_string, tag_ops);
	} else {
	    ret = parse_tag_line (line_ctx, line, TAG_FLAG_BE_GENEROUS,
				  &query_string, tag_ops);

	    if (ret == 0) {
		if (strncmp ("id:", query_string, 3) != 0) {
		    fprintf (stderr, "Warning: unsupported query: %s\n", query_string);
		    continue;
		}
		/* delete id: from front of string; tag_message
		 * expects a raw message-id.
		 *
		 * XXX: Note that query string id:foo and bar will be
		 * interpreted as a message id "foo and bar". This
		 * should eventually be fixed to give a better error
		 * message.
		 */
		query_string = query_string + 3;
	    }
	}

	if (ret > 0)
	    continue;

	if (ret < 0)
	    break;

	ret = tag_message (line_ctx, notmuch, query_string,
			   tag_ops, flags);
	if (ret)
	    break;

    }  while ((line_len = getline (&line, &line_size, input)) != -1);

    if (line_ctx != NULL)
	talloc_free (line_ctx);

    if (input_format == DUMP_FORMAT_SUP)
	regfree (&regex);

    if (line)
	free (line);

    notmuch_database_destroy (notmuch);

    if (input != stdin)
	fclose (input);

    return ret;
}
