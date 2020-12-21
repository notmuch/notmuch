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
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-client.h"
#include "hex-escape.h"
#include "tag-util.h"
#include "string-util.h"
#include "zlib-extra.h"

static int
process_config_line (notmuch_database_t *notmuch, const char *line)
{
    const char *key_p, *val_p;
    char *key, *val;
    size_t key_len, val_len;
    const char *delim = " \t\n";
    int ret = EXIT_FAILURE;

    void *local = talloc_new (NULL);

    key_p = strtok_len_c (line, delim, &key_len);
    val_p = strtok_len_c (key_p + key_len, delim, &val_len);

    key = talloc_strndup (local, key_p, key_len);
    val = talloc_strndup (local, val_p, val_len);
    if (hex_decode_inplace (key) != HEX_SUCCESS ||
	hex_decode_inplace (val) != HEX_SUCCESS ) {
	fprintf (stderr, "hex decoding failure on line %s\n", line);
	goto DONE;
    }

    if (print_status_database ("notmuch restore", notmuch,
			       notmuch_database_set_config (notmuch, key, val)))
	goto DONE;

    ret = EXIT_SUCCESS;

  DONE:
    talloc_free (local);
    return ret;
}

static int
process_properties_line (notmuch_database_t *notmuch, const char *line)
{
    const char *id_p, *tok;
    size_t id_len = 0, tok_len = 0;
    char *id;

    notmuch_message_t *message = NULL;
    const char *delim = " \t\n";
    int ret = EXIT_FAILURE;

    void *local = talloc_new (NULL);

    id_p = strtok_len_c (line, delim, &id_len);
    id = talloc_strndup (local, id_p, id_len);
    if (hex_decode_inplace (id) != HEX_SUCCESS) {
	fprintf (stderr, "hex decoding failure on line %s\n", line);
	goto DONE;
    }

    if (print_status_database ("notmuch restore", notmuch,
			       notmuch_database_find_message (notmuch, id, &message)))
	goto DONE;

    if (print_status_database ("notmuch restore", notmuch,
			       notmuch_message_remove_all_properties (message, NULL)))
	goto DONE;

    tok = id_p + id_len;

    while ((tok = strtok_len_c (tok + tok_len, delim, &tok_len)) != NULL) {
	char *key, *value;
	size_t off = strcspn (tok, "=");
	if (off > tok_len) {
	    fprintf (stderr, "unparsable token %s\n", tok);
	    goto DONE;
	}

	key = talloc_strndup (local, tok, off);
	value = talloc_strndup (local, tok + off + 1, tok_len - off - 1);

	if (hex_decode_inplace (key) != HEX_SUCCESS) {
	    fprintf (stderr, "hex decoding failure on key %s\n", key);
	    goto DONE;
	}

	if (hex_decode_inplace (value) != HEX_SUCCESS) {
	    fprintf (stderr, "hex decoding failure on value %s\n", value);
	    goto DONE;
	}

	if (print_status_database ("notmuch restore", notmuch,
				   notmuch_message_add_property (message, key, value)))
	    goto DONE;

    }

    ret = EXIT_SUCCESS;

  DONE:
    talloc_free (local);
    return ret;
}


static regex_t regex;

/* Non-zero return indicates an error in retrieving the message,
 * or in applying the tags.  Missing messages are reported, but not
 * considered errors.
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
    if (status) {
	fprintf (stderr, "Error applying tags to message %s: %s\n",
		 message_id, notmuch_status_to_string (status));
	return 1;
    }
    if (message == NULL) {
	fprintf (stderr, "Warning: cannot apply tags to missing message: %s\n",
		 message_id);
	/* We consider this a non-fatal error. */
	return 0;
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

    *query_str = talloc_strndup_debug (ctx, line + match[1].rm_so,
				       match[1].rm_eo - match[1].rm_so);

    file_tags = talloc_strndup_debug (ctx, line + match[2].rm_so,
				      match[2].rm_eo - match[2].rm_so);

    char *tok = file_tags;
    size_t tok_len = 0;

    tag_op_list_reset (tag_ops);

    while ((tok = strtok_len (tok + tok_len, " ", &tok_len)) != NULL) {

	if (*(tok + tok_len) != '\0') {
	    *(tok + tok_len) = '\0';
	    tok_len++;
	}

	if (tag_op_list_append (tag_ops, tok, false))
	    return -1;
    }

    return 0;

}

int
notmuch_restore_command (notmuch_config_t *config, unused(notmuch_database_t *notmuch), int argc, char *argv[])
{
    notmuch_database_t *notmuch;
    bool accumulate = false;
    tag_op_flag_t flags = 0;
    tag_op_list_t *tag_ops;

    const char *input_file_name = NULL;
    const char *name_for_error = NULL;
    gzFile input = NULL;
    char *line = NULL;
    void *line_ctx = NULL;
    ssize_t line_len;

    int ret = 0;
    int opt_index;
    int include = 0;
    int input_format = DUMP_FORMAT_AUTO;
    int errnum;

    if (notmuch_database_open (notmuch_config_get_database_path (config),
			       NOTMUCH_DATABASE_MODE_READ_WRITE, &notmuch))
	return EXIT_FAILURE;

    if (notmuch_config_get_maildir_synchronize_flags (config))
	flags |= TAG_FLAG_MAILDIR_SYNC;

    notmuch_opt_desc_t options[] = {
	{ .opt_keyword = &input_format, .name = "format", .keywords =
	      (notmuch_keyword_t []){ { "auto", DUMP_FORMAT_AUTO },
				      { "batch-tag", DUMP_FORMAT_BATCH_TAG },
				      { "sup", DUMP_FORMAT_SUP },
				      { 0, 0 } } },
	{ .opt_flags = &include, .name = "include", .keywords =
	      (notmuch_keyword_t []){ { "config", DUMP_INCLUDE_CONFIG },
				      { "properties", DUMP_INCLUDE_PROPERTIES },
				      { "tags", DUMP_INCLUDE_TAGS } } },

	{ .opt_string = &input_file_name, .name = "input" },
	{ .opt_bool = &accumulate, .name = "accumulate" },
	{ .opt_inherit = notmuch_shared_options },
	{ }
    };

    opt_index = parse_arguments (argc, argv, options, 1);
    if (opt_index < 0) {
	ret = EXIT_FAILURE;
	goto DONE;
    }

    notmuch_process_shared_options (argv[0]);
    notmuch_exit_if_unmatched_db_uuid (notmuch);

    if (include == 0) {
	include = DUMP_INCLUDE_CONFIG | DUMP_INCLUDE_PROPERTIES | DUMP_INCLUDE_TAGS;
    }

    name_for_error = input_file_name ? input_file_name : "stdin";

    if (! accumulate)
	flags |= TAG_FLAG_REMOVE_ALL;

    errno = 0;
    if (input_file_name)
	input = gzopen (input_file_name, "r");
    else {
	int infd = dup (STDIN_FILENO);
	if (infd < 0) {
	    fprintf (stderr, "Error duping stdin: %s\n",
		     strerror (errno));
	    ret = EXIT_FAILURE;
	    goto DONE;
	}
	input = gzdopen (infd, "r");
	if (! input)
	    close (infd);
    }

    if (input == NULL) {
	fprintf (stderr, "Error opening %s for (gzip) reading: %s\n",
		 name_for_error, strerror (errno));
	ret = EXIT_FAILURE;
	goto DONE;
    }

    if (opt_index < argc) {
	fprintf (stderr, "Unused positional parameter: %s\n", argv[opt_index]);
	ret = EXIT_FAILURE;
	goto DONE;
    }

    tag_ops = tag_op_list_create (config);
    if (tag_ops == NULL) {
	fprintf (stderr, "Out of memory.\n");
	ret = EXIT_FAILURE;
	goto DONE;
    }

    do {
	util_status_t status;

	status = gz_getline (line_ctx, &line, &line_len, input);

	/* empty input file not considered an error */
	if (status == UTIL_EOF) {
	    ret = EXIT_SUCCESS;
	    goto DONE;
	}

	if (status) {
	    fprintf (stderr, "Error reading (gzipped) input: %s\n",
		     gz_error_string (status, input));
	    ret = EXIT_FAILURE;
	    goto DONE;
	}

	if ((include & DUMP_INCLUDE_CONFIG) && line_len >= 2 && line[0] == '#' && line[1] == '@') {
	    ret = process_config_line (notmuch, line + 2);
	    if (ret)
		goto DONE;
	}
	if ((include & DUMP_INCLUDE_PROPERTIES) && line_len >= 2 && line[0] == '#' && line[1] == '=') {
	    ret = process_properties_line (notmuch, line + 2);
	    if (ret)
		goto DONE;
	}

    } while ((line_len == 0) ||
	     (line[0] == '#') ||
             /* the cast is safe because we checked about for line_len < 0 */
	     (strspn (line, " \t\n") == (unsigned) line_len));

    if (! ((include & DUMP_INCLUDE_TAGS) || (include & DUMP_INCLUDE_PROPERTIES))) {
	ret = EXIT_SUCCESS;
	goto DONE;
    }

    char *p;
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
	char *query_string, *prefix, *term;

	if (line_ctx != NULL)
	    talloc_free (line_ctx);

	line_ctx = talloc_new (config);

	if ((include & DUMP_INCLUDE_PROPERTIES) && line_len >= 2 && line[0] == '#' && line[1] == '=') {
	    ret = process_properties_line (notmuch, line + 2);
	    if (ret)
		goto DONE;
	}

	if (input_format == DUMP_FORMAT_SUP) {
	    ret = parse_sup_line (line_ctx, line, &query_string, tag_ops);
	} else {
	    ret = parse_tag_line (line_ctx, line, TAG_FLAG_BE_GENEROUS,
				  &query_string, tag_ops);

	    if (ret == 0) {
		ret = parse_boolean_term (line_ctx, query_string,
					  &prefix, &term);
		if (ret && errno == EINVAL) {
		    fprintf (stderr, "Warning: cannot parse query: %s (skipping)\n", query_string);
		    continue;
		} else if (ret) {
		    /* This is more fatal (e.g., out of memory) */
		    fprintf (stderr, "Error parsing query: %s\n",
			     strerror (errno));
		    ret = 1;
		    break;
		} else if (strcmp ("id", prefix) != 0) {
		    fprintf (stderr, "Warning: not an id query: %s (skipping)\n", query_string);
		    continue;
		}
		query_string = term;
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

    }  while (! (ret = gz_getline (line_ctx, &line, &line_len, input)));


    /* EOF is normal loop termination condition, UTIL_SUCCESS is
     * impossible here */
    if (ret == UTIL_EOF) {
	ret = EXIT_SUCCESS;
    } else {
	fprintf (stderr, "Error reading (gzipped) input: %s\n",
		 gz_error_string (ret, input));
	ret = EXIT_FAILURE;
    }

    /* currently this should not be after DONE: since we don't
     * know if the xregcomp was reached
     */

    if (input_format == DUMP_FORMAT_SUP)
	regfree (&regex);

  DONE:
    if (line_ctx != NULL)
	talloc_free (line_ctx);

    if (notmuch)
	notmuch_database_destroy (notmuch);

    if (input) {
	errnum = gzclose_r (input);
	if (errnum) {
	    fprintf (stderr, "Error closing %s: %d\n",
		     name_for_error, errnum);
	    ret = EXIT_FAILURE;
	}
    }

    return ret ? EXIT_FAILURE : EXIT_SUCCESS;
}
