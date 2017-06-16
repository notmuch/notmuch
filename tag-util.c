#include <assert.h>
#include "string-util.h"
#include "tag-util.h"
#include "hex-escape.h"

#define TAG_OP_LIST_INITIAL_SIZE 10

struct _tag_operation_t {
    const char *tag;
    notmuch_bool_t remove;
};

struct _tag_op_list_t {
    tag_operation_t *ops;
    size_t count;
    size_t size;
};

static tag_parse_status_t
line_error (tag_parse_status_t status,
	    const char *line,
	    const char *format, ...)
{
    va_list va_args;

    va_start (va_args, format);

    fprintf (stderr, status < 0 ? "Error: " : "Warning: ");
    vfprintf (stderr, format, va_args);
    fprintf (stderr, " [%s]\n", line);

    va_end (va_args);

    return status;
}

const char *
illegal_tag (const char *tag, notmuch_bool_t remove)
{
    if (*tag == '\0' && ! remove)
	return "empty tag forbidden";

    /* This disallows adding tags starting with "-", in particular the
     * non-removable tag "-" and enables notmuch tag to take long
     * options more easily.
     */

    if (*tag == '-' && ! remove)
	return "tag starting with '-' forbidden";

    return NULL;
}

tag_parse_status_t
parse_tag_line (void *ctx, char *line,
		tag_op_flag_t flags,
		char **query_string,
		tag_op_list_t *tag_ops)
{
    char *tok = line;
    size_t tok_len = 0;
    char *line_for_error;
    tag_parse_status_t ret = TAG_PARSE_SUCCESS;

    chomp_newline (line);

    line_for_error = talloc_strdup (ctx, line);
    if (line_for_error == NULL) {
	fprintf (stderr, "Error: out of memory\n");
	return TAG_PARSE_OUT_OF_MEMORY;
    }

    /* remove leading space */
    while (*tok == ' ' || *tok == '\t')
	tok++;

    /* Skip empty and comment lines. */
    if (*tok == '\0' || *tok == '#') {
	ret = TAG_PARSE_SKIPPED;
	goto DONE;
    }

    tag_op_list_reset (tag_ops);

    /* Parse tags. */
    while ((tok = strtok_len (tok + tok_len, " ", &tok_len)) != NULL) {
	notmuch_bool_t remove;
	char *tag;

	/* Optional explicit end of tags marker. */
	if (tok_len == 2 && strncmp (tok, "--", tok_len) == 0) {
	    tok = strtok_len (tok + tok_len, " ", &tok_len);
	    if (tok == NULL) {
		ret = line_error (TAG_PARSE_INVALID, line_for_error,
				  "no query string after --");
		goto DONE;
	    }
	    break;
	}

	/* Implicit end of tags. */
	if (*tok != '-' && *tok != '+')
	    break;

	/* If tag is terminated by NUL, there's no query string. */
	if (*(tok + tok_len) == '\0') {
	    ret = line_error (TAG_PARSE_INVALID, line_for_error,
			      "no query string");
	    goto DONE;
	}

	/* Terminate, and start next token after terminator. */
	*(tok + tok_len++) = '\0';

	remove = (*tok == '-');
	tag = tok + 1;

	/* Maybe refuse illegal tags. */
	if (! (flags & TAG_FLAG_BE_GENEROUS)) {
	    const char *msg = illegal_tag (tag, remove);
	    if (msg) {
		ret = line_error (TAG_PARSE_INVALID, line_for_error, msg);
		goto DONE;
	    }
	}

	/* Decode tag. */
	if (hex_decode_inplace (tag) != HEX_SUCCESS) {
	    ret = line_error (TAG_PARSE_INVALID, line_for_error,
			      "hex decoding of tag %s failed", tag);
	    goto DONE;
	}

	if (tag_op_list_append (tag_ops, tag, remove)) {
	    ret = line_error (TAG_PARSE_OUT_OF_MEMORY, line_for_error,
			      "aborting");
	    goto DONE;
	}
    }

    if (tok == NULL) {
	/* use a different error message for testing */
	ret = line_error (TAG_PARSE_INVALID, line_for_error,
			  "missing query string");
	goto DONE;
    }

    /* tok now points to the query string */
    *query_string = tok;

  DONE:
    talloc_free (line_for_error);
    return ret;
}

tag_parse_status_t
parse_tag_command_line (void *ctx, int argc, char **argv,
			char **query_str, tag_op_list_t *tag_ops)
{
    int i;

    for (i = 0; i < argc; i++) {
	if (strcmp (argv[i], "--") == 0) {
	    i++;
	    break;
	}

	if (argv[i][0] != '+' && argv[i][0] != '-')
	    break;

	notmuch_bool_t is_remove = argv[i][0] == '-';
	const char *msg;

	msg = illegal_tag (argv[i] + 1, is_remove);
	if (msg) {
	    fprintf (stderr, "Error: %s\n", msg);
	    return TAG_PARSE_INVALID;
	}

	tag_op_list_append (tag_ops, argv[i] + 1, is_remove);
    }

    *query_str = query_string_from_args (ctx, argc - i, &argv[i]);

    if (*query_str == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return TAG_PARSE_OUT_OF_MEMORY;
    }

    return TAG_PARSE_SUCCESS;
}


static inline void
message_error (notmuch_message_t *message,
	       notmuch_status_t status,
	       const char *format, ...)
{
    va_list va_args;

    va_start (va_args, format);

    vfprintf (stderr, format, va_args);
    fprintf (stderr, "Message-ID: %s\n", notmuch_message_get_message_id (message));
    fprintf (stderr, "Status: %s\n", notmuch_status_to_string (status));

    va_end (va_args);
}

static int
makes_changes (notmuch_message_t *message,
	       tag_op_list_t *list,
	       tag_op_flag_t flags)
{
    size_t i;

    notmuch_tags_t *tags;
    notmuch_bool_t changes = FALSE;

    /* First, do we delete an existing tag? */
    for (tags = notmuch_message_get_tags (message);
	 ! changes && notmuch_tags_valid (tags);
	 notmuch_tags_move_to_next (tags)) {
	const char *cur_tag = notmuch_tags_get (tags);
	int last_op =  (flags & TAG_FLAG_REMOVE_ALL) ? -1 : 0;

	/* scan backwards to get last operation */
	i = list->count;
	while (i > 0) {
	    i--;
	    if (strcmp (cur_tag, list->ops[i].tag) == 0) {
		last_op = list->ops[i].remove ? -1 : 1;
		break;
	    }
	}

	changes = (last_op == -1);
    }
    notmuch_tags_destroy (tags);

    if (changes)
	return TRUE;

    /* Now check for adding new tags */
    for (i = 0; i < list->count; i++) {
	notmuch_bool_t exists = FALSE;

	if (list->ops[i].remove)
	    continue;

	for (tags = notmuch_message_get_tags (message);
	     notmuch_tags_valid (tags);
	     notmuch_tags_move_to_next (tags)) {
	    const char *cur_tag = notmuch_tags_get (tags);
	    if (strcmp (cur_tag, list->ops[i].tag) == 0) {
		exists = TRUE;
		break;
	    }
	}
	notmuch_tags_destroy (tags);

	/* the following test is conservative,
	 * in the sense it ignores cases like +foo ... -foo
	 * but this is OK from a correctness point of view
	 */
	if (! exists)
	    return TRUE;
    }
    return FALSE;

}

notmuch_status_t
tag_op_list_apply (notmuch_message_t *message,
		   tag_op_list_t *list,
		   tag_op_flag_t flags)
{
    size_t i;
    notmuch_status_t status = 0;
    tag_operation_t *tag_ops = list->ops;

    if (! (flags & TAG_FLAG_PRE_OPTIMIZED) && ! makes_changes (message, list, flags))
	return NOTMUCH_STATUS_SUCCESS;

    status = notmuch_message_freeze (message);
    if (status) {
	message_error (message, status, "freezing message");
	return status;
    }

    if (flags & TAG_FLAG_REMOVE_ALL) {
	status = notmuch_message_remove_all_tags (message);
	if (status) {
	    message_error (message, status, "removing all tags");
	    return status;
	}
    }

    for (i = 0; i < list->count; i++) {
	if (tag_ops[i].remove) {
	    status = notmuch_message_remove_tag (message, tag_ops[i].tag);
	    if (status) {
		message_error (message, status, "removing tag %s", tag_ops[i].tag);
		return status;
	    }
	} else {
	    status = notmuch_message_add_tag (message, tag_ops[i].tag);
	    if (status) {
		message_error (message, status, "adding tag %s", tag_ops[i].tag);
		return status;
	    }

	}
    }

    status = notmuch_message_thaw (message);
    if (status) {
	message_error (message, status, "thawing message");
	return status;
    }


    if (flags & TAG_FLAG_MAILDIR_SYNC) {
	status = notmuch_message_tags_to_maildir_flags (message);
	if (status) {
	    message_error (message, status, "synching tags to maildir");
	    return status;
	}
    }

    return NOTMUCH_STATUS_SUCCESS;

}


/* Array of tagging operations (add or remove.  Size will be increased
 * as necessary. */

tag_op_list_t *
tag_op_list_create (void *ctx)
{
    tag_op_list_t *list;

    list = talloc (ctx, tag_op_list_t);
    if (list == NULL)
	return NULL;

    list->size = TAG_OP_LIST_INITIAL_SIZE;
    list->count = 0;

    list->ops = talloc_array (list, tag_operation_t, list->size);
    if (list->ops == NULL)
	return NULL;

    return list;
}


int
tag_op_list_append (tag_op_list_t *list,
		    const char *tag,
		    notmuch_bool_t remove)
{
    /* Make room if current array is full.  This should be a fairly
     * rare case, considering the initial array size.
     */

    if (list->count == list->size) {
	list->size *= 2;
	list->ops = talloc_realloc (list, list->ops, tag_operation_t,
				    list->size);
	if (list->ops == NULL) {
	    fprintf (stderr, "Out of memory.\n");
	    return 1;
	}
    }

    /* add the new operation */

    list->ops[list->count].tag = tag;
    list->ops[list->count].remove = remove;
    list->count++;
    return 0;
}

/*
 *   Is the i'th tag operation a remove?
 */

notmuch_bool_t
tag_op_list_isremove (const tag_op_list_t *list, size_t i)
{
    assert (i < list->count);
    return list->ops[i].remove;
}

/*
 * Reset a list to contain no operations
 */

void
tag_op_list_reset (tag_op_list_t *list)
{
    list->count = 0;
}

/*
 * Return the number of operations in a list
 */

size_t
tag_op_list_size (const tag_op_list_t *list)
{
    return list->count;
}

/*
 *   return the i'th tag in the list
 */

const char *
tag_op_list_tag (const tag_op_list_t *list, size_t i)
{
    assert (i < list->count);
    return list->ops[i].tag;
}
