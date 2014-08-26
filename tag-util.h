#ifndef _TAG_UTIL_H
#define _TAG_UTIL_H

#include "notmuch-client.h"

typedef struct _tag_operation_t tag_operation_t;
typedef struct _tag_op_list_t tag_op_list_t;

/* Use powers of 2 */
typedef enum {
    TAG_FLAG_NONE = 0,

    /* Operations are synced to maildir, if possible.
     */
    TAG_FLAG_MAILDIR_SYNC = (1 << 0),

    /* Remove all tags from message before applying list.
     */
    TAG_FLAG_REMOVE_ALL = (1 << 1),

    /* Don't try to avoid database operations. Useful when we
     * know that message passed needs these operations.
     */
    TAG_FLAG_PRE_OPTIMIZED = (1 << 2),

    /* Accept strange tags that might be user error;
     * intended for use by notmuch-restore.
     */
    TAG_FLAG_BE_GENEROUS = (1 << 3)

} tag_op_flag_t;

/* These should obey the convention that fatal errors are negative,
 * skipped lines are positive.
 */
typedef enum {
    TAG_PARSE_OUT_OF_MEMORY = -1,

    /* Line parsed successfuly. */
    TAG_PARSE_SUCCESS = 0,

    /* Line has a syntax error */
    TAG_PARSE_INVALID = 1,

    /* Line was blank or a comment */
    TAG_PARSE_SKIPPED = 2

} tag_parse_status_t;

/* Parse a string of the following format:
 *
 * +<tag>|-<tag> [...] [--] <search-terms>
 *
 * Each line is interpreted similarly to "notmuch tag" command line
 * arguments. The delimiter is one or more spaces ' '. Any characters
 * in <tag> and <search-terms> MAY be hex encoded with %NN where NN is
 * the hexadecimal value of the character. Any ' ' and '%' characters
 * in <tag> and <search-terms> MUST be hex encoded (using %20 and %25,
 * respectively). Any characters that are not part of <tag> or
 * <search-terms> MUST NOT be hex encoded.
 *
 * Leading and trailing space ' ' is ignored. Empty lines and lines
 * beginning with '#' are ignored.
 *
 *
 * Output Parameters:
 *	ops	contains a list of tag operations
 *	query_str the search terms.
 */
tag_parse_status_t
parse_tag_line (void *ctx, char *line,
		tag_op_flag_t flags,
		char **query_str, tag_op_list_t *ops);



/* Parse a command line of the following format:
 *
 * +<tag>|-<tag> [...] [--] <search-terms>
 *
 * Output Parameters:
 *	ops	contains a list of tag operations
 *	query_str the search terms.
 *
 * The ops argument is not cleared.
 */

tag_parse_status_t
parse_tag_command_line (void *ctx, int argc, char **argv,
			char **query_str, tag_op_list_t *ops);

/*
 * Test tags for some forbidden cases.
 *
 * Relax the checks if 'remove' is true to allow removal of previously
 * added forbidden tags.
 *
 * return: NULL if OK,
 *	   explanatory message otherwise.
 */
const char *
illegal_tag (const char *tag, notmuch_bool_t remove);

/*
 * Create an empty list of tag operations
 *
 * ctx is passed to talloc
 */

tag_op_list_t *
tag_op_list_create (void *ctx);

/*
 * Add a tag operation (delete iff remove == TRUE) to a list.
 * The list is expanded as necessary.
 */

int
tag_op_list_append (tag_op_list_t *list,
		    const char *tag,
		    notmuch_bool_t remove);

/*
 * Apply a list of tag operations, in order, to a given message.
 *
 * Flags can be bitwise ORed; see enum above for possibilies.
 */

notmuch_status_t
tag_op_list_apply (notmuch_message_t *message,
		   tag_op_list_t *tag_ops,
		   tag_op_flag_t flags);

/*
 * Return the number of operations in a list
 */

size_t
tag_op_list_size (const tag_op_list_t *list);

/*
 * Reset a list to contain no operations
 */

void
tag_op_list_reset (tag_op_list_t *list);


/*
 *   return the i'th tag in the list
 */

const char *
tag_op_list_tag (const tag_op_list_t *list, size_t i);

/*
 *   Is the i'th tag operation a remove?
 */

notmuch_bool_t
tag_op_list_isremove (const tag_op_list_t *list, size_t i);

#endif
