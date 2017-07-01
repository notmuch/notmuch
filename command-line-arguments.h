#ifndef NOTMUCH_OPTS_H
#define NOTMUCH_OPTS_H

#include "notmuch.h"

enum notmuch_opt_type {
    NOTMUCH_OPT_END = 0,
    NOTMUCH_OPT_INHERIT,	/* another options table */
    NOTMUCH_OPT_BOOLEAN,	/* --verbose              */
    NOTMUCH_OPT_INT,		/* --frob=8               */
    NOTMUCH_OPT_KEYWORD,	/* --format=raw|json|text */
    NOTMUCH_OPT_KEYWORD_FLAGS,	/* the above with values OR'd together */
    NOTMUCH_OPT_STRING,		/* --file=/tmp/gnarf.txt  */
    NOTMUCH_OPT_POSITION	/* notmuch dump pos_arg   */
};

/*
 * Describe one of the possibilities for a keyword option
 * 'value' will be copied to the output variable
 */

typedef struct notmuch_keyword {
    const char *name;
    int value;
} notmuch_keyword_t;

/*
 * Describe one option.
 *
 * First two parameters are mandatory.
 *
 * name is mandatory _except_ for positional arguments.
 *
 * arg_id is currently unused, but could define short arguments.
 *
 * keywords is a (possibly NULL) pointer to an array of keywords
 */
typedef struct notmuch_opt_desc {
    enum notmuch_opt_type opt_type;
    void *output_var;
    const char *name;
    int  arg_id;
    const struct notmuch_keyword *keywords;
} notmuch_opt_desc_t;


/*
  This is the main entry point for command line argument parsing.

  Parse command line arguments according to structure options,
  starting at position opt_index.

  All output of parsed values is via pointers in options.

  Parsing stops at -- (consumed) or at the (k+1)st argument
  not starting with -- (a "positional argument") if options contains
  k positional argument descriptors.

  Returns the index of first non-parsed argument, or -1 in case of error.

*/
int
parse_arguments (int argc, char **argv, const notmuch_opt_desc_t *options, int opt_index);

/*
 * If the argument parsing loop provided by parse_arguments is not
 * flexible enough, then the user might be interested in the following
 * routines, but note that the API to parse_option might have to
 * change. See command-line-arguments.c for descriptions of these
 * functions.
 */

int
parse_option (int argc, char **argv, const notmuch_opt_desc_t* options, int opt_index);

notmuch_bool_t
parse_position_arg (const char *arg,
		    int position_arg_index,
		    const notmuch_opt_desc_t* options);


#endif
