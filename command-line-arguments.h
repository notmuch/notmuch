#ifndef NOTMUCH_OPTS_H
#define NOTMUCH_OPTS_H

#include <stdbool.h>

#include "notmuch.h"

/*
 * Describe one of the possibilities for a keyword option
 * 'value' will be copied to the output variable
 */

typedef struct notmuch_keyword {
    const char *name;
    int value;
} notmuch_keyword_t;

/* Describe one option. */
typedef struct notmuch_opt_desc {
    /* One and only one of opt_* must be set. */
    const struct notmuch_opt_desc *opt_inherit;
    bool *opt_bool;
    int *opt_int;
    int *opt_keyword;
    int *opt_flags;
    const char **opt_string;
    const char **opt_position;

    /* for opt_keyword only: if no matching arguments were found, and
     * keyword_no_arg_value is set, then use keyword_no_arg_value instead. */
    const char *keyword_no_arg_value;

    /* Must be set except for opt_inherit and opt_position. */
    const char *name;

    /* Optional, if non-NULL, set to true if the option is present. */
    bool *present;

    /* Optional, allow empty strings for opt_string. */
    bool allow_empty;

    /* Must be set for opt_keyword and opt_flags. */
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

bool
parse_position_arg (const char *arg,
		    int position_arg_index,
		    const notmuch_opt_desc_t* options);


#endif
