#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "error_util.h"
#include "command-line-arguments.h"

/*
  Search the array of keywords for a given argument, assigning the
  output variable to the corresponding value.  Return FALSE if nothing
  matches.
*/

static notmuch_bool_t
_process_keyword_arg (const notmuch_opt_desc_t *arg_desc, const char *arg_str) {

    notmuch_keyword_t *keywords = arg_desc->keywords;

    while (keywords->name) {
	if (strcmp (arg_str, keywords->name) == 0) {
	    if (arg_desc->output_var) {
		*((int *)arg_desc->output_var) = keywords->value;
	    }
	    return TRUE;
	}
	keywords++;
    }
    fprintf (stderr, "unknown keyword: %s\n", arg_str);
    return FALSE;
}

/*
   Search for the {pos_arg_index}th position argument, return FALSE if
   that does not exist.
*/

notmuch_bool_t
parse_position_arg (const char *arg_str, int pos_arg_index,
		    const notmuch_opt_desc_t *arg_desc) {

    int pos_arg_counter = 0;
    while (arg_desc->opt_type != NOTMUCH_OPT_END){
	if (arg_desc->opt_type == NOTMUCH_OPT_POSITION) {
	    if (pos_arg_counter == pos_arg_index) {
		if (arg_desc->output_var) {
		    *((const char **)arg_desc->output_var) = arg_str;
		}
		return TRUE;
	    }
	    pos_arg_counter++;
	}
	arg_desc++;
    }
    return FALSE;
}

/*
 * Search for a non-positional (i.e. starting with --) argument matching arg,
 * parse a possible value, and assign to *output_var
 */

notmuch_bool_t
parse_option (const char *arg,
	      const notmuch_opt_desc_t *options) {

    assert(arg);
    assert(options);

    arg += 2;

    const notmuch_opt_desc_t *try = options;
    while (try->opt_type != NOTMUCH_OPT_END) {
	if (try->name && strncmp (arg, try->name, strlen (try->name)) == 0) {
	    char next = arg[strlen (try->name)];
	    const char *value= arg+strlen(try->name)+1;

	    char *endptr;

	    /* Everything but boolean arguments (switches) needs a
	     * delimiter, and a non-zero length value
	     */

	    if (try->opt_type != NOTMUCH_OPT_BOOLEAN) {
		if (next != '=' && next != ':') return FALSE;
		if (value[0] == 0) return FALSE;
	    } else {
		if (next != 0) return FALSE;
	    }

	    if (try->output_var == NULL)
		INTERNAL_ERROR ("output pointer NULL for option %s", try->name);

	    switch (try->opt_type) {
	    case NOTMUCH_OPT_KEYWORD:
		return _process_keyword_arg (try, value);
		break;
	    case NOTMUCH_OPT_BOOLEAN:
		*((notmuch_bool_t *)try->output_var) = TRUE;
		return TRUE;
		break;
	    case NOTMUCH_OPT_INT:
		*((int *)try->output_var) = strtol (value, &endptr, 10);
		return (*endptr == 0);
		break;
	    case NOTMUCH_OPT_STRING:
		*((const char **)try->output_var) = value;
		return TRUE;
		break;
	    case NOTMUCH_OPT_POSITION:
	    case NOTMUCH_OPT_END:
	    default:
		INTERNAL_ERROR ("unknown or unhandled option type %d", try->opt_type);
		/*UNREACHED*/
	    }
	}
	try++;
    }
    fprintf (stderr, "Unrecognized option: --%s\n", arg);
    return FALSE;
}

/* See command-line-arguments.h for description */
int
parse_arguments (int argc, char **argv,
		 const notmuch_opt_desc_t *options, int opt_index) {

    int pos_arg_index = 0;
    notmuch_bool_t more_args = TRUE;

    while (more_args && opt_index < argc) {
	if (strncmp (argv[opt_index],"--",2) != 0) {

	    more_args = parse_position_arg (argv[opt_index], pos_arg_index, options);

	    if (more_args) {
		pos_arg_index++;
		opt_index++;
	    }

	} else {

	    if (strlen (argv[opt_index]) == 2)
		return opt_index+1;

	    more_args = parse_option (argv[opt_index], options);
	    if (more_args) {
		opt_index++;
	    } else {
		opt_index = -1;
	    }

	}
    }

    return opt_index;
}
