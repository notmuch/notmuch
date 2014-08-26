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
_process_keyword_arg (const notmuch_opt_desc_t *arg_desc, char next, const char *arg_str) {

    const notmuch_keyword_t *keywords = arg_desc->keywords;

    if (next == '\0') {
	/* No keyword given */
	arg_str = "";
    }

    while (keywords->name) {
	if (strcmp (arg_str, keywords->name) == 0) {
	    if (arg_desc->output_var) {
		*((int *)arg_desc->output_var) = keywords->value;
	    }
	    return TRUE;
	}
	keywords++;
    }
    if (next != '\0')
	fprintf (stderr, "Unknown keyword argument \"%s\" for option \"%s\".\n", arg_str, arg_desc->name);
    else
	fprintf (stderr, "Option \"%s\" needs a keyword argument.\n", arg_desc->name);
    return FALSE;
}

static notmuch_bool_t
_process_boolean_arg (const notmuch_opt_desc_t *arg_desc, char next, const char *arg_str) {

    if (next == '\0') {
	*((notmuch_bool_t *)arg_desc->output_var) = TRUE;
	return TRUE;
    }
    if (strcmp (arg_str, "false") == 0) {
	*((notmuch_bool_t *)arg_desc->output_var) = FALSE;
	return TRUE;
    }
    if (strcmp (arg_str, "true") == 0) {
	*((notmuch_bool_t *)arg_desc->output_var) = TRUE;
	return TRUE;
    }
    fprintf (stderr, "Unknown argument \"%s\" for (boolean) option \"%s\".\n", arg_str, arg_desc->name);
    return FALSE;
}

static notmuch_bool_t
_process_int_arg (const notmuch_opt_desc_t *arg_desc, char next, const char *arg_str) {

    char *endptr;
    if (next == '\0' || arg_str[0] == '\0') {
	fprintf (stderr, "Option \"%s\" needs an integer argument.\n", arg_desc->name);
	return FALSE;
    }

    *((int *)arg_desc->output_var) = strtol (arg_str, &endptr, 10);
    if (*endptr == '\0')
	return TRUE;

    fprintf (stderr, "Unable to parse argument \"%s\" for option \"%s\" as an integer.\n",
	     arg_str, arg_desc->name);
    return FALSE;
}

static notmuch_bool_t
_process_string_arg (const notmuch_opt_desc_t *arg_desc, char next, const char *arg_str) {

    if (next == '\0') {
	fprintf (stderr, "Option \"%s\" needs a string argument.\n", arg_desc->name);
	return FALSE;
    }
    if (arg_str[0] == '\0') {
	fprintf (stderr, "String argument for option \"%s\" must be non-empty.\n", arg_desc->name);
	return FALSE;
    }
    *((const char **)arg_desc->output_var) = arg_str;
    return TRUE;
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

    const notmuch_opt_desc_t *try;
    for (try = options; try->opt_type != NOTMUCH_OPT_END; try++) {
	if (! try->name)
	    continue;

	if (strncmp (arg, try->name, strlen (try->name)) != 0)
	    continue;

	char next = arg[strlen (try->name)];
	const char *value = arg + strlen(try->name) + 1;

	/*
	 * If we have not reached the end of the argument (i.e. the
	 * next character is not a space or delimiter) then the
	 * argument could still match a longer option name later in
	 * the option table.
	 */
	if (next != '=' && next != ':' && next != '\0')
	    continue;

	if (try->output_var == NULL)
	    INTERNAL_ERROR ("output pointer NULL for option %s", try->name);

	switch (try->opt_type) {
	case NOTMUCH_OPT_KEYWORD:
	    return _process_keyword_arg (try, next, value);
	case NOTMUCH_OPT_BOOLEAN:
	    return _process_boolean_arg (try, next, value);
	case NOTMUCH_OPT_INT:
	    return _process_int_arg (try, next, value);
	case NOTMUCH_OPT_STRING:
	    return _process_string_arg (try, next, value);
	case NOTMUCH_OPT_POSITION:
	case NOTMUCH_OPT_END:
	default:
	    INTERNAL_ERROR ("unknown or unhandled option type %d", try->opt_type);
	    /*UNREACHED*/
	}
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
