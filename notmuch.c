/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2009 Carl Worth
 * Copyright © 2009 Keith Packard
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
 * Authors: Carl Worth <cworth@cworth.org>
 *	    Keith Packard <keithp@keithp.com>
 */

#include "notmuch-client.h"

typedef int (*command_function_t) (void *ctx, int argc, char *argv[]);

typedef struct command {
    const char *name;
    command_function_t function;
    const char *arguments;
    const char *summary;
} command_t;

#define MAX_ALIAS_SUBSTITUTIONS 3

typedef struct alias {
    const char *name;
    const char *substitutions[MAX_ALIAS_SUBSTITUTIONS];
} alias_t;

alias_t aliases[] = {
    { "part", { "show", "--format=raw"}},
    { "search-tags", {"search", "--output=tags", "*"}}
};

static int
notmuch_help_command (void *ctx, int argc, char *argv[]);

static command_t commands[] = {
    { "setup", notmuch_setup_command,
      NULL,
      "Interactively setup notmuch for first use." },
    { "new", notmuch_new_command,
      "[options...]",
      "Find and import new messages to the notmuch database." },
    { "search", notmuch_search_command,
      "[options...] <search-terms> [...]",
      "Search for messages matching the given search terms." },
    { "show", notmuch_show_command,
      "<search-terms> [...]",
      "Show all messages matching the search terms." },
    { "count", notmuch_count_command,
      "[options...] <search-terms> [...]",
      "Count messages matching the search terms." },
    { "reply", notmuch_reply_command,
      "[options...] <search-terms> [...]",
      "Construct a reply template for a set of messages." },
    { "tag", notmuch_tag_command,
      "+<tag>|-<tag> [...] [--] <search-terms> [...]" ,
      "Add/remove tags for all messages matching the search terms." },
    { "dump", notmuch_dump_command,
      "[<filename>] [--] [<search-terms>]",
      "Create a plain-text dump of the tags for each message." },
    { "restore", notmuch_restore_command,
      "[--accumulate] [<filename>]",
      "Restore the tags from the given dump file (see 'dump')." },
    { "config", notmuch_config_command,
      "[get|set] <section>.<item> [value ...]",
      "Get or set settings in the notmuch configuration file." },
    { "help", notmuch_help_command,
      "[<command>]",
      "This message, or more detailed help for the named command." }
};

int notmuch_format_version;

static void
usage (FILE *out)
{
    command_t *command;
    unsigned int i;

    fprintf (out,
	     "Usage: notmuch --help\n"
	     "       notmuch --version\n"
	     "       notmuch <command> [args...]\n");
    fprintf (out, "\n");
    fprintf (out, "The available commands are as follows:\n");
    fprintf (out, "\n");

    for (i = 0; i < ARRAY_SIZE (commands); i++) {
	command = &commands[i];

	fprintf (out, "  %-11s  %s\n",
		 command->name, command->summary);
    }

    fprintf (out, "\n");
    fprintf (out,
    "Use \"notmuch help <command>\" for more details on each command\n"
    "and \"notmuch help search-terms\" for the common search-terms syntax.\n\n");
}

void
notmuch_exit_if_unsupported_format (void)
{
    if (notmuch_format_version > NOTMUCH_FORMAT_CUR) {
	fprintf (stderr, "\
A caller requested output format version %d, but the installed notmuch\n\
CLI only supports up to format version %d.  You may need to upgrade your\n\
notmuch CLI.\n",
		 notmuch_format_version, NOTMUCH_FORMAT_CUR);
	exit (NOTMUCH_EXIT_FORMAT_TOO_NEW);
    } else if (notmuch_format_version < NOTMUCH_FORMAT_MIN) {
	fprintf (stderr, "\
A caller requested output format version %d, which is no longer supported\n\
by the notmuch CLI (it requires at least version %d).  You may need to\n\
upgrade your notmuch front-end.\n",
		 notmuch_format_version, NOTMUCH_FORMAT_MIN);
	exit (NOTMUCH_EXIT_FORMAT_TOO_OLD);
    } else if (notmuch_format_version != NOTMUCH_FORMAT_CUR) {
	/* Warn about old version requests so compatibility issues are
	 * less likely when we drop support for a deprecated format
	 * versions. */
	fprintf (stderr, "\
A caller requested deprecated output format version %d, which may not\n\
be supported in the future.\n", notmuch_format_version);
    }
}

static void
exec_man (const char *page)
{
    if (execlp ("man", "man", page, (char *) NULL)) {
	perror ("exec man");
	exit (1);
    }
}

static int
notmuch_help_command (void *ctx, int argc, char *argv[])
{
    command_t *command;
    unsigned int i;

    argc--; argv++; /* Ignore "help" */

    if (argc == 0) {
	printf ("The notmuch mail system.\n\n");
	usage (stdout);
	return 0;
    }

    if (strcmp (argv[0], "help") == 0) {
	printf ("The notmuch help system.\n\n"
		"\tNotmuch uses the man command to display help. In case\n"
		"\tof difficulties check that MANPATH includes the pages\n"
		"\tinstalled by notmuch.\n\n"
		"\tTry \"notmuch help\" for a list of topics.\n");
	return 0;
    }

    for (i = 0; i < ARRAY_SIZE (commands); i++) {
	command = &commands[i];

	if (strcmp (argv[0], command->name) == 0) {
	    char *page = talloc_asprintf (ctx, "notmuch-%s", command->name);
	    exec_man (page);
	}
    }

    if (strcmp (argv[0], "search-terms") == 0) {
	exec_man ("notmuch-search-terms");
    } else if (strcmp (argv[0], "hooks") == 0) {
	exec_man ("notmuch-hooks");
    }

    fprintf (stderr,
	     "\nSorry, %s is not a known command. There's not much I can do to help.\n\n",
	     argv[0]);
    return 1;
}

/* Handle the case of "notmuch" being invoked with no command
 * argument. For now we just call notmuch_setup_command, but we plan
 * to be more clever about this in the future.
 */
static int
notmuch (void *ctx)
{
    notmuch_config_t *config;
    notmuch_bool_t is_new;
    char *db_path;
    struct stat st;

    config = notmuch_config_open (ctx, NULL, &is_new);

    /* If the user has never configured notmuch, then run
     * notmuch_setup_command which will give a nice welcome message,
     * and interactively guide the user through the configuration. */
    if (is_new) {
	notmuch_config_close (config);
	return notmuch_setup_command (ctx, 0, NULL);
    }

    /* Notmuch is already configured, but is there a database? */
    db_path = talloc_asprintf (ctx, "%s/%s",
			       notmuch_config_get_database_path (config),
			       ".notmuch");
    if (stat (db_path, &st)) {
	notmuch_config_close (config);
	if (errno != ENOENT) {
	    fprintf (stderr, "Error looking for notmuch database at %s: %s\n",
		     db_path, strerror (errno));
	    return 1;
	}
	printf ("Notmuch is configured, but there's not yet a database at\n\n\t%s\n\n",
		db_path);
	printf ("You probably want to run \"notmuch new\" now to create that database.\n\n"
		"Note that the first run of \"notmuch new\" can take a very long time\n"
		"and that the resulting database will use roughly the same amount of\n"
		"storage space as the email being indexed.\n\n");
	return 0;
    }

    printf ("Notmuch is configured and appears to have a database. Excellent!\n\n"
	    "At this point you can start exploring the functionality of notmuch by\n"
	    "using commands such as:\n\n"
	    "\tnotmuch search tag:inbox\n\n"
	    "\tnotmuch search to:\"%s\"\n\n"
	    "\tnotmuch search from:\"%s\"\n\n"
	    "\tnotmuch search subject:\"my favorite things\"\n\n"
	    "See \"notmuch help search\" for more details.\n\n"
	    "You can also use \"notmuch show\" with any of the thread IDs resulting\n"
	    "from a search. Finally, you may want to explore using a more sophisticated\n"
	    "interface to notmuch such as the emacs interface implemented in notmuch.el\n"
	    "or any other interface described at http://notmuchmail.org\n\n"
	    "And don't forget to run \"notmuch new\" whenever new mail arrives.\n\n"
	    "Have fun, and may your inbox never have much mail.\n\n",
	    notmuch_config_get_user_name (config),
	    notmuch_config_get_user_primary_email (config));

    notmuch_config_close (config);

    return 0;
}

int
main (int argc, char *argv[])
{
    void *local;
    command_t *command;
    alias_t *alias;
    unsigned int i, j;
    const char **argv_local;

    talloc_enable_null_tracking ();

    local = talloc_new (NULL);

    g_mime_init (0);
    g_type_init ();

    /* Globally default to the current output format version. */
    notmuch_format_version = NOTMUCH_FORMAT_CUR;

    if (argc == 1)
	return notmuch (local);

    if (strcmp (argv[1], "--help") == 0)
	return notmuch_help_command (NULL, argc - 1, &argv[1]);

    if (strcmp (argv[1], "--version") == 0) {
	printf ("notmuch " STRINGIFY(NOTMUCH_VERSION) "\n");
	return 0;
    }

    for (i = 0; i < ARRAY_SIZE (aliases); i++) {
	alias = &aliases[i];

	if (strcmp (argv[1], alias->name) == 0)
	{
	    int substitutions;

	    argv_local = talloc_size (local, sizeof (char *) *
				      (argc + MAX_ALIAS_SUBSTITUTIONS - 1));
	    if (argv_local == NULL) {
		fprintf (stderr, "Out of memory.\n");
		return 1;
	    }

	    /* Copy all substution arguments from the alias. */
	    argv_local[0] = argv[0];
	    for (j = 0; j < MAX_ALIAS_SUBSTITUTIONS; j++) {
		if (alias->substitutions[j] == NULL)
		    break;
		argv_local[j+1] = alias->substitutions[j];
	    }
	    substitutions = j;

	    /* And copy all original arguments (skipping the argument
	     * that matched the alias of course. */
	    for (j = 2; j < (unsigned) argc; j++) {
		argv_local[substitutions+j-1] = argv[j];
	    }

	    argc += substitutions - 1;
	    argv = (char **) argv_local;
	}
    }

    for (i = 0; i < ARRAY_SIZE (commands); i++) {
	command = &commands[i];

	if (strcmp (argv[1], command->name) == 0) {
	    int ret;
	    char *talloc_report;

	    ret = (command->function)(local, argc - 1, &argv[1]);

	    /* in the future support for this environment variable may
	     * be supplemented or replaced by command line arguments
	     * --leak-report and/or --leak-report-full */

	    talloc_report = getenv ("NOTMUCH_TALLOC_REPORT");

	    /* this relies on the previous call to
	     * talloc_enable_null_tracking */

	    if (talloc_report && strcmp (talloc_report, "") != 0) {
		FILE *report = fopen (talloc_report, "w");
		talloc_report_full (NULL, report);
	    }

	    return ret;
	}
    }

    fprintf (stderr, "Error: Unknown command '%s' (see \"notmuch help\")\n",
	     argv[1]);

    talloc_free (local);

    return 1;
}
