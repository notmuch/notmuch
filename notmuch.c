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

typedef int (*command_function_t) (notmuch_config_t *config, int argc, char *argv[]);

typedef struct command {
    const char *name;
    command_function_t function;
    notmuch_bool_t create_config;
    const char *summary;
} command_t;

static int
notmuch_help_command (notmuch_config_t *config, int argc, char *argv[]);

static int
notmuch_command (notmuch_config_t *config, int argc, char *argv[]);

static command_t commands[] = {
    { NULL, notmuch_command, TRUE,
      "Notmuch main command." },
    { "setup", notmuch_setup_command, TRUE,
      "Interactively setup notmuch for first use." },
    { "new", notmuch_new_command, FALSE,
      "Find and import new messages to the notmuch database." },
    { "search", notmuch_search_command, FALSE,
      "Search for messages matching the given search terms." },
    { "show", notmuch_show_command, FALSE,
      "Show all messages matching the search terms." },
    { "count", notmuch_count_command, FALSE,
      "Count messages matching the search terms." },
    { "reply", notmuch_reply_command, FALSE,
      "Construct a reply template for a set of messages." },
    { "tag", notmuch_tag_command, FALSE,
      "Add/remove tags for all messages matching the search terms." },
    { "dump", notmuch_dump_command, FALSE,
      "Create a plain-text dump of the tags for each message." },
    { "restore", notmuch_restore_command, FALSE,
      "Restore the tags from the given dump file (see 'dump')." },
    { "config", notmuch_config_command, FALSE,
      "Get or set settings in the notmuch configuration file." },
    { "help", notmuch_help_command, TRUE, /* create but don't save config */
      "This message, or more detailed help for the named command." }
};

static command_t *
find_command (const char *name)
{
    size_t i;

    for (i = 0; i < ARRAY_SIZE (commands); i++)
	if ((!name && !commands[i].name) ||
	    (name && commands[i].name && strcmp (name, commands[i].name) == 0))
	    return &commands[i];

    return NULL;
}

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

	if (command->name)
	    fprintf (out, "  %-11s  %s\n", command->name, command->summary);
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
notmuch_help_command (notmuch_config_t *config, int argc, char *argv[])
{
    command_t *command;

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

    command = find_command (argv[0]);
    if (command) {
	char *page = talloc_asprintf (config, "notmuch-%s", command->name);
	exec_man (page);
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
notmuch_command (notmuch_config_t *config,
		 unused(int argc), unused(char *argv[]))
{
    char *db_path;
    struct stat st;

    /* If the user has never configured notmuch, then run
     * notmuch_setup_command which will give a nice welcome message,
     * and interactively guide the user through the configuration. */
    if (notmuch_config_is_new (config))
	return notmuch_setup_command (config, 0, NULL);

    /* Notmuch is already configured, but is there a database? */
    db_path = talloc_asprintf (config, "%s/%s",
			       notmuch_config_get_database_path (config),
			       ".notmuch");
    if (stat (db_path, &st)) {
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

    return 0;
}

static int
redirect_stderr (const char * stderr_file)
{
    if (strcmp (stderr_file, "-") == 0) {
	if (dup2 (STDOUT_FILENO, STDERR_FILENO) < 0) {
	    perror ("dup2");
	    return 1;
	}
    } else {
	int fd = open (stderr_file, O_WRONLY|O_CREAT|O_TRUNC, 0666);
	if (fd < 0) {
	    fprintf (stderr, "Error: Cannot redirect stderr to '%s': %s\n",
		     stderr_file, strerror (errno));
	    return 1;
	}
	if (fd != STDERR_FILENO) {
	    if (dup2 (fd, STDERR_FILENO) < 0) {
		perror ("dup2");
		return 1;
	    }
	    close (fd);
	}
    }
    return 0;
}

int
main (int argc, char *argv[])
{
    void *local;
    char *talloc_report;
    const char *command_name = NULL;
    command_t *command;
    char *config_file_name = NULL;
    char *stderr_file = NULL;
    notmuch_config_t *config;
    notmuch_bool_t print_help=FALSE, print_version=FALSE;
    int opt_index;
    int ret = 0;

    notmuch_opt_desc_t options[] = {
	{ NOTMUCH_OPT_BOOLEAN, &print_help, "help", 'h', 0 },
	{ NOTMUCH_OPT_BOOLEAN, &print_version, "version", 'v', 0 },
	{ NOTMUCH_OPT_STRING, &config_file_name, "config", 'c', 0 },
	{ NOTMUCH_OPT_STRING, &stderr_file, "stderr", '\0', 0 },
	{ 0, 0, 0, 0, 0 }
    };

    talloc_enable_null_tracking ();

    local = talloc_new (NULL);

    g_mime_init (0);
    g_type_init ();

    /* Globally default to the current output format version. */
    notmuch_format_version = NOTMUCH_FORMAT_CUR;

    opt_index = parse_arguments (argc, argv, options, 1);
    if (opt_index < 0) {
	/* diagnostics already printed */
	return 1;
    }

    if (stderr_file && redirect_stderr (stderr_file) != 0) {
	/* error already printed */
	return 1;
    }
    if (print_help)
	return notmuch_help_command (NULL, argc - 1, &argv[1]);

    if (print_version) {
	printf ("notmuch " STRINGIFY(NOTMUCH_VERSION) "\n");
	return 0;
    }

    if (opt_index < argc)
	command_name = argv[opt_index];

    command = find_command (command_name);
    if (!command) {
	fprintf (stderr, "Error: Unknown command '%s' (see \"notmuch help\")\n",
		 command_name);
	return 1;
    }

    config = notmuch_config_open (local, config_file_name, command->create_config);
    if (!config)
	return 1;

    ret = (command->function)(config, argc - opt_index, argv + opt_index);

    notmuch_config_close (config);

    talloc_report = getenv ("NOTMUCH_TALLOC_REPORT");
    if (talloc_report && strcmp (talloc_report, "") != 0) {
	/* this relies on the previous call to
	 * talloc_enable_null_tracking
	 */

	FILE *report = fopen (talloc_report, "w");
	if (report) {
	    talloc_report_full (NULL, report);
	} else {
	    ret = 1;
	    fprintf (stderr, "ERROR: unable to write talloc log. ");
	    perror (talloc_report);
	}
    }

    talloc_free (local);

    return ret;
}
