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
    const char *documentation;
} command_t;

static int
notmuch_help_command (void *ctx, int argc, char *argv[]);

static const char search_terms_help[] =
    "\t\tSeveral notmuch commands accept a comman syntax for search\n"
    "\t\tterms.\n"
    "\n"
    "\t\tThe search terms can consist of free-form text (and quoted\n"
    "\t\tphrases) which will match all messages that contain all of\n"
    "\t\tthe given terms/phrases in the body, the subject, or any of\n"
    "\t\tthe sender or recipient headers.\n"
    "\n"
    "\t\tIn addition to free text, the following prefixes can be used\n"
    "\t\tto force terms to match against specific portions of an email,\n"
    "\t\t(where <brackets> indicate user-supplied values):\n"
    "\n"
    "\t\t\tfrom:<name-or-address>\n"
    "\t\t\tto:<name-or-address>\n"
    "\t\t\tsubject:<word-or-quoted-phrase>\n"
    "\t\t\tattachment:<word>\n"
    "\t\t\ttag:<tag>\n"
    "\t\t\tid:<message-id>\n"
    "\t\t\tthread:<thread-id>\n"
    "\n"
    "\t\tThe from: prefix is used to match the name or address of\n"
    "\t\tthe sender of an email message.\n"
    "\n"
    "\t\tThe to: prefix is used to match the names or addresses of\n"
    "\t\tany recipient of an email message, (whether To, Cc, or Bcc).\n"
    "\n"
    "\t\tAny term prefixed with subject: will match only text from\n"
    "\t\tthe subject of an email. Quoted phrases are supported when\n"
    "\t\tsearching with: subject:\"this is a phrase\".\n"
    "\n"
    "\t\tFor tag:, valid tag values include \"inbox\" and \"unread\"\n"
    "\t\tby default for new messages added by \"notmuch new\" as well\n"
    "\t\tas any other tag values added manually with \"notmuch tag\".\n"
    "\n"
    "\t\tFor id:, message ID values are the literal contents of the\n"
    "\t\tMessage-ID: header of email messages, but without the '<','>'\n"
    "\t\tdelimiters.\n"
    "\n"
    "\t\tThe thread: prefix can be used with the thread ID values that\n"
    "\t\tare generated internally by notmuch (and do not appear in email\n"
    "\t\tmessages). These thread ID values can be seen in the first\n"
    "\t\tcolumn of output from \"notmuch search\".\n"
    "\n"
    "\t\tIn addition to individual terms, multiple terms can be\n"
    "\t\tcombined with Boolean operators (\"and\", \"or\", \"not\", etc.).\n"
    "\t\tEach term in the query will be implicitly connected by a\n"
    "\t\tlogical AND if no explicit operator is provided, (except\n"
    "\t\tthat terms with a common prefix will be implicitly combined\n"
    "\t\twith OR until we get Xapian defect #402 fixed).\n"
    "\n"
    "\t\tParentheses can also be used to control the combination of\n"
    "\t\tthe Boolean operators, but will have to be protected from\n"
    "\t\tinterpretation by the shell, (such as by putting quotation\n"
    "\t\tmarks around any parenthesized expression).\n"
    "\n"
    "\t\tFinally, results can be restricted to only messages within a\n"
    "\t\tparticular time range, (based on the Date: header) with:\n"
    "\n"
    "\t\t\t<intial-timestamp>..<final-timestamp>\n"
    "\n"
    "\t\tEach timestamp is a number representing the number of seconds\n"
    "\t\tsince 1970-01-01 00:00:00 UTC. This is not the most convenient\n"
    "\t\tmeans of expressing date ranges, but until notmuch is fixed to\n"
    "\t\taccept a more convenient form, one can use the date program to\n"
    "\t\tconstruct timestamps. For example, with the bash shell the\n"
    "\t\tfollowing syntax would specify a date range to return messages\n"
    "\t\tfrom 2009-10-01 until the current time:\n"
    "\n"
    "\t\t\t$(date +%s -d 2009-10-01)..$(date +%s)\n\n";

command_t commands[] = {
    { "setup", notmuch_setup_command,
      NULL,
      "Interactively setup notmuch for first use.",
      "\t\tThe setup command will prompt for your full name, your primary\n"
      "\t\temail address, any alternate email addresses you use, and the\n"
      "\t\tdirectory containing your email archives. Your answers will be\n"
      "\t\twritten to a configuration file in ${NOTMUCH_CONFIG} (if set)\n"
      "\t\tor ${HOME}/.notmuch-config.\n"
      "\n"
      "\t\tThis configuration file will be created with descriptive\n"
      "\t\tcomments, making it easy to edit by hand later to change the\n"
      "\t\tconfiguration. Or you can run \"notmuch setup\" again.\n"
      "\n"
      "\t\tInvoking notmuch with no command argument will run setup if\n"
      "\t\tthe setup command has not previously been completed." },
    { "new", notmuch_new_command,
      "[--verbose]",
      "\t\tFind and import new messages to the notmuch database.",
      "\t\tScans all sub-directories of the mail directory, performing\n"
      "\t\tfull-text indexing on new messages that are found. Each new\n"
      "\t\tmessage will be tagged as both \"inbox\" and \"unread\".\n"
      "\n"
      "\t\tYou should run \"notmuch new\" once after first running\n"
      "\t\t\"notmuch setup\" to create the initial database. The first\n"
      "\t\trun may take a long time if you have a significant amount of\n"
      "\t\tmail (several hundred thousand messages or more).\n"
      "\n"
      "\t\tSubsequently, you should run \"notmuch new\" whenever new mail\n"
      "\t\tis delivered and you wish to incorporate it into the database.\n"
      "\t\tThese subsequent runs will be much quicker than the initial run.\n"
      "\n"
      "\t\tSupported options for new include:\n"
      "\n"
      "\t\t--verbose\n"
      "\n"
      "\t\t\tVerbose operation. Shows paths of message files as\n"
      "\t\t\tthey are being indexed.\n"
      "\n"
      "\t\tNote: \"notmuch new\" runs (other than the first run) will\n"
      "\t\tskip any read-only directories, so you can use that to mark\n"
      "\t\tdirectories that will not receive any new mail (and make\n"
      "\t\t\"notmuch new\" even faster).\n"
      "\n"
      "\t\tInvoking notmuch with no command argument will run new if\n"
      "\t\tthe setup command has previously been completed, but new has\n"
      "\t\tnot previously been run." },
    { "search", notmuch_search_command,
      "[options...] <search-terms> [...]",
      "\t\tSearch for messages matching the given search terms.",
      "\t\tNote that the individual mail messages will be matched\n"
      "\t\tagainst the search terms, but the results will be the\n"
      "\t\tthreads (one per line) containing the matched messages.\n"
      "\n"
      "\t\tSupported options for search include:\n"
      "\n"
      "\t\t--max-threads=<value>\n"
      "\n"
      "\t\t\tRestricts displayed search results to a subset\n"
      "\t\t\tof the results that would match the terms.\n"
      "\n"
      "\t\t--first=<value>\n"
      "\n"
      "\t\t\tOmits the first <value> threads from the search\n"
      "\t\t\tresults that would otherwise be displayed.\n"
      "\n"
      "\t\t--sort=(newest-first|oldest-first)\n"
      "\n"
      "\t\t\tPresent results in either chronological order\n"
      "\t\t\t(oldest-first) or reverse chronological order\n"
      "\t\t\t(newest-first), which is the default.\n"
      "\n"
      "\t\tSee \"notmuch help search-terms\" for details of the search\n"
      "\t\tterms syntax." },
    { "show", notmuch_show_command,
      "<search-terms> [...]",
      "\t\tShow all messages matching the search terms.",
      "\t\tThe messages are grouped and sorted based on the threading\n"
      "\t\t(all replies to a particular message appear immediately\n"
      "\t\tafter that message in date order).\n"
      "\n"
      "\t\tThe output format is plain-text, with all text-content\n"
      "\t\tMIME parts decoded. Various components in the output,\n"
      "\t\t('message', 'header', 'body', 'attachment', and MIME 'part')\n"
      "\t\tare delimited by easily-parsed markers. Each marker consists\n"
      "\t\tof a Control-L character (ASCII decimal 12), the name of\n"
      "\t\tthe marker, and then either an opening or closing brace,\n"
      "\t\t'{' or '}' to either open or close the component.\n"
      "\n"
      "\t\tA common use of \"notmuch show\" is to display a single\n"
      "\t\tthread of email messages. For this, use a search term of\n"
      "\t\t\"thread:<thread-id>\" as can be seen in the first column\n"
      "\t\tof output from the \"notmuch search\" command.\n"
      "\n"
      "\t\tSee \"notmuch help search-terms\" for details of the search\n"
      "\t\tterms syntax." },
    { "count", notmuch_count_command,
      "<search-terms> [...]",
      "\t\tCount messages matching the search terms.",
      "\t\tThe number of matching messages is output to stdout.\n"
      "\n"
      "\t\tA common use of \"notmuch count\" is to display the count\n"
      "\t\tof messages matching both a specific tag and either inbox\n"
      "\t\tor unread\n"
      "\n"
      "\t\tSee \"notmuch help search-terms\" for details of the search\n"
      "\t\tterms syntax." },
    { "reply", notmuch_reply_command,
      "<search-terms> [...]",
      "\t\tConstruct a reply template for a set of messages.",
      "\t\tConstructs a new message as a reply to a set of existing\n"
      "\t\tmessages. The Reply-To: header (if any, otherwise From:) is\n"
      "\t\tused for the To: address. The To: and Cc: headers are copied,\n"
      "\t\tbut not including any of the user's configured addresses.\n"
      "\n"
      "\t\tA suitable subject is constructed. The In-Reply-to: and\n"
      "\t\tReferences: headers are set appropriately, and the content\n"
      "\t\tof the original messages is quoted and included in the body.\n"
      "\n"
      "\t\tThe resulting message template is output to stdout.\n"
      "\n"
      "\t\tSee \"notmuch help search-terms\" for details of the search\n"
      "\t\tterms syntax." },
    { "tag", notmuch_tag_command,
      "+<tag>|-<tag> [...] [--] <search-terms> [...]",
      "\t\tAdd/remove tags for all messages matching the search terms.",
      "\t\tThe search terms are handled exactly as in 'search' so one\n"
      "\t\tcan use that command first to see what will be modified.\n"
      "\n"
      "\t\tTags prefixed by '+' are added while those prefixed by\n"
      "\t\t'-' are removed. For each message, tag removal is performed\n"
      "\t\tbefore tag addition.\n"
      "\n"
      "\t\tThe beginning of <search-terms> is recognized by the first\n"
      "\t\targument that begins with neither '+' nor '-'. Support for\n"
      "\t\tan initial search term beginning with '+' or '-' is provided\n"
      "\t\tby allowing the user to specify a \"--\" argument to separate\n"
      "\t\tthe tags from the search terms.\n"
      "\n"
      "\t\tSee \"notmuch help search-terms\" for details of the search\n"
      "\t\tterms syntax." },
    { "dump", notmuch_dump_command,
      "[<filename>]",
      "\t\tCreate a plain-text dump of the tags for each message.",
      "\t\tOutput is to the given filename, if any, or to stdout.\n"
      "\t\tThese tags are the only data in the notmuch database\n"
      "\t\tthat can't be recreated from the messages themselves.\n"
      "\t\tThe output of notmuch dump is therefore the only\n"
      "\t\tcritical thing to backup (and much more friendly to\n"
      "\t\tincremental backup than the native database files.)" },
    { "restore", notmuch_restore_command,
      "<filename>",
      "\t\tRestore the tags from the given dump file (see 'dump').",
      "\t\tNote: The dump file format is specifically chosen to be\n"
      "\t\tcompatible with the format of files produced by sup-dump.\n"
      "\t\tSo if you've previously been using sup for mail, then the\n"
      "\t\t\"notmuch restore\" command provides you a way to import\n"
      "\t\tall of your tags (or labels as sup calls them)." },
    { "help", notmuch_help_command,
      "[<command>]",
      "\t\tThis message, or more detailed help for the named command.",
      "\t\tExcept in this case, where there's not much more detailed\n"
      "\t\thelp available." }
};

static void
usage (FILE *out)
{
    command_t *command;
    unsigned int i;

    fprintf (out, "Usage: notmuch <command> [args...]\n");
    fprintf (out, "\n");
    fprintf (out, "Where <command> and [args...] are as follows:\n");
    fprintf (out, "\n");

    for (i = 0; i < ARRAY_SIZE (commands); i++) {
	command = &commands[i];

	if (command->arguments)
	    fprintf (out, "\t%s\t%s\n\n%s\n\n",
		     command->name, command->arguments, command->summary);
	else
	    fprintf (out, "\t%s\t%s\n\n",
		     command->name, command->summary);
    }

    fprintf (out,
    "Use \"notmuch help <command>\" for more details on each command.\n"
    "And \"notmuch help search-terms\" for the common search-terms syntax.\n\n");
}

static int
notmuch_help_command (unused (void *ctx), int argc, char *argv[])
{
    command_t *command;
    unsigned int i;

    if (argc == 0) {
	printf ("The notmuch mail system.\n\n");
	usage (stdout);
	return 0;
    }

    for (i = 0; i < ARRAY_SIZE (commands); i++) {
	command = &commands[i];

	if (strcmp (argv[0], command->name) == 0) {
	    printf ("Help for \"notmuch %s\":\n\n", argv[0]);
	    if (command->arguments)
		printf ("\t%s\t%s\n\n%s\n\n%s\n\n",
			command->name, command->arguments,
			command->summary, command->documentation);
	    else
		printf ("\t%s\t%s\n\n%s\n\n", command->name,
			command->summary, command->documentation);
	    return 0;
	}
    }

    if (strcmp (argv[0], "search-terms") == 0) {
	printf ("Help for <%s>\n\n", argv[0]);
	for (i = 0; i < ARRAY_SIZE (commands); i++) {
	    command = &commands[i];

	    if (command->arguments &&
		strstr (command->arguments, "search-terms"))
	    {
		printf ("\t%s\t%s\n",
			command->name, command->arguments);
	    }
	}
	printf ("\n");
	printf (search_terms_help);
	return 0;
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
    unsigned int i;

    local = talloc_new (NULL);

    g_mime_init (0);

    if (argc == 1)
	return notmuch (local);

    for (i = 0; i < ARRAY_SIZE (commands); i++) {
	command = &commands[i];

	if (strcmp (argv[1], command->name) == 0)
	    return (command->function) (local, argc - 2, &argv[2]);
    }

    fprintf (stderr, "Error: Unknown command '%s' (see \"notmuch help\")\n",
	     argv[1]);

    talloc_free (local);

    return 1;
}
