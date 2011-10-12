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

static const char search_terms_help[] =
    "\tSeveral notmuch commands accept a common syntax for search\n"
    "\tterms.\n"
    "\n"
    "\tThe search terms can consist of free-form text (and quoted\n"
    "\tphrases) which will match all messages that contain all of\n"
    "\tthe given terms/phrases in the body, the subject, or any of\n"
    "\tthe sender or recipient headers.\n"
    "\n"
    "\tAs a special case, a search string consisting of exactly a\n"
    "\tsingle asterisk (\"*\") will match all messages.\n"
    "\n"
    "\tIn addition to free text, the following prefixes can be used\n"
    "\tto force terms to match against specific portions of an email,\n"
    "\t(where <brackets> indicate user-supplied values):\n"
    "\n"
    "\t\tfrom:<name-or-address>\n"
    "\t\tto:<name-or-address>\n"
    "\t\tsubject:<word-or-quoted-phrase>\n"
    "\t\tattachment:<word>\n"
    "\t\ttag:<tag> (or is:<tag>)\n"
    "\t\tid:<message-id>\n"
    "\t\tthread:<thread-id>\n"
    "\t\tfolder:<directory-path>\n"
    "\n"
    "\tThe from: prefix is used to match the name or address of\n"
    "\tthe sender of an email message.\n"
    "\n"
    "\tThe to: prefix is used to match the names or addresses of\n"
    "\tany recipient of an email message, (whether To, Cc, or Bcc).\n"
    "\n"
    "\tAny term prefixed with subject: will match only text from\n"
    "\tthe subject of an email. Quoted phrases are supported when\n"
    "\tsearching with: subject:\"this is a phrase\".\n"
    "\n"
    "\tFor tag: and is:, valid tag values include \"inbox\" and \"unread\"\n"
    "\tby default for new messages added by \"notmuch new\" as well\n"
    "\tas any other tag values added manually with \"notmuch tag\".\n"
    "\n"
    "\tFor id:, message ID values are the literal contents of the\n"
    "\tMessage-ID: header of email messages, but without the '<','>'\n"
    "\tdelimiters.\n"
    "\n"
    "\tThe thread: prefix can be used with the thread ID values that\n"
    "\tare generated internally by notmuch (and do not appear in email\n"
    "\tmessages). These thread ID values can be seen in the first\n"
    "\tcolumn of output from \"notmuch search\".\n"
    "\n"
    "\tThe folder: prefix can be used to search for email message\n"
    "\tfiles that are contained within particular directories within\n"
    "\tthe mail store. Only the directory components below the top-level\n"
    "\tmail database path are available to be searched.\n"
    "\n"
    "\tIn addition to individual terms, multiple terms can be\n"
    "\tcombined with Boolean operators (\"and\", \"or\", \"not\", etc.).\n"
    "\tEach term in the query will be implicitly connected by a\n"
    "\tlogical AND if no explicit operator is provided, (except\n"
    "\tthat terms with a common prefix will be implicitly combined\n"
    "\twith OR until we get Xapian defect #402 fixed).\n"
    "\n"
    "\tParentheses can also be used to control the combination of\n"
    "\tthe Boolean operators, but will have to be protected from\n"
    "\tinterpretation by the shell, (such as by putting quotation\n"
    "\tmarks around any parenthesized expression).\n"
    "\n"
    "\tFinally, results can be restricted to only messages within a\n"
    "\tparticular time range, (based on the Date: header) with:\n"
    "\n"
    "\t\t<intial-timestamp>..<final-timestamp>\n"
    "\n"
    "\tEach timestamp is a number representing the number of seconds\n"
    "\tsince 1970-01-01 00:00:00 UTC. This is not the most convenient\n"
    "\tmeans of expressing date ranges, but until notmuch is fixed to\n"
    "\taccept a more convenient form, one can use the date program to\n"
    "\tconstruct timestamps. For example, with the bash shell the\n"
    "\tfollowing syntax would specify a date range to return messages\n"
    "\tfrom 2009-10-01 until the current time:\n"
    "\n"
    "\t\t$(date +%%s -d 2009-10-01)..$(date +%%s)\n\n";

static command_t commands[] = {
    { "setup", notmuch_setup_command,
      NULL,
      "Interactively setup notmuch for first use.",
      "\tThe setup command will prompt for your full name, your primary\n"
      "\temail address, any alternate email addresses you use, and the\n"
      "\tdirectory containing your email archives. Your answers will be\n"
      "\twritten to a configuration file in ${NOTMUCH_CONFIG} (if set)\n"
      "\tor ${HOME}/.notmuch-config.\n"
      "\n"
      "\tThis configuration file will be created with descriptive\n"
      "\tcomments, making it easy to edit by hand later to change the\n"
      "\tconfiguration. Or you can run \"notmuch setup\" again.\n"
      "\n"
      "\tInvoking notmuch with no command argument will run setup if\n"
      "\tthe setup command has not previously been completed." },
    { "new", notmuch_new_command,
      "[--verbose]",
      "Find and import new messages to the notmuch database.",
      "\tScans all sub-directories of the mail directory, performing\n"
      "\tfull-text indexing on new messages that are found. Each new\n"
      "\tmessage will be tagged as both \"inbox\" and \"unread\".\n"
      "\n"
      "\tYou should run \"notmuch new\" once after first running\n"
      "\t\"notmuch setup\" to create the initial database. The first\n"
      "\trun may take a long time if you have a significant amount of\n"
      "\tmail (several hundred thousand messages or more).\n"
      "\n"
      "\tSubsequently, you should run \"notmuch new\" whenever new mail\n"
      "\tis delivered and you wish to incorporate it into the database.\n"
      "\tThese subsequent runs will be much quicker than the initial run.\n"
      "\n"
      "\tSupported options for new include:\n"
      "\n"
      "\t--verbose\n"
      "\n"
      "\t\tVerbose operation. Shows paths of message files as\n"
      "\t\tthey are being indexed.\n"
      "\n"
      "\tInvoking notmuch with no command argument will run new if\n"
      "\tthe setup command has previously been completed, but new has\n"
      "\tnot previously been run." },
    { "search", notmuch_search_command,
      "[options...] <search-terms> [...]",
      "Search for messages matching the given search terms.",
      "\tNote that the individual mail messages will be matched\n"
      "\tagainst the search terms, but the results will be the\n"
      "\tthreads (one per line) containing the matched messages.\n"
      "\n"
      "\tSupported options for search include:\n"
      "\n"
      "\t--format=(json|text)\n"
      "\n"
      "\t\tPresents the results in either JSON or\n"
      "\t\tplain-text (default)\n"
      "\n"
      "\t--output=(summary|threads|messages|files|tags)\n"
      "\n"
      "\t\tsummary (default)\n"
      "\n"
      "\t\tOutput a summary of each thread with any message matching the\n"
      "\t\tsearch terms. The summary includes the thread ID, date, the\n"
      "\t\tnumber of messages in the thread (both the number matched and\n"
      "\t\tthe total number), the authors of the thread and the subject.\n"
      "\n"
      "\t\tthreads\n"
      "\n"
      "\t\tOutput the thread IDs of all threads with any message matching\n"
      "\t\tthe search terms, either one per line (--format=text) or as a\n"
      "\t\tJSON array (--format=json).\n"
      "\n"
      "\t\tmessages\n"
      "\n"
      "\t\tOutput the message IDs of all messages matching the search\n"
      "\t\tterms, either one per line (--format=text) or as a JSON array\n"
      "\t\t(--format=json).\n"
      "\n"
      "\t\tfiles\n"
      "\n"
      "\t\tOutput the filenames of all messages matching the search\n"
      "\t\tterms, either one per line (--format=text) or as a JSON array\n"
      "\t\t(--format=json).\n"
      "\n"
      "\t\ttags\n"
      "\n"
      "\t\tOutput all tags that appear on any message matching the search\n"
      "\t\tterms, either one per line (--format=text) or as a JSON array\n"
      "\t\t(--format=json).\n"
      "\n"
      "\t--sort=(newest-first|oldest-first)\n"
      "\n"
      "\t\tPresent results in either chronological order\n"
      "\t\t(oldest-first) or reverse chronological order\n"
      "\t\t(newest-first), which is the default.\n"
      "\n"
      "\tSee \"notmuch help search-terms\" for details of the search\n"
      "\tterms syntax." },
    { "show", notmuch_show_command,
      "<search-terms> [...]",
      "Show all messages matching the search terms.",
      "\tThe messages are grouped and sorted based on the threading\n"
      "\t(all replies to a particular message appear immediately\n"
      "\tafter that message in date order).\n"
      "\n"
      "\tSupported options for show include:\n"
      "\n"
      "\t--entire-thread\n"
      "\n"
      "\t\tBy default only those messages that match the\n"
      "\t\tsearch terms will be displayed. With this option,\n"
      "\t\tall messages in the same thread as any matched\n"
      "\t\tmessage will be displayed.\n"
      "\n"
      "\t--format=(text|json|mbox|raw)\n"
      "\n"
      "\t\ttext (default for messages)\n"
      "\n"
      "\t\tThe default plain-text format has all text-content MIME parts\n"
      "\t\tdecoded. Various components in the output, ('message', 'header',\n"
      "\t\t'body', 'attachment', and MIME 'part') are delimited by\n"
      "\t\teasily-parsed markers. Each marker consists of a Control-L\n"
      "\t\tcharacter (ASCII decimal 12), the name of the marker, and\n"
      "\t\tthen either an opening or closing brace, '{' or '}' to\n"
      "\t\teither open or close the component. For a multipart MIME\n"
      "\t\tmessage, these parts will be nested.\n"
      "\n"
      "\t\tjson\n"
      "\n"
      "\t\tThe output is formatted with Javascript Object Notation\n"
      "\t\t(JSON). This format is more robust than the text format\n"
      "\t\tfor automated processing. The nested structure of multipart\n"
      "\t\tMIME messages is reflected in nested JSON output. JSON\n"
      "\t\toutput always includes all messages in a matching thread;\n"
      "\t\tin effect '--format=json' implies '--entire-thread'\n"
      "\n"
      "\t\tmbox\n"
      "\n"
      "\t\tAll matching messages are output in the traditional, Unix\n"
      "\t\tmbox format with each message being prefixed by a line\n"
      "\t\tbeginning with 'From ' and a blank line separating each\n"
      "\t\tmessage. Lines in the message content beginning with 'From '\n"
      "\t\t(preceded by zero or more '>' characters) have an additional\n"
      "\t\t'>' character added. This reversible escaping is termed\n"
      "\t\t\"mboxrd\" format and described in detail here:\n"
      "\n"
      "\t\thttp://homepage.ntlworld.com/jonathan.deboynepollard/FGA/mail-mbox-formats.html\n"
      "\n"
      "\t\traw (default for a single part, see --part)\n"
      "\n"
      "\t\tFor a message, the original, raw content of the email\n"
      "\t\tmessage is output. Consumers of this format should\n"
      "\t\texpect to implement MIME decoding and similar functions.\n"
      "\n"
      "\t\tFor a single part (--part) the raw part content is output\n"
      "\t\tafter performing any necessary MIME decoding.\n"
      "\n"
      "\t\tThe raw format must only be used with search terms matching\n"
      "\t\tsingle message.\n"
      "\n"
      "\t--part=N\n"
      "\n"
      "\t\tOutput the single decoded MIME part N of a single message.\n"
      "\t\tThe search terms must match only a single message.\n"
      "\t\tMessage parts are numbered in a depth-first walk of the\n"
      "\t\tmessage MIME structure, and are identified in the 'json' or\n"
      "\t\t'text' output formats.\n"
      "\n"
      "\t--verify\n"
      "\n"
      "\t\tCompute and report the validity of any MIME cryptographic\n"
      "\t\tsignatures found in the selected content (ie.\n"
      "\t\t\"multipart/signed\" parts). Status of the signature will be\n"
      "\t\treported (currently only supported with --format=json) and\n"
      "\t\tthe multipart/signed part will be replaced by the signed data.\n"
      "\n"
      "\t--decrypt\n"
      "\n"
      "\t\tDecrypt any MIME encrypted parts found in the selected content\n"
      "\t\t(ie. \"multipart/encrypted\" parts). Status of the decryption\n"
      "\t\twill be reported (currently only supported with --format=json)\n"
      "\t\tand the multipart/encrypted part will be replaced by the\n"
      "\t\tdecrypted content.\n"
      "\n"
      "\n"
      "\tA common use of \"notmuch show\" is to display a single\n"
      "\tthread of email messages. For this, use a search term of\n"
      "\t\"thread:<thread-id>\" as can be seen in the first column\n"
      "\tof output from the \"notmuch search\" command.\n"
      "\n"
      "\tSee \"notmuch help search-terms\" for details of the search\n"
      "\tterms syntax." },
    { "count", notmuch_count_command,
      "<search-terms> [...]",
      "Count messages matching the search terms.",
      "\tThe number of matching messages is output to stdout.\n"
      "\n"
      "\tWith no search terms, a count of all messages in the database\n"
      "\twill be displayed.\n"
      "\n"
      "\tSee \"notmuch help search-terms\" for details of the search\n"
      "\tterms syntax." },
    { "reply", notmuch_reply_command,
      "[options...] <search-terms> [...]",
      "Construct a reply template for a set of messages.",
      "\tConstructs a new message as a reply to a set of existing\n"
      "\tmessages. The Reply-To: header (if any, otherwise From:) is\n"
      "\tused for the To: address. The To: and Cc: headers are copied,\n"
      "\tbut not including any of the user's configured addresses.\n"
      "\n"
      "\tA suitable subject is constructed. The In-Reply-to: and\n"
      "\tReferences: headers are set appropriately, and the content\n"
      "\tof the original messages is quoted and included in the body\n"
      "\t(unless --format=headers-only is given).\n"
      "\n"
      "\tThe resulting message template is output to stdout.\n"
      "\n"
      "\tSupported options for reply include:\n"
      "\n"
      "\t--format=(default|headers-only)\n"
      "\n"
      "\t\tdefault:\n"
      "\t\t\tIncludes subject and quoted message body.\n"
      "\n"
      "\t\theaders-only:\n"
      "\t\t\tOnly produces In-Reply-To, References, To\n"
      "\t\t\tCc, and Bcc headers.\n"
      "\n"
      "\tSee \"notmuch help search-terms\" for details of the search\n"
      "\tterms syntax." },
    { "tag", notmuch_tag_command,
      "+<tag>|-<tag> [...] [--] <search-terms> [...]",
      "Add/remove tags for all messages matching the search terms.",
      "\tThe search terms are handled exactly as in 'search' so one\n"
      "\tcan use that command first to see what will be modified.\n"
      "\n"
      "\tTags prefixed by '+' are added while those prefixed by\n"
      "\t'-' are removed. For each message, tag removal is performed\n"
      "\tbefore tag addition.\n"
      "\n"
      "\tThe beginning of <search-terms> is recognized by the first\n"
      "\targument that begins with neither '+' nor '-'. Support for\n"
      "\tan initial search term beginning with '+' or '-' is provided\n"
      "\tby allowing the user to specify a \"--\" argument to separate\n"
      "\tthe tags from the search terms.\n"
      "\n"
      "\tSee \"notmuch help search-terms\" for details of the search\n"
      "\tterms syntax." },
    { "dump", notmuch_dump_command,
      "[<filename>] [--] [<search-terms>]",
      "Create a plain-text dump of the tags for each message.",
      "\tOutput is to the given filename, if any, or to stdout.\n"
      "\tNote that using the filename argument is deprecated.\n"
      "\n"
      "\tThese tags are the only data in the notmuch database\n"
      "\tthat can't be recreated from the messages themselves.\n"
      "\tThe output of notmuch dump is therefore the only\n"
      "\tcritical thing to backup (and much more friendly to\n"
      "\tincremental backup than the native database files.)\n" 
      "\n"
      "\tWith no search terms, a dump of all messages in the\n"
      "\tdatabase will be generated. A \"--\" argument instructs\n"
      "\tnotmuch that the remaining arguments are search terms.\n"
      "\n"
      "\tSee \"notmuch help search-terms\" for the search-term syntax.\n"      
 },
    { "restore", notmuch_restore_command,
      "<filename>",
      "Restore the tags from the given dump file (see 'dump').",
      "\tNote: The dump file format is specifically chosen to be\n"
      "\tcompatible with the format of files produced by sup-dump.\n"
      "\tSo if you've previously been using sup for mail, then the\n"
      "\t\"notmuch restore\" command provides you a way to import\n"
      "\tall of your tags (or labels as sup calls them)." },
    { "config", notmuch_config_command,
      "[get|set] <section>.<item> [value ...]",
      "Get or set settings in the notmuch configuration file.",
      "    config get <section>.<item>\n"
      "\n"
      "\tThe value of the specified configuration item is printed\n"
      "\tto stdout. If the item has multiple values, each value\n"
      "\tis separated by a newline character.\n"
      "\n"
      "\tAvailable configuration items include at least\n"
      "\n"
      "\t\tdatabase.path\n"
      "\t\tuser.name\n"
      "\t\tuser.primary_email\n"
      "\t\tuser.other_email\n"
      "\t\tnew.tags\n"
      "\n"
      "    config set <section>.<item> [value ...]\n"
      "\n"
      "\tThe specified configuration item is set to the given value.\n"
      "\tTo specify a multiple-value item, provide each value as\n"
      "\ta separate command-line argument.\n"
      "\n"
      "\tIf no values are provided, the specified configuration item\n"
      "\twill be removed from the configuration file." },
    { "help", notmuch_help_command,
      "[<command>]",
      "This message, or more detailed help for the named command.",
      "\tExcept in this case, where there's not much more detailed\n"
      "\thelp available." }
};

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
		printf ("%s %s\n\n\t%s\n\n%s\n\n",
			command->name, command->arguments,
			command->summary, command->documentation);
	    else
		printf ("%s\t%s\n\n%s\n\n", command->name,
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
    alias_t *alias;
    unsigned int i, j;
    const char **argv_local;

    talloc_enable_null_tracking ();

    local = talloc_new (NULL);

    g_mime_init (0);
    g_type_init ();

    if (argc == 1)
	return notmuch (local);

    if (STRNCMP_LITERAL (argv[1], "--help") == 0)
	return notmuch_help_command (NULL, 0, NULL);

    if (STRNCMP_LITERAL (argv[1], "--version") == 0) {
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

	if (strcmp (argv[1], command->name) == 0)
	    return (command->function) (local, argc - 2, &argv[2]);
    }

    fprintf (stderr, "Error: Unknown command '%s' (see \"notmuch help\")\n",
	     argv[1]);

    talloc_free (local);

    return 1;
}
