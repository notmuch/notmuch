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

static int
notmuch_help_command (void *ctx, int argc, char *argv[]);

command_t commands[] = {
    { "setup", notmuch_setup_command,
      "Interactively setup notmuch for first use.",
      "\t\tThe setup command is the first command you will run in order\n"
      "\t\tto start using notmuch. It will prompt you for the directory\n"
      "\t\tcontaining your email archives, and will then proceed to build\n"
      "\t\ta database to allow fast searching of that mail.\n\n"
      "\t\tInvoking notmuch with no command argument will run setup if\n"
      "\t\tthe setup command has not previously been completed." },
    { "new", notmuch_new_command,
      "Find and import any new messages.",
      "\t\tScans all sub-directories of the database, adding new messages\n"
      "\t\tthat are found. Each new message will be tagged as both\n"
      "\t\t\"inbox\" and \"unread\".\n"
      "\n"
      "\t\tNote: \"notmuch new\" will skip any read-only directories,\n"
      "\t\tso you can use that to mark directories that will not\n"
      "\t\treceive any new mail (and make \"notmuch new\" faster)." },
    { "search", notmuch_search_command,
      "<search-term> [...]\n\n"
      "\t\tSearch for threads matching the given search terms.",
      "\t\tNote that the individual mail messages will be matched\n"
      "\t\tagainst the search terms, but the results will be the\n"
      "\t\tthreads containing the matched messages.\n\n"
      "\t\tCurrently, in addition to free text (and quoted phrases)\n"
      "\t\twhich match terms appearing anywhere within an email,\n"
      "\t\tthe following prefixes can be used to search specific\n"
      "\t\tportions of an email, (where <brackets> indicate user-\n"
      "\t\tsupplied values):\n\n"
      "\t\t\tfrom:<name-or-address>\n"
      "\t\t\tto:<name-or-address>\n"
      "\t\t\tsubject:<word-or-quoted-phrase>\n"
      "\t\t\ttag:<tag>\n"
      "\t\t\tid:<message-id>\n"
      "\t\t\tthread:<thread-id>\n\n"
      "\t\tThe from: prefix is used to match the name or address of\n"
      "\t\tthe sender of an email message.\n\n"
      "\t\tThe to: prefix is used to match the names or addresses of\n"
      "\t\tany recipient of an email message, (whether To, Cc, or Bcc).\n\n"
      "\t\tAny term prefixed with subject: will match only text from\n"
      "\t\tthe subject of an email. Quoted phrases are supported when\n"
      "\t\tsearching with: subject:\"this is a phrase\".\n\n"
      "\t\tValid tag values include \"inbox\" and \"unread\" by default\n"
      "\t\tfor new messages added by \"notmuch new\" as well as any other\n"
      "\t\ttag values added manually with \"notmuch tag\".\n\n"
      "\t\tMessage ID values are the literal contents of the Message-ID:\n"
      "\t\theader of email messages, but without the '<','>' delimiters.\n\n"
      "\t\tThread ID values are generated internally by notmuch but can\n"
      "\t\tbe seen in the output of \"notmuch search\" for example.\n\n"
      "\t\tIn addition to individual terms, multiple terms can be\n"
      "\t\tcombined with Boolean operators (\"and\", \"or\", \"not\", etc.).\n"
      "\t\tEach term in the query will be implicitly connected by a\n"
      "\t\tlogical AND if no explicit operator is provided, (except\n"
      "\t\tthat terms with a common prefix will be implicitly combined\n"
      "\t\twith OR until we get Xapian defect #402 fixed).\n\n"
      "\t\tParentheses can also be used to control the combination of\n"
      "\t\tthe Boolean operators, but will have to be protected from\n"
      "\t\tinterpretation by the shell, (such as by putting quotation\n"
      "\t\tmarks around any parenthesized expression)." },
    { "reply", notmuch_reply_command,
      "<search-terms> [...]\n\n"
      "\t\tFormats a reply from a set of existing messages.",
      "\t\tConstructs a new message as a reply to a set of existing\n"
      "\t\tmessages. The From: address is used as a To: address\n"
      "\t\talong with all old To: addresses. All of the Cc: addresses\n"
      "\t\tare copied as new Cc: addresses. An In-Reply-To: header\n"
      "\t\twill be constructed from the name and date of the original\n"
      "\t\tmessage, and the original Message-ID will be added to the\n"
      "\t\tlist of References in the new message. The text of each\n"
      "\t\tmessage (as described in the \"show\" command) will be\n"
      "\t\tpresented, each line prefixed with \"> \" The resulting\n"
      "\t\tmessage will be dumped to stdout." },
    { "show", notmuch_show_command,
      "<search-terms> [...]\n\n"
      "\t\tShows all messages matching the search terms.",
      "\t\tSee the documentation of \"notmuch search\" for details\n"
      "\t\tof the supported syntax of search terms.\n\n"
      "\t\tA common use of \"notmuch show\" is to display a single\n"
      "\t\tthread of email messages. For this, use a search term of\n"
      "\t\t\"thread:<thread-id>\" as can be seen in the first column\n"
      "\t\tof output from the \"notmuch search\" command.\n\n"
      "\t\tAll messages will be displayed in date order. The output\n"
      "\t\tformat is plain-text, with all text-content MIME parts\n"
      "\t\tdecoded. Various components in the output, ('message',\n"
      "\t\t'header', 'body', 'attachment', and MIME 'part') will be\n"
      "\t\tdelimited by easily-parsed markers. Each marker consists\n"
      "\t\tof a Control-L character (ASCII decimal 12), the name of\n"
      "\t\tthe marker, and then either an opening or closing brace,\n"
      "\t\t'{' or '}' to either open or close the component."},
    { "tag", notmuch_tag_command,
      "+<tag>|-<tag> [...] [--] <search-term> [...]\n\n"
      "\t\tAdd/remove tags for all messages matching the search terms.",
      "\t\tThe search terms are handled exactly as in 'search' so one\n"
      "\t\tcan use that command first to see what will be modified.\n\n"
      "\t\tTags prefixed by '+' are added while those prefixed by '-' are\n"
      "\t\tremoved. For each message, tag removal is before tag addition.\n\n"
      "\t\tThe beginning of <search-terms> is recognized by the first\n"
      "\t\targument that begins with neither '+' nor '-'. Support for\n"
      "\t\tan initial search term beginning with '+' or '-' is provided\n"
      "\t\tby allowing the user to specify a \"--\" argument to separate\n"
      "\t\tthe tags from the search terms.\n\n"
      "\t\tNote: If you run \"notmuch new\" between reading a thread with\n"
      "\t\t\"notmuch show\" and removing the \"inbox\" tag for that thread\n"
      "\t\twith \"notmuch tag\" then you create the possibility of moving\n"
      "\t\tsome messages from that thread out of your inbox without ever\n"
      "\t\treading them. The easiest way to avoid this problem is to not\n"
      "\t\trun \"notmuch new\" between reading and removing tags." },
    { "dump", notmuch_dump_command,
      "[<filename>]\n\n"
      "\t\tCreate a plain-text dump of the tags for each message.",
      "\t\tOutput is to the given filename, if any, or to stdout.\n"
      "\t\tThese tags are the only data in the notmuch database\n"
      "\t\tthat can't be recreated from the messages themselves.\n"
      "\t\tThe output of notmuch dump is therefore the only\n"
      "\t\tcritical thing to backup (and much more friendly to\n"
      "\t\tincremental backup than the native database files.)" },
    { "restore", notmuch_restore_command,
      "<filename>\n\n"
      "\t\tRestore the tags from the given dump file (see 'dump').",
      "\t\tNote: The dump file format is specifically chosen to be\n"
      "\t\tcompatible with the format of files produced by sup-dump.\n"
      "\t\tSo if you've previously been using sup for mail, then the\n"
      "\t\t\"notmuch restore\" command provides you a way to import\n"
      "\t\tall of your tags (or labels as sup calls them)." },
    { "help", notmuch_help_command,
      "[<command>]\n\n"
      "\t\tThis message, or more detailed help for the named command.",
      "\t\tExcept in this case, where there's not much more detailed\n"
      "\t\thelp available." }
};

static void
usage (void)
{
    command_t *command;
    unsigned int i;

    fprintf (stderr, "Usage: notmuch <command> [args...]\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Where <command> and [args...] are as follows:\n");
    fprintf (stderr, "\n");

    for (i = 0; i < ARRAY_SIZE (commands); i++) {
	command = &commands[i];

	fprintf (stderr, "\t%s\t%s\n\n", command->name, command->summary);
    }

    fprintf (stderr, "Use \"notmuch help <command>\" for more details on each command.\n\n");
}

static int
notmuch_help_command (unused (void *ctx), int argc, char *argv[])
{
    command_t *command;
    unsigned int i;

    if (argc == 0) {
	fprintf (stderr, "The notmuch mail system.\n\n");
	usage ();
	return 0;
    }

    for (i = 0; i < ARRAY_SIZE (commands); i++) {
	command = &commands[i];

	if (strcmp (argv[0], command->name) == 0) {
	    fprintf (stderr, "Help for \"notmuch %s\":\n\n", argv[0]);
	    fprintf (stderr, "\t%s\t%s\n\n%s\n\n", command->name,
		     command->summary, command->documentation);
	    return 0;
	}
    }

    fprintf (stderr,
	     "\nSorry, %s is not a known command. There's not much I can do to help.\n\n",
	     argv[0]);
    return 1;
}
    
int
main (int argc, char *argv[])
{
    void *local = talloc_new (NULL);
    command_t *command;
    unsigned int i;

    if (argc == 1)
	return notmuch_setup_command (local, 0, NULL);

    for (i = 0; i < ARRAY_SIZE (commands); i++) {
	command = &commands[i];

	if (strcmp (argv[1], command->name) == 0)
	    return (command->function) (local, argc - 2, &argv[2]);
    }

    /* Don't complain about "help" being an unknown command when we're
       about to provide exactly what's wanted anyway. */
    fprintf (stderr, "Error: Unknown command '%s' (see \"notmuch help\")\n",
	     argv[1]);

    talloc_free (local);

    return 1;
}
