/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2009 Carl Worth
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
 * Author: Carl Worth <cworth@cworth.org>
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>

#include <glib.h>

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))

typedef int (*command_function_t) (int argc, char *argv[]);

typedef struct command {
    const char *name;
    command_function_t function;
    const char *usage;
} command_t;

/* Read a line from stdin, without any line-terminator character.  The
 * return value is a newly allocated string. The caller should free()
 * the string when finished with it.
 *
 * This function returns NULL if EOF is encountered before any
 * characters are input (otherwise it returns those characters).
 */
char *
read_line (void)
{
    char *result = NULL;
    GError *error = NULL;
    GIOStatus g_io_status;
    gsize length;

    GIOChannel *channel = g_io_channel_unix_new (fileno (stdin));

    g_io_status = g_io_channel_read_line (channel, &result,
					  &length, NULL, &error);

    if (g_io_status == EOF)
	goto DONE;

    if (g_io_status != G_IO_STATUS_NORMAL) {
	fprintf(stderr, "Read error: %s\n", error->message);
	exit (1);
    }

    if (length && result[length - 1] == '\n')
	result[length - 1] = '\0';

  DONE:
    g_io_channel_unref (channel);
    return result;
}

/* Recursively count all regular files in path and all sub-direcotries
 * of path.  The result is added to *count (which should be
 * initialized to zero by the top-level caller before calling
 * count_files). */
void
count_files (const char *path, int *count)
{
    DIR *dir;
    struct dirent *entry, *e;
    int entry_length;
    int err;
    char *next;
    struct stat st;

    dir = opendir (path);

    if (dir == NULL) {
	fprintf (stderr, "Warning: failed to open directory %s: %s\n",
		 path, strerror (errno));
	return;
    }

    entry_length = offsetof (struct dirent, d_name) +
	pathconf (path, _PC_NAME_MAX) + 1;
    entry = malloc (entry_length);

    while (1) {
	err = readdir_r (dir, entry, &e);
	if (err) {
	    fprintf (stderr, "Error reading directory: %s\n",
		     strerror (errno));
	    free (entry);
	    return;
	}

	if (e == NULL)
	    break;

	/* Skip these special directories to avoid infinite recursion. */
	if (strcmp (entry->d_name, ".") == 0 ||
	    strcmp (entry->d_name, "..") == 0)
	{
	    continue;
	}

	next = g_strdup_printf ("%s/%s", path, entry->d_name);

	stat (next, &st);

	if (S_ISREG (st.st_mode))
	    *count = *count + 1;
	else if (S_ISDIR (st.st_mode))
	    count_files (next, count);

	if (*count % 1000 == 0) {
	    printf ("Found %d files so far.\r", *count);
	    fflush (stdout);
	}

	free (next);
    }

    free (entry);

    closedir (dir);
}

int
setup_command (int argc, char *argv[])
{
    char *mail_directory;
    int count;

    printf ("Welcome to notmuch!\n\n");

    printf ("The goal of notmuch is to help you manage and search your collection of\n"
	    "email, and to efficiently keep up with the flow of email as it comes in.\n\n");

    printf ("Notmuch needs to know the top-level directory of your email archive,\n"
	    "(where you already have mail stored and where messages will be delivered\n"
	    "in the future). This directory can contain any number of sub-directories\n"
	    "but the only files it contains should be individual email messages.\n"
	    "Either maildir or mh format directories are fine, but you will want to\n"
	    "move away any auxiliary files maintained by other email programs.\n\n");

    printf ("Mail storage that uses mbox format, (where one mbox file contains many\n"
	    "messages), will not work with notmuch. If that's how your mail is currently\n"
	    "stored, we recommend you first convert it to maildir format with a utility\n"
	    "such as mb2md. In that case, press Control-C now and run notmuch again\n"
	    "once the conversion is complete.\n\n");

    printf ("Top-level mail directory [~/mail]: ");
    fflush (stdout);

    mail_directory = read_line ();

    if (mail_directory == NULL || strlen (mail_directory) == 0) {
	char *home;

	if (mail_directory)
	    free (mail_directory);

	home = getenv ("HOME");
	if (!home) {
	    fprintf (stderr, "Error: No mail directory provided HOME environment variable is not set.\n");
	    fprintf (stderr, "Cowardly refusing to just guess where your mail might be.\n");
	    exit (1);
	}

	mail_directory = g_strdup_printf ("%s/mail", home);
    }

    printf ("OK. Let's take a look at the mail we can find in the directory\n");
    printf ("%s ...\n", mail_directory);

    count = 0;
    count_files (mail_directory, &count);

    printf ("Found %d total files. That's not much mail.\n", count);

    free (mail_directory);
    
    return 0;
}

int
search_command (int argc, char *argv[])
{
    fprintf (stderr, "Error: search is not implemented yet.\n");
    return 1;
}

int
show_command (int argc, char *argv[])
{
    fprintf (stderr, "Error: show-thread is not implemented yet.\n");
    return 1;
}

command_t commands[] = {
    { "setup", setup_command,
      "Interactively setup notmuch for first use (no arguments).\n"
      "\t\tInvoking notmuch with no command argument will run setup if\n"
      "\t\the setup command has not previously been completed." },
    { "search", search_command,
      "Search for threads matching the given search terms." },
    { "show", show_command,
      "Show the thread with the given thread ID (see 'search')." }
};

void
usage (void)
{
    command_t *command;
    int i;

    fprintf (stderr, "Usage: notmuch <command> [args...]\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Where <command> is one of the following:\n");
    fprintf (stderr, "\n");

    for (i = 0; i < ARRAY_SIZE (commands); i++) {
	command = &commands[i];

	fprintf (stderr, "\t%s\t%s\n\n", command->name, command->usage);
    }
}
    
int
main (int argc, char *argv[])
{
    command_t *command;
    int i;

    if (argc == 1)
	return setup_command (0, NULL);

    for (i = 0; i < ARRAY_SIZE (commands); i++) {
	command = &commands[i];

	if (strcmp (argv[1], command->name) == 0)
	    return (command->function) (argc - 2, &argv[2]);
    }

    fprintf (stderr, "Error: Unknown command '%s'\n\n", argv[1]);
    usage ();
    exit (1);

    return 0;
}
