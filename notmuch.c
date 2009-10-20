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

#include "notmuch.h"

#ifndef _GNU_SOURCE
#define _GNU_SOURCE /* for getline */
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>

#include <glib.h> /* g_strdup_printf */

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))

typedef int (*command_function_t) (int argc, char *argv[]);

typedef struct command {
    const char *name;
    command_function_t function;
    const char *usage;
} command_t;

typedef struct {
    int total_messages;
    int count;
    struct timeval tv_start;
} add_files_state_t;

/* Compute the number of seconds elapsed from start to end. */
double
tv_elapsed (struct timeval start, struct timeval end)
{
    return ((end.tv_sec - start.tv_sec) +
	    (end.tv_usec - start.tv_usec) / 1e6);
}

void
print_formatted_seconds (double seconds)
{
    int hours;
    int minutes;

    if (seconds > 3600) {
	hours = (int) seconds / 3600;
	printf ("%dh ", hours);
	seconds -= hours * 3600;
    }

    if (seconds > 60) {
	minutes = (int) seconds / 60;
	printf ("%dm ", minutes);
	seconds -= minutes * 60;
    }

    printf ("%02ds", (int) seconds);
}

void
add_files_print_progress (add_files_state_t *state)
{
    struct timeval tv_now;
    double elapsed_overall, rate_overall;

    gettimeofday (&tv_now, NULL);

    elapsed_overall = tv_elapsed (state->tv_start, tv_now);
    rate_overall = (state->count) / elapsed_overall;

    printf ("Added %d of %d messages (",
	    state->count, state->total_messages);
    print_formatted_seconds ((state->total_messages - state->count) /
			     rate_overall);
    printf (" remaining).\r");

    fflush (stdout);
}

/* Recursively find all regular files in 'path' and add them to the
 * database. */
void
add_files (notmuch_database_t *notmuch, const char *path,
	   add_files_state_t *state)
{
    DIR *dir;
    struct dirent *entry, *e;
    int entry_length;
    int err;
    char *next;
    struct stat st;
    notmuch_status_t status;

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

	/* Ignore special directories to avoid infinite recursion.
	 * Also ignore the .notmuch directory.
	 */
	/* XXX: Eventually we'll want more sophistication to let the
	 * user specify files to be ignored. */
	if (strcmp (entry->d_name, ".") == 0 ||
	    strcmp (entry->d_name, "..") == 0 ||
	    strcmp (entry->d_name, ".notmuch") ==0)
	{
	    continue;
	}

	next = g_strdup_printf ("%s/%s", path, entry->d_name);

	stat (next, &st);

	if (S_ISREG (st.st_mode)) {
	    status = notmuch_database_add_message (notmuch, next);
	    if (status == NOTMUCH_STATUS_FILE_NOT_EMAIL) {
		fprintf (stderr, "Note: Ignoring non-mail file: %s\n",
			 next);
	    } else {
		state->count++;
	    }
	    if (state->count % 1000 == 0)
		add_files_print_progress (state);
	} else if (S_ISDIR (st.st_mode)) {
	    add_files (notmuch, next, state);
	}

	free (next);
    }

    free (entry);

    closedir (dir);
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

	/* Ignore special directories to avoid infinite recursion.
	 * Also ignore the .notmuch directory.
	 */
	/* XXX: Eventually we'll want more sophistication to let the
	 * user specify files to be ignored. */
	if (strcmp (entry->d_name, ".") == 0 ||
	    strcmp (entry->d_name, "..") == 0 ||
	    strcmp (entry->d_name, ".notmuch") == 0)
	{
	    continue;
	}

	next = g_strdup_printf ("%s/%s", path, entry->d_name);

	stat (next, &st);

	if (S_ISREG (st.st_mode)) {
	    *count = *count + 1;
	    if (*count % 1000 == 0) {
		printf ("Found %d files so far.\r", *count);
		fflush (stdout);
	    }
	} else if (S_ISDIR (st.st_mode)) {
	    count_files (next, count);
	}

	free (next);
    }

    free (entry);

    closedir (dir);
}

int
setup_command (int argc, char *argv[])
{
    notmuch_database_t *notmuch;
    char *mail_directory, *default_path;
    size_t line_size;
    int count;
    add_files_state_t add_files_state;
    double elapsed;
    struct timeval tv_now;

    printf ("Welcome to notmuch!\n\n");

    printf ("The goal of notmuch is to help you manage and search your collection of\n"
	    "email, and to efficiently keep up with the flow of email as it comes in.\n\n");

    printf ("Notmuch needs to know the top-level directory of your email archive,\n"
	    "(where you already have mail stored and where messages will be delivered\n"
	    "in the future). This directory can contain any number of sub-directories\n"
	    "and primarily just files with indvidual email messages (eg. maildir or mh\n"
	    "archives are perfect). If there are other, non-email files (such as\n"
	    "indexes maintained by other email programs) then notmuch will do its\n"
	    "best to detect those and ignore them.\n\n");

    printf ("Mail storage that uses mbox format, (where one mbox file contains many\n"
	    "messages), will not work with notmuch. If that's how your mail is currently\n"
	    "stored, we recommend you first convert it to maildir format with a utility\n"
	    "such as mb2md. In that case, press Control-C now and run notmuch again\n"
	    "once the conversion is complete.\n\n");


    default_path = notmuch_database_default_path ();
    printf ("Top-level mail directory [%s]: ", default_path);
    fflush (stdout);

    mail_directory = NULL;
    getline (&mail_directory, &line_size, stdin);
    printf ("\n");

    if (mail_directory == NULL || strlen (mail_directory) == 0) {
	if (mail_directory)
	    free (mail_directory);
	mail_directory = default_path;
    } else {
	if (mail_directory[strlen(mail_directory)-1] == '\n')
	    mail_directory[strlen(mail_directory)-1] = '\0';
	/* XXX: Instead of telling the user to use an environment
	 * variable here, we should really be writing out a configuration
	 * file and loading that on the next run. */
	if (strcmp (mail_directory, default_path)) {
	    printf ("Note: Since you are not using the default path, you will want to set\n"
		    "the NOTMUCH_BASE environment variable to %s so that\n"
		    "future calls to notmuch commands will know where to find your mail.\n",
		    mail_directory);
	    printf ("For example, if you are using bash for your shell, add:\n\n");
	    printf ("\texport NOTMUCH_BASE=%s\n\n", mail_directory);
	    printf ("to your ~/.bashrc file.\n\n");
	}
	free (default_path);
    }

    notmuch = notmuch_database_create (mail_directory);
    if (notmuch == NULL) {
	fprintf (stderr, "Failed to create new notmuch database at %s\n",
		 mail_directory);
	free (mail_directory);
	return 1;
    }

    printf ("OK. Let's take a look at the mail we can find in the directory\n");
    printf ("%s ...\n", mail_directory);

    count = 0;
    count_files (mail_directory, &count);

    printf ("Found %d total files. That's not much mail.\n\n", count);

    printf ("Next, we'll inspect the messages and create a database of threads:\n");

    add_files_state.total_messages = count;
    add_files_state.count = 0;
    gettimeofday (&add_files_state.tv_start, NULL);

    add_files (notmuch, mail_directory, &add_files_state);

    gettimeofday (&tv_now, NULL);
    elapsed = tv_elapsed (add_files_state.tv_start,
			  tv_now);
    printf ("Added %d total messages in ", add_files_state.count);
    print_formatted_seconds (elapsed);
    printf (" (%d messages/sec.).                 \n", (int) (add_files_state.count / elapsed));

    notmuch_database_close (notmuch);

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
    fprintf (stderr, "Error: show is not implemented yet.\n");
    return 1;
}

int
dump_command (int argc, char *argv[])
{
    fprintf (stderr, "Error: dump is not implemented yet.\n");
    return 1;
}

int
restore_command (int argc, char *argv[])
{
    fprintf (stderr, "Error: restore is not implemented yet.\n");
    return 1;
}

command_t commands[] = {
    { "setup", setup_command,
      "Interactively setup notmuch for first use.\n"
      "\t\tInvoking notmuch with no command argument will run setup if\n"
      "\t\tthe setup command has not previously been completed." },
    { "search", search_command,
      "<search-term> [...]\n\n"
      "\t\tSearch for threads matching the given search terms.\n"
      "\t\tOnce we actually implement search we'll document the\n"
      "\t\tsyntax here." },
    { "show", show_command,
      "<thread-id>\n\n"
      "\t\tShow the thread with the given thread ID (see 'search')." },
    { "dump", dump_command,
      "[<filename>]\n\n"
      "\t\tCreate a plain-text dump of the tags for each message\n"
      "\t\twriting to the given filename, if any, or to stdout.\n"
      "\t\tThese tags are the only data in the notmuch database\n"
      "\t\tthat can't be recreated from the messages themselves.\n"
      "\t\tThe output of notmuch dump is therefore the only\n"
      "\t\tcritical thing to backup (and much more friendly to\n"
      "\t\tincremental backup than the native database files." },
    { "restore", restore_command,
      "<filename>\n\n"
      "\t\tRestore the tags from the given dump file (see 'dump')." }
};

void
usage (void)
{
    command_t *command;
    int i;

    fprintf (stderr, "Usage: notmuch <command> [args...]\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Where <command> and [args...] are as follows:\n");
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

    /* Don't complain about "help" being an unknown command when we're
       about to provide exactly what's wanted anyway. */
    if (strcmp (argv[1], "help") == 0 ||
	strcmp (argv[1], "--help") == 0)
    {
	fprintf (stderr, "The notmuch mail system.\n\n");
    } else {
	fprintf (stderr, "Error: Unknown command '%s'\n\n", argv[1]);
    }
    usage ();
    exit (1);

    return 0;
}
