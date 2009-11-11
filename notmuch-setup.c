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

#include "notmuch-client.h"

static notmuch_status_t
add_all_files (notmuch_database_t *notmuch,
	       const char *mail_directory,
	       int num_files)
{
    add_files_state_t add_files_state;
    double elapsed;
    struct timeval tv_now;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;

    add_files_state.ignore_read_only_directories = FALSE;
    add_files_state.saw_read_only_directory = FALSE;
    add_files_state.total_files = num_files;
    add_files_state.processed_files = 0;
    add_files_state.added_messages = 0;
    add_files_state.callback = NULL;
    gettimeofday (&add_files_state.tv_start, NULL);

    ret = add_files (notmuch, mail_directory, &add_files_state);

    gettimeofday (&tv_now, NULL);
    elapsed = notmuch_time_elapsed (add_files_state.tv_start,
				    tv_now);
    printf ("Processed %d %s in ", add_files_state.processed_files,
	    add_files_state.processed_files == 1 ?
	    "file" : "total files");
    notmuch_time_print_formatted_seconds (elapsed);
    if (elapsed > 1) {
	printf (" (%d files/sec.).                 \n",
		(int) (add_files_state.processed_files / elapsed));
    } else {
	printf (".                    \n");
    }
    if (add_files_state.added_messages) {
	printf ("Added %d %s to the database.\n\n",
		add_files_state.added_messages,
		add_files_state.added_messages == 1 ?
		"message" : "unique messages");
    }

    return ret;
}


/* XXX: This should be merged with the existing add_files function in
 * add-files.c. */
/* Recursively count all regular files in path and all sub-direcotries
 * of path.  The result is added to *count (which should be
 * initialized to zero by the top-level caller before calling
 * count_files). */
static void
count_files (const char *path, int *count)
{
    DIR *dir;
    struct dirent *e, *entry = NULL;
    int entry_length;
    int err;
    char *next;
    struct stat st;

    dir = opendir (path);

    if (dir == NULL) {
	fprintf (stderr, "Warning: failed to open directory %s: %s\n",
		 path, strerror (errno));
	goto DONE;
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
	    goto DONE;
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

	if (asprintf (&next, "%s/%s", path, entry->d_name) == -1) {
	    next = NULL;
	    fprintf (stderr, "Error descending from %s to %s: Out of memory\n",
		     path, entry->d_name);
	    continue;
	}

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

  DONE:
    if (entry)
	free (entry);

    closedir (dir);
}

static void
welcome_message (void)
{
    printf (
"Welcome to notmuch!\n\n"
"The goal of notmuch is to help you manage and search your collection of\n"
"email, and to efficiently keep up with the flow of email as it comes in.\n\n"
"Notmuch needs to know the top-level directory of your email archive,\n"
"(where you already have mail stored and where messages will be delivered\n"
"in the future). This directory can contain any number of sub-directories\n"
"and primarily just files with indvidual email messages (eg. maildir or mh\n"
"archives are perfect). If there are other, non-email files (such as\n"
"indexes maintained by other email programs) then notmuch will do its\n"
"best to detect those and ignore them.\n\n"
"Mail storage that uses mbox format, (where one mbox file contains many\n"
"messages), will not work with notmuch. If that's how your mail is currently\n"
"stored, we recommend you first convert it to maildir format with a utility\n"
"such as mb2md. In that case, press Control-C now and run notmuch again\n"
"once the conversion is complete.\n\n");
}

static char *
prompt_user_for_mail_directory ()
{
    char *default_path, *mail_directory = NULL;
    size_t line_size;

    default_path = notmuch_database_default_path ();
    printf ("Top-level mail directory [%s]: ", default_path);
    fflush (stdout);

    getline (&mail_directory, &line_size, stdin);
    chomp_newline (mail_directory);

    printf ("\n");

    if (mail_directory == NULL || strlen (mail_directory) == 0) {
	if (mail_directory)
	    free (mail_directory);
	mail_directory = default_path;
    } else {
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

    /* Coerce the directory into an absolute directory name. */
    if (*mail_directory != '/') {
	char *cwd, *absolute_mail_directory;

	cwd = getcwd (NULL, 0);
	if (cwd == NULL) {
	    fprintf (stderr, "Out of memory.\n");
	    exit (1);
	}

	if (asprintf (&absolute_mail_directory, "%s/%s",
		      cwd, mail_directory) < 0)
	{
	    fprintf (stderr, "Out of memory.\n");
	    exit (1);
	}

	free (cwd);
	free (mail_directory);
	mail_directory = absolute_mail_directory;
    }

    return mail_directory;
}

int
notmuch_setup_command (unused (void *ctx),
		       unused (int argc), unused (char *argv[]))
{
    notmuch_database_t *notmuch = NULL;
    char *mail_directory = NULL;
    int count;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;

    welcome_message ();

    mail_directory = prompt_user_for_mail_directory ();

    notmuch = notmuch_database_create (mail_directory);
    if (notmuch == NULL) {
	fprintf (stderr, "Failed to create new notmuch database at %s\n",
		 mail_directory);
	ret = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    printf ("OK. Let's take a look at the mail we can find in the directory\n");
    printf ("%s ...\n", mail_directory);

    count = 0;
    count_files (mail_directory, &count);

    printf ("Found %d total files. That's not much mail.\n\n", count);

    printf ("Next, we'll inspect the messages and create a database of threads:\n");

    ret = add_all_files (notmuch, mail_directory, count);

    printf ("When new mail is delivered to %s in the future,\n"
	    "run \"notmuch new\" to add it to the database.\n\n",
	    mail_directory);

    if (ret) {
	printf ("Note: At least one error was encountered: %s\n",
		notmuch_status_to_string (ret));
    }

  DONE:
    if (mail_directory)
	free (mail_directory);
    if (notmuch)
	notmuch_database_close (notmuch);

    return ret;
}
