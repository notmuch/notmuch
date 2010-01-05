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

#include <unistd.h>

typedef void (*add_files_callback_t) (notmuch_message_t *message);

typedef struct {
    int output_is_a_tty;
    int verbose;

    int total_files;
    int processed_files;
    int added_messages;
    struct timeval tv_start;

    add_files_callback_t callback;
} add_files_state_t;

static volatile sig_atomic_t do_add_files_print_progress = 0;

static void
handle_sigalrm (unused (int signal))
{
    do_add_files_print_progress = 1;
}

static volatile sig_atomic_t interrupted;

static void
handle_sigint (unused (int sig))
{
    ssize_t ignored;
    static char msg[] = "Stopping...         \n";

    ignored = write(2, msg, sizeof(msg)-1);
    interrupted = 1;
}

static void
tag_inbox_and_unread (notmuch_message_t *message)
{
    notmuch_message_add_tag (message, "inbox");
    notmuch_message_add_tag (message, "unread");
}

static void
add_files_print_progress (add_files_state_t *state)
{
    struct timeval tv_now;
    double elapsed_overall, rate_overall;

    gettimeofday (&tv_now, NULL);

    elapsed_overall = notmuch_time_elapsed (state->tv_start, tv_now);
    rate_overall = (state->processed_files) / elapsed_overall;

    printf ("Processed %d", state->processed_files);

    if (state->total_files) {
	double time_remaining;

	time_remaining = ((state->total_files - state->processed_files) /
			  rate_overall);
	printf (" of %d files (", state->total_files);
	notmuch_time_print_formatted_seconds (time_remaining);
	printf (" remaining).      \r");
    } else {
	printf (" files (%d files/sec.)    \r", (int) rate_overall);
    }

    fflush (stdout);
}

static int ino_cmp(const struct dirent **a, const struct dirent **b)
{
    return ((*a)->d_ino < (*b)->d_ino) ? -1 : 1;
}

/* Test if the directory looks like a Maildir directory.
 *
 * Search through the array of directory entries to see if we can find all
 * three subdirectories typical for Maildir, that is "new", "cur", and "tmp".
 *
 * Return 1 if the directory looks like a Maildir and 0 otherwise.
 */
static int
is_maildir (struct dirent **entries, int count)
{
    int i, found = 0;

    for (i = 0; i < count; i++) {
	if (entries[i]->d_type != DT_DIR) continue;
	if (strcmp(entries[i]->d_name, "new") == 0 ||
	    strcmp(entries[i]->d_name, "cur") == 0 ||
	    strcmp(entries[i]->d_name, "tmp") == 0)
	{
	    found++;
	    if (found == 3)
		return 1;
	}
    }

    return 0;
}

/* Examine 'path' recursively as follows:
 *
 *   o Ask the filesystem for the mtime of 'path' (path_mtime)
 *
 *   o Ask the database for its timestamp of 'path' (path_dbtime)
 *
 *   o If 'path_mtime' > 'path_dbtime'
 *
 *       o For each regular file in 'path' with mtime newer than the
 *         'path_dbtime' call add_message to add the file to the
 *         database.
 *
 *       o For each sub-directory of path, recursively call into this
 *         same function.
 *
 *   o Tell the database to update its time of 'path' to 'path_mtime'
 *
 * The 'struct stat *st' must point to a structure that has already
 * been initialized for 'path' by calling stat().
 */
static notmuch_status_t
add_files_recursive (notmuch_database_t *notmuch,
		     const char *path,
		     struct stat *st,
		     add_files_state_t *state)
{
    DIR *dir = NULL;
    struct dirent *entry = NULL;
    char *next = NULL;
    time_t path_mtime, path_dbtime;
    notmuch_status_t status, ret = NOTMUCH_STATUS_SUCCESS;
    notmuch_message_t *message = NULL;
    struct dirent **namelist = NULL;
    int num_entries;
    notmuch_directory_t *directory;

    path_mtime = st->st_mtime;

    directory = notmuch_database_get_directory (notmuch, path);
    path_dbtime = notmuch_directory_get_mtime (directory);

    num_entries = scandir (path, &namelist, 0, ino_cmp);

    if (num_entries == -1) {
	fprintf (stderr, "Error opening directory %s: %s\n",
		 path, strerror (errno));
	ret = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    int i=0;

    while (!interrupted) {
	if (i == num_entries)
	    break;

        entry= namelist[i++];

	/* If this directory hasn't been modified since the last
	 * add_files, then we only need to look further for
	 * sub-directories. */
	if (path_mtime <= path_dbtime && entry->d_type == DT_REG)
	    continue;

	/* Ignore special directories to avoid infinite recursion.
	 * Also ignore the .notmuch directory.
	 */
	/* XXX: Eventually we'll want more sophistication to let the
	 * user specify files to be ignored. */
	if (strcmp (entry->d_name, ".") == 0 ||
	    strcmp (entry->d_name, "..") == 0 ||
	    (entry->d_type == DT_DIR &&
	     (strcmp (entry->d_name, "tmp") == 0) &&
	     is_maildir (namelist, num_entries)) ||
	    strcmp (entry->d_name, ".notmuch") ==0)
	{
	    continue;
	}

	next = talloc_asprintf (notmuch, "%s/%s", path, entry->d_name);

	if (stat (next, st)) {
	    int err = errno;

	    switch (err) {
	    case ENOENT:
		/* The file was removed between scandir and now... */
	    case EPERM:
	    case EACCES:
		/* We can't read this file so don't add it to the cache. */
		continue;
	    }

	    fprintf (stderr, "Error reading %s: %s\n",
		     next, strerror (errno));
	    ret = NOTMUCH_STATUS_FILE_ERROR;
	    goto DONE;
	}

	if (S_ISREG (st->st_mode)) {
	    /* If the file hasn't been modified since the last
	     * add_files, then we need not look at it. */
	    if (path_dbtime == 0 || st->st_mtime > path_dbtime) {
		state->processed_files++;

		if (state->verbose) {
		    if (state->output_is_a_tty)
			printf("\r\033[K");

		    printf ("%i/%i: %s",
			    state->processed_files,
			    state->total_files,
			    next);

		    putchar((state->output_is_a_tty) ? '\r' : '\n');
		    fflush (stdout);
		}

		status = notmuch_database_add_message (notmuch, next, &message);
		switch (status) {
		    /* success */
		    case NOTMUCH_STATUS_SUCCESS:
			state->added_messages++;
			tag_inbox_and_unread (message);
			break;
		    /* Non-fatal issues (go on to next file) */
		    case NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID:
		        /* Stay silent on this one. */
			break;
		    case NOTMUCH_STATUS_FILE_NOT_EMAIL:
			fprintf (stderr, "Note: Ignoring non-mail file: %s\n",
				 next);
			break;
		    /* Fatal issues. Don't process anymore. */
		    case NOTMUCH_STATUS_READONLY_DATABASE:
		    case NOTMUCH_STATUS_XAPIAN_EXCEPTION:
		    case NOTMUCH_STATUS_OUT_OF_MEMORY:
			fprintf (stderr, "Error: %s. Halting processing.\n",
				 notmuch_status_to_string (status));
			ret = status;
			goto DONE;
		    default:
		    case NOTMUCH_STATUS_FILE_ERROR:
		    case NOTMUCH_STATUS_NULL_POINTER:
		    case NOTMUCH_STATUS_TAG_TOO_LONG:
		    case NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW:
		    case NOTMUCH_STATUS_LAST_STATUS:
			INTERNAL_ERROR ("add_message returned unexpected value: %d",  status);
			goto DONE;
		}

		if (message) {
		    notmuch_message_destroy (message);
		    message = NULL;
		}

		if (do_add_files_print_progress) {
		    do_add_files_print_progress = 0;
		    add_files_print_progress (state);
		}
	    }
	} else if (S_ISDIR (st->st_mode)) {
	    status = add_files_recursive (notmuch, next, st, state);
	    if (status && ret == NOTMUCH_STATUS_SUCCESS)
		ret = status;
	}

	talloc_free (next);
	next = NULL;
    }

    status = notmuch_directory_set_mtime (directory, path_mtime);
    if (status && ret == NOTMUCH_STATUS_SUCCESS)
	ret = status;

  DONE:
    if (next)
	talloc_free (next);
    if (entry)
	free (entry);
    if (dir)
	closedir (dir);
    if (namelist)
	free (namelist);

    return ret;
}

/* This is the top-level entry point for add_files. It does a couple
 * of error checks, sets up the progress-printing timer and then calls
 * into the recursive function. */
static notmuch_status_t
add_files (notmuch_database_t *notmuch,
	   const char *path,
	   add_files_state_t *state)
{
    struct stat st;
    notmuch_status_t status;
    struct sigaction action;
    struct itimerval timerval;
    notmuch_bool_t timer_is_active = FALSE;

    if (stat (path, &st)) {
	fprintf (stderr, "Error reading directory %s: %s\n",
		 path, strerror (errno));
	return NOTMUCH_STATUS_FILE_ERROR;
    }

    if (! S_ISDIR (st.st_mode)) {
	fprintf (stderr, "Error: %s is not a directory.\n", path);
	return NOTMUCH_STATUS_FILE_ERROR;
    }

    if (state->output_is_a_tty && ! debugger_is_active () && ! state->verbose) {
	/* Setup our handler for SIGALRM */
	memset (&action, 0, sizeof (struct sigaction));
	action.sa_handler = handle_sigalrm;
	sigemptyset (&action.sa_mask);
	action.sa_flags = SA_RESTART;
	sigaction (SIGALRM, &action, NULL);

	/* Then start a timer to send SIGALRM once per second. */
	timerval.it_interval.tv_sec = 1;
	timerval.it_interval.tv_usec = 0;
	timerval.it_value.tv_sec = 1;
	timerval.it_value.tv_usec = 0;
	setitimer (ITIMER_REAL, &timerval, NULL);

	timer_is_active = TRUE;
    }

    status = add_files_recursive (notmuch, path, &st, state);

    if (timer_is_active) {
	/* Now stop the timer. */
	timerval.it_interval.tv_sec = 0;
	timerval.it_interval.tv_usec = 0;
	timerval.it_value.tv_sec = 0;
	timerval.it_value.tv_usec = 0;
	setitimer (ITIMER_REAL, &timerval, NULL);

	/* And disable the signal handler. */
	action.sa_handler = SIG_IGN;
	sigaction (SIGALRM, &action, NULL);
    }

    return status;
}

/* XXX: This should be merged with the add_files function since it
 * shares a lot of logic with it. */
/* Recursively count all regular files in path and all sub-directories
 * of path.  The result is added to *count (which should be
 * initialized to zero by the top-level caller before calling
 * count_files). */
static void
count_files (const char *path, int *count)
{
    struct dirent *entry = NULL;
    char *next;
    struct stat st;
    struct dirent **namelist = NULL;
    int n_entries = scandir (path, &namelist, 0, ino_cmp);
    int i = 0;

    if (n_entries == -1) {
	fprintf (stderr, "Warning: failed to open directory %s: %s\n",
		 path, strerror (errno));
	goto DONE;
    }

    while (!interrupted) {
        if (i == n_entries)
	    break;

        entry= namelist[i++];

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
    if (namelist)
        free (namelist);
}

int
notmuch_new_command (void *ctx, int argc, char *argv[])
{
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    add_files_state_t add_files_state;
    double elapsed;
    struct timeval tv_now;
    int ret = 0;
    struct stat st;
    const char *db_path;
    char *dot_notmuch_path;
    struct sigaction action;
    int i;

    add_files_state.verbose = 0;
    add_files_state.output_is_a_tty = isatty (fileno (stdout));

    for (i = 0; i < argc && argv[i][0] == '-'; i++) {
	if (STRNCMP_LITERAL (argv[i], "--verbose") == 0) {
	    add_files_state.verbose = 1;
	} else {
	    fprintf (stderr, "Unrecognized option: %s\n", argv[i]);
	    return 1;
	}
    }

    /* Setup our handler for SIGINT */
    memset (&action, 0, sizeof (struct sigaction));
    action.sa_handler = handle_sigint;
    sigemptyset (&action.sa_mask);
    action.sa_flags = SA_RESTART;
    sigaction (SIGINT, &action, NULL);

    config = notmuch_config_open (ctx, NULL, NULL);
    if (config == NULL)
	return 1;

    db_path = notmuch_config_get_database_path (config);

    dot_notmuch_path = talloc_asprintf (ctx, "%s/%s", db_path, ".notmuch");

    if (stat (dot_notmuch_path, &st)) {
	int count;

	count = 0;
	count_files (db_path, &count);
	if (interrupted)
	    return 1;

	printf ("Found %d total files (that's not much mail).\n", count);
	notmuch = notmuch_database_create (db_path);
	add_files_state.total_files = count;
    } else {
	notmuch = notmuch_database_open (db_path,
					 NOTMUCH_DATABASE_MODE_READ_WRITE);
	add_files_state.total_files = 0;
    }

    if (notmuch == NULL)
	return 1;

    talloc_free (dot_notmuch_path);
    dot_notmuch_path = NULL;

    add_files_state.processed_files = 0;
    add_files_state.added_messages = 0;
    gettimeofday (&add_files_state.tv_start, NULL);

    ret = add_files (notmuch, db_path, &add_files_state);

    gettimeofday (&tv_now, NULL);
    elapsed = notmuch_time_elapsed (add_files_state.tv_start,
				    tv_now);
    if (add_files_state.processed_files) {
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
    }
    if (add_files_state.added_messages) {
	printf ("Added %d new %s to the database.\n",
		add_files_state.added_messages,
		add_files_state.added_messages == 1 ?
		"message" : "messages");
    } else {
	printf ("No new mail.\n");
    }

    if (ret) {
	printf ("\nNote: At least one error was encountered: %s\n",
		notmuch_status_to_string (ret));
    }

    notmuch_database_close (notmuch);

    return ret || interrupted;
}
