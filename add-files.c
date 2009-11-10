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

static volatile sig_atomic_t do_add_files_print_progress = 0;

static void
handle_sigalrm (unused (int signal))
{
    do_add_files_print_progress = 1;
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
    struct dirent *e, *entry = NULL;
    int entry_length;
    int err;
    char *next = NULL;
    time_t path_mtime, path_dbtime;
    notmuch_status_t status, ret = NOTMUCH_STATUS_SUCCESS;
    notmuch_message_t *message = NULL, **closure;

    /* If we're told to, we bail out on encountering a read-only
     * directory, (with this being a clear clue from the user to
     * Notmuch that new mail won't be arriving there and we need not
     * look. */
    if (state->ignore_read_only_directories &&
	(st->st_mode & S_IWUSR) == 0)
    {
	state->saw_read_only_directory = TRUE;
	goto DONE;
    }

    path_mtime = st->st_mtime;

    path_dbtime = notmuch_database_get_timestamp (notmuch, path);

    dir = opendir (path);
    if (dir == NULL) {
	fprintf (stderr, "Error opening directory %s: %s\n",
		 path, strerror (errno));
	ret = NOTMUCH_STATUS_FILE_ERROR;
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
	    ret = NOTMUCH_STATUS_FILE_ERROR;
	    goto DONE;
	}

	if (e == NULL)
	    break;

	/* If this directory hasn't been modified since the last
	 * add_files, then we only need to look further for
	 * sub-directories. */
	if (path_mtime <= path_dbtime && entry->d_type != DT_DIR)
	    continue;

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

	next = talloc_asprintf (notmuch, "%s/%s", path, entry->d_name);

	if (stat (next, st)) {
	    fprintf (stderr, "Error reading %s: %s\n",
		     next, strerror (errno));
	    ret = NOTMUCH_STATUS_FILE_ERROR;
	    continue;
	}

	if (S_ISREG (st->st_mode)) {
	    /* If the file hasn't been modified since the last
	     * add_files, then we need not look at it. */
	    if (st->st_mtime > path_dbtime) {
		state->processed_files++;

		if (state->callback)
		    closure = &message;
		else
		    closure = NULL;

		status = notmuch_database_add_message (notmuch, next, closure);
		switch (status) {
		    /* success */
		    case NOTMUCH_STATUS_SUCCESS:
			state->added_messages++;
			if (state->callback)
			    (state->callback) (message);
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

    status = notmuch_database_set_timestamp (notmuch, path, path_mtime);
    if (status && ret == NOTMUCH_STATUS_SUCCESS)
	ret = status;

  DONE:
    if (next)
	talloc_free (next);
    if (entry)
	free (entry);
    if (dir)
	closedir (dir);

    return ret;
}

/* This is the top-level entry point for add_files. It does a couple
 * of error checks, sets up the progress-printing timer and then calls
 * into the recursive function. */
notmuch_status_t
add_files (notmuch_database_t *notmuch,
	   const char *path,
	   add_files_state_t *state)
{
    struct stat st;
    notmuch_status_t status;
    struct sigaction action;
    struct itimerval timerval;

    if (stat (path, &st)) {
	fprintf (stderr, "Error reading directory %s: %s\n",
		 path, strerror (errno));
	return NOTMUCH_STATUS_FILE_ERROR;
    }

    if (! S_ISDIR (st.st_mode)) {
	fprintf (stderr, "Error: %s is not a directory.\n", path);
	return NOTMUCH_STATUS_FILE_ERROR;
    }

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

    status = add_files_recursive (notmuch, path, &st, state);

    /* Now stop the timer. */
    timerval.it_interval.tv_sec = 0;
    timerval.it_interval.tv_usec = 0;
    timerval.it_value.tv_sec = 0;
    timerval.it_value.tv_usec = 0;
    setitimer (ITIMER_REAL, &timerval, NULL);

    /* And disable the signal handler. */
    action.sa_handler = SIG_IGN;
    sigaction (SIGALRM, &action, NULL);

    return status;
}
