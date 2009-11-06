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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE /* for getline */
#endif
#include <stdio.h>

#include <gmime/gmime.h>

#include "notmuch.h"

/* This is separate from notmuch-private.h because we're trying to
 * keep notmuch.c from looking into any internals, (which helps us
 * develop notmuch.h into a plausible library interface).
 */
#include "xutil.h"

#include <stddef.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <signal.h>

#include <talloc.h>

#define unused(x) x __attribute__ ((unused))

/* There's no point in continuing when we've detected that we've done
 * something wrong internally (as opposed to the user passing in a
 * bogus value).
 *
 * Note that __location__ comes from talloc.h.
 */
#define INTERNAL_ERROR(format, ...)			\
    do {						\
	fprintf(stderr,					\
		"Internal error: " format " (%s)\n",	\
		##__VA_ARGS__, __location__);		\
	exit (1);					\
    } while (0)

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))

typedef int (*command_function_t) (void *ctx, int argc, char *argv[]);

typedef struct command {
    const char *name;
    command_function_t function;
    const char *summary;
    const char *documentation;
} command_t;

typedef void (*add_files_callback_t) (notmuch_message_t *message);

typedef struct {
    int ignore_read_only_directories;
    int saw_read_only_directory;

    int total_files;
    int processed_files;
    int added_messages;
    struct timeval tv_start;

    add_files_callback_t callback;
} add_files_state_t;

static void
chomp_newline (char *str)
{
    if (str && str[strlen(str)-1] == '\n')
	str[strlen(str)-1] = '\0';
}

/* Compute the number of seconds elapsed from start to end. */
static double
tv_elapsed (struct timeval start, struct timeval end)
{
    return ((end.tv_sec - start.tv_sec) +
	    (end.tv_usec - start.tv_usec) / 1e6);
}

static void
print_formatted_seconds (double seconds)
{
    int hours;
    int minutes;

    if (seconds < 1) {
	printf ("almost no time");
	return;
    }

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

    printf ("%ds", (int) seconds);
}

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

    elapsed_overall = tv_elapsed (state->tv_start, tv_now);
    rate_overall = (state->processed_files) / elapsed_overall;

    printf ("Processed %d", state->processed_files);

    if (state->total_files) {
	printf (" of %d files (", state->total_files);
	print_formatted_seconds ((state->total_files - state->processed_files) /
				 rate_overall);
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
static notmuch_status_t
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

static int
setup_command (unused (void *ctx), unused (int argc), unused (char *argv[]))
{
    notmuch_database_t *notmuch = NULL;
    char *default_path, *mail_directory = NULL;
    size_t line_size;
    int count;
    add_files_state_t add_files_state;
    double elapsed;
    struct timeval tv_now;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;

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

    /* Coerce th directory into an absolute directory name. */
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

	printf ("Abs: %s\n", mail_directory);
    }

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

    add_files_state.ignore_read_only_directories = FALSE;
    add_files_state.saw_read_only_directory = FALSE;
    add_files_state.total_files = count;
    add_files_state.processed_files = 0;
    add_files_state.added_messages = 0;
    add_files_state.callback = NULL;
    gettimeofday (&add_files_state.tv_start, NULL);

    ret = add_files (notmuch, mail_directory, &add_files_state);

    gettimeofday (&tv_now, NULL);
    elapsed = tv_elapsed (add_files_state.tv_start,
			  tv_now);
    printf ("Processed %d %s in ", add_files_state.processed_files,
	    add_files_state.processed_files == 1 ?
	    "file" : "total files");
    print_formatted_seconds (elapsed);
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

static void
tag_inbox_and_unread (notmuch_message_t *message)
{
    notmuch_message_add_tag (message, "inbox");
    notmuch_message_add_tag (message, "unread");
}

static int
new_command (unused (void *ctx), unused (int argc), unused (char *argv[]))
{
    notmuch_database_t *notmuch;
    const char *mail_directory;
    add_files_state_t add_files_state;
    double elapsed;
    struct timeval tv_now;
    int ret = 0;

    notmuch = notmuch_database_open (NULL);
    if (notmuch == NULL) {
	ret = 1;
	goto DONE;
    }

    mail_directory = notmuch_database_get_path (notmuch);

    add_files_state.ignore_read_only_directories = TRUE;
    add_files_state.saw_read_only_directory = FALSE;
    add_files_state.total_files = 0;
    add_files_state.processed_files = 0;
    add_files_state.added_messages = 0;
    add_files_state.callback = tag_inbox_and_unread;
    gettimeofday (&add_files_state.tv_start, NULL);

    ret = add_files (notmuch, mail_directory, &add_files_state);

    gettimeofday (&tv_now, NULL);
    elapsed = tv_elapsed (add_files_state.tv_start,
			  tv_now);
    if (add_files_state.processed_files) {
	printf ("Processed %d %s in ", add_files_state.processed_files,
		add_files_state.processed_files == 1 ?
		"file" : "total files");
	print_formatted_seconds (elapsed);
	if (elapsed > 1) {
	    printf (" (%d files/sec.).                 \n",
		    (int) (add_files_state.processed_files / elapsed));
	} else {
	    printf (".                    \n");
	}
    }
    if (add_files_state.added_messages) {
	printf ("Added %d new %s to the database (not much, really).\n",
		add_files_state.added_messages,
		add_files_state.added_messages == 1 ?
		"message" : "messages");
    } else {
	printf ("No new mail---and that's not much.\n");
    }

    if (elapsed > 1 && ! add_files_state.saw_read_only_directory) {
	printf ("\nTip: If you have any sub-directories that are archives (that is,\n"
		"they will never receive new mail), marking these directores as\n"
		"read-only (chmod u-w /path/to/dir) will make \"notmuch new\"\n"
		"much more efficient (it won't even look in those directories).\n");
    }

    if (ret) {
	printf ("\nNote: At least one error was encountered: %s\n",
		notmuch_status_to_string (ret));
    }

  DONE:
    if (notmuch)
	notmuch_database_close (notmuch);

    return ret;
}

/* Construct a single query string from the passed arguments, using
 * 'ctx' as the talloc owner for all allocations.
 *
 * Currently, the arguments are just connected with space characters,
 * but we might do more processing in the future, (such as inserting
 * any AND operators needed to work around Xapian QueryParser bugs).
 *
 * This function returns NULL in case of insufficient memory.
 */
static char *
query_string_from_args (void *ctx, int argc, char *argv[])
{
    char *query_string;
    int i;

    query_string = talloc_strdup (ctx, "");
    if (query_string == NULL)
	return NULL;

    for (i = 0; i < argc; i++) {
	if (i != 0) {
	    query_string = talloc_strdup_append (query_string, " ");
	    if (query_string == NULL)
		return NULL;
	}

	query_string = talloc_strdup_append (query_string, argv[i]);
	if (query_string == NULL)
	    return NULL;
    }

    return query_string;
}

/* Format a nice representation of 'time' relative to the current time.
 *
 * Examples include:
 *
 *	5 mins. ago	(For times less than 60 minutes ago)
 *	Today 12:30	(For times >60 minutes but still today)
 *	Yest. 12:30
 *	Mon.  12:30	(Before yesterday but fewer than 7 days ago)
 *	October 12	(Between 7 and 180 days ago (about 6 months))
 *	2008-06-30	(More than 180 days ago)
 */
#define MINUTE (60)
#define HOUR (60 * MINUTE)
#define DAY (24 * HOUR)
#define RELATIVE_DATE_MAX 20
static const char *
_format_relative_date (void *ctx, time_t then)
{
    struct tm tm_now, tm_then;
    time_t now = time(NULL);
    time_t delta;
    char *result;

    localtime_r (&now, &tm_now);
    localtime_r (&then, &tm_then);

    result = talloc_zero_size (ctx, RELATIVE_DATE_MAX);
    if (result == NULL)
	return "when?";

    if (then > now)
	return "the future";

    delta = now - then;

    if (delta > 180 * DAY) {
	strftime (result, RELATIVE_DATE_MAX,
		  "%F", &tm_then); /* 2008-06-30 */
	return result;
    }

    if (delta < 3600) {
	snprintf (result, RELATIVE_DATE_MAX,
		  "%d mins. ago", (int) (delta / 60));
	return result;
    }

    if (delta <= 7 * DAY) {
	if (tm_then.tm_wday == tm_now.tm_wday &&
	    delta < DAY)
	{
	    strftime (result, RELATIVE_DATE_MAX,
		      "Today %R", &tm_then); /* Today 12:30 */
	    return result;
	} else if ((tm_now.tm_wday + 7 - tm_then.tm_wday) % 7 == 1) {
	    strftime (result, RELATIVE_DATE_MAX,
		      "Yest. %R", &tm_then); /* Yest. 12:30 */
	    return result;
	} else {
	    if (tm_then.tm_wday != tm_now.tm_wday) {
		strftime (result, RELATIVE_DATE_MAX,
			  "%a. %R", &tm_then); /* Mon. 12:30 */
		return result;
	    }
	}
    }

    strftime (result, RELATIVE_DATE_MAX,
	      "%B %d", &tm_then); /* October 12 */
    return result;
}
#undef MINUTE
#undef HOUR
#undef DAY

static int
search_command (void *ctx, int argc, char *argv[])
{
    void *local = talloc_new (ctx);
    notmuch_database_t *notmuch = NULL;
    notmuch_query_t *query;
    notmuch_threads_t *threads;
    notmuch_thread_t *thread;
    notmuch_tags_t *tags;
    char *query_str;
    const char *relative_date;
    time_t date;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;

    notmuch = notmuch_database_open (NULL);
    if (notmuch == NULL) {
	ret = 1;
	goto DONE;
    }

    query_str = query_string_from_args (local, argc, argv);

    query = notmuch_query_create (notmuch, query_str);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	ret = 1;
	goto DONE;
    }

    for (threads = notmuch_query_search_threads (query);
	 notmuch_threads_has_more (threads);
	 notmuch_threads_advance (threads))
    {
	int first = 1;

	thread = notmuch_threads_get (threads);

	date = notmuch_thread_get_oldest_date (thread);
	relative_date = _format_relative_date (local, date);

	printf ("thread:%s %12s %s",
		notmuch_thread_get_thread_id (thread),
		relative_date,
		notmuch_thread_get_subject (thread));

	printf (" (");
	for (tags = notmuch_thread_get_tags (thread);
	     notmuch_tags_has_more (tags);
	     notmuch_tags_advance (tags))
	{
	    if (! first)
		printf (" ");
	    printf ("%s", notmuch_tags_get (tags));
	    first = 0;
	}
	printf (")\n");

	notmuch_thread_destroy (thread);
    }

    notmuch_query_destroy (query);

  DONE:
    if (notmuch)
	notmuch_database_close (notmuch);
    talloc_free (local);

    return ret;
}

static const char *
_get_tags_as_string (void *ctx, notmuch_message_t *message)
{
    notmuch_tags_t *tags;
    int first = 1;
    const char *tag;
    char *result;

    result = talloc_strdup (ctx, "");
    if (result == NULL)
	return NULL;

    for (tags = notmuch_message_get_tags (message);
	 notmuch_tags_has_more (tags);
	 notmuch_tags_advance (tags))
    {
	tag = notmuch_tags_get (tags);

	result = talloc_asprintf_append (result, "%s%s",
					 first ? "" : " ", tag);
	first = 0;
    }

    return result;
}

/* Get a nice, single-line summary of message. */
static const char *
_get_one_line_summary (void *ctx, notmuch_message_t *message)
{
    const char *from;
    time_t date;
    const char *relative_date;
    const char *tags;

    from = notmuch_message_get_header (message, "from");

    date = notmuch_message_get_date (message);
    relative_date = _format_relative_date (ctx, date);

    tags = _get_tags_as_string (ctx, message);

    return talloc_asprintf (ctx, "%s (%s) (%s)",
			    from, relative_date, tags);
}

static void
show_message_part (GMimeObject *part, int *part_count)
{
    GMimeStream *stream;
    GMimeDataWrapper *wrapper;
    GMimeContentDisposition *disposition;
    GMimeContentType *content_type;

    *part_count = *part_count + 1;

    if (GMIME_IS_MULTIPART (part)) {
	GMimeMultipart *multipart = GMIME_MULTIPART (part);
	int i;

	for (i = 0; i < g_mime_multipart_get_count (multipart); i++) {
	    if (GMIME_IS_MULTIPART_SIGNED (multipart)) {
		/* Don't index the signature. */
		if (i == 1)
		    continue;
		if (i > 1)
		    fprintf (stderr, "Warning: Unexpected extra parts of mutlipart/signed. Continuing.\n");
	    }
	    show_message_part (g_mime_multipart_get_part (multipart, i),
			       part_count);
	}
	return;
    }

    if (GMIME_IS_MESSAGE_PART (part)) {
	GMimeMessage *mime_message;

	mime_message = g_mime_message_part_get_message (GMIME_MESSAGE_PART (part));

	show_message_part (g_mime_message_get_mime_part (mime_message),
			   part_count);

	return;
    }

    if (! (GMIME_IS_PART (part))) {
	fprintf (stderr, "Warning: Not displaying unknown mime part: %s.\n",
		 g_type_name (G_OBJECT_TYPE (part)));
	return;
    }

    disposition = g_mime_object_get_content_disposition (part);
    if (disposition &&
	strcmp (disposition->disposition, GMIME_DISPOSITION_ATTACHMENT) == 0)
    {
	const char *filename = g_mime_part_get_filename (GMIME_PART (part));
	content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

	printf ("\fattachment{ ID: %d, Content-type: %s\n",
		*part_count,
		g_mime_content_type_to_string (content_type));
	printf ("Attachment: %s (%s)\n", filename,
		g_mime_content_type_to_string (content_type));
	printf ("\fattachment}\n");

	return;
    }

    content_type = g_mime_object_get_content_type (GMIME_OBJECT (part));

    printf ("\fpart{ ID: %d, Content-type: %s\n",
	    *part_count,
	    g_mime_content_type_to_string (content_type));

    if (g_mime_content_type_is_type (content_type, "text", "*") &&
	!g_mime_content_type_is_type (content_type, "text", "html"))
    {
	stream = g_mime_stream_file_new (stdout);
	g_mime_stream_file_set_owner (GMIME_STREAM_FILE (stream), FALSE);

	wrapper = g_mime_part_get_content_object (GMIME_PART (part));
	if (wrapper)
	    g_mime_data_wrapper_write_to_stream (wrapper, stream);

	g_object_unref (stream);
    }
    else
    {
	printf ("Non-text part: %s\n",
		g_mime_content_type_to_string (content_type));
    }

    printf ("\fpart}\n");
}

static notmuch_status_t
show_message_body (const char *filename)
{
    GMimeStream *stream = NULL;
    GMimeParser *parser = NULL;
    GMimeMessage *mime_message = NULL;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;
    static int initialized = 0;
    FILE *file = NULL;
    int part_count = 0;

    if (! initialized) {
	g_mime_init (0);
	initialized = 1;
    }

    file = fopen (filename, "r");
    if (! file) {
	fprintf (stderr, "Error opening %s: %s\n", filename, strerror (errno));
	ret = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    stream = g_mime_stream_file_new (file);
    g_mime_stream_file_set_owner (GMIME_STREAM_FILE (stream), FALSE);

    parser = g_mime_parser_new_with_stream (stream);

    mime_message = g_mime_parser_construct_message (parser);

    show_message_part (g_mime_message_get_mime_part (mime_message),
		       &part_count);

  DONE:
    if (mime_message)
	g_object_unref (mime_message);

    if (parser)
	g_object_unref (parser);

    if (stream)
	g_object_unref (stream);

    if (file)
	fclose (file);

    return ret;
}

static int
show_command (void *ctx, unused (int argc), unused (char *argv[]))
{
    void *local = talloc_new (ctx);
    char *query_string;
    notmuch_database_t *notmuch = NULL;
    notmuch_query_t *query = NULL;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    int ret = 0;

    const char *headers[] = {
	"From", "To", "Cc", "Bcc", "Date"
    };
    const char *name, *value;
    unsigned int i;

    notmuch = notmuch_database_open (NULL);
    if (notmuch == NULL) {
	ret = 1;
	goto DONE;
    }

    query_string = query_string_from_args (local, argc, argv);
    if (query_string == NULL) {
	fprintf (stderr, "Out of memory\n");
	ret = 1;
	goto DONE;
    }

    query = notmuch_query_create (notmuch, query_string);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	ret = 1;
	goto DONE;
    }

    for (messages = notmuch_query_search_messages (query);
	 notmuch_messages_has_more (messages);
	 notmuch_messages_advance (messages))
    {
	message = notmuch_messages_get (messages);

	printf ("\fmessage{ id:%s filename:%s\n",
		notmuch_message_get_message_id (message),
		notmuch_message_get_filename (message));

	printf ("\fheader{\n");

	printf ("%s\n", _get_one_line_summary (local, message));

	printf ("%s\n", notmuch_message_get_header (message, "subject"));

	for (i = 0; i < ARRAY_SIZE (headers); i++) {
	    name = headers[i];
	    value = notmuch_message_get_header (message, name);
	    if (value)
		printf ("%s: %s\n", name, value);
	}

	printf ("\fheader}\n");
	printf ("\fbody{\n");

	show_message_body (notmuch_message_get_filename (message));

	printf ("\fbody}\n");

	printf ("\fmessage}\n");

	notmuch_message_destroy (message);
    }

  DONE:
    if (local)
	talloc_free (local);

    if (query)
	notmuch_query_destroy (query);

    if (notmuch)
	notmuch_database_close (notmuch);

    return ret;
}

static int
tag_command (void *ctx, unused (int argc), unused (char *argv[]))
{
    void *local;
    int *add_tags, *remove_tags;
    int add_tags_count = 0;
    int remove_tags_count = 0;
    char *query_string;
    notmuch_database_t *notmuch = NULL;
    notmuch_query_t *query;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    int ret = 0;
    int i;

    local = talloc_new (ctx);
    if (local == NULL) {
	ret = 1;
	goto DONE;
    }

    add_tags = talloc_size (local, argc * sizeof (int));
    if (add_tags == NULL) {
	ret = 1;
	goto DONE;
    }

    remove_tags = talloc_size (local, argc * sizeof (int));
    if (remove_tags == NULL) {
	ret = 1;
	goto DONE;
    }

    for (i = 0; i < argc; i++) {
	if (strcmp (argv[i], "--") == 0) {
	    i++;
	    break;
	}
	if (argv[i][0] == '+') {
	    add_tags[add_tags_count++] = i;
	} else if (argv[i][0] == '-') {
	    remove_tags[remove_tags_count++] = i;
	} else {
	    break;
	}
    }

    if (add_tags_count == 0 && remove_tags_count == 0) {
	fprintf (stderr, "Error: 'notmuch tag' requires at least one tag to add or remove.\n");
	ret = 1;
	goto DONE;
    }

    if (i == argc) {
	fprintf (stderr, "Error: 'notmuch tag' requires at least one search term.\n");
	ret = 1;
	goto DONE;
    }

    notmuch = notmuch_database_open (NULL);
    if (notmuch == NULL) {
	ret = 1;
	goto DONE;
    }

    query_string = query_string_from_args (local, argc - i, &argv[i]);

    query = notmuch_query_create (notmuch, query_string);
    if (query == NULL) {
	fprintf (stderr, "Out of memory.\n");
	ret = 1;
	goto DONE;
    }

    for (messages = notmuch_query_search_messages (query);
	 notmuch_messages_has_more (messages);
	 notmuch_messages_advance (messages))
    {
	message = notmuch_messages_get (messages);

	notmuch_message_freeze (message);

	for (i = 0; i < remove_tags_count; i++)
	    notmuch_message_remove_tag (message,
					argv[remove_tags[i]] + 1);

	for (i = 0; i < add_tags_count; i++)
	    notmuch_message_add_tag (message, argv[add_tags[i]] + 1);

	notmuch_message_thaw (message);

	notmuch_message_destroy (message);
    }

    notmuch_query_destroy (query);

  DONE:
    if (notmuch)
	notmuch_database_close (notmuch);

    talloc_free (local);

    return ret;
}

static int
dump_command (unused (void *ctx), int argc, char *argv[])
{
    FILE *output = NULL;
    notmuch_database_t *notmuch = NULL;
    notmuch_query_t *query;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    notmuch_tags_t *tags;
    int ret = 0;

    if (argc) {
	output = fopen (argv[0], "w");
	if (output == NULL) {
	    fprintf (stderr, "Error opening %s for writing: %s\n",
		     argv[0], strerror (errno));
	    ret = 1;
	    goto DONE;
	}
    } else {
	output = stdout;
    }

    notmuch = notmuch_database_open (NULL);
    if (notmuch == NULL) {
	ret = 1;
	goto DONE;
    }

    query = notmuch_query_create (notmuch, "");
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	ret = 1;
	goto DONE;
    }

    notmuch_query_set_sort (query, NOTMUCH_SORT_MESSAGE_ID);

    for (messages = notmuch_query_search_messages (query);
	 notmuch_messages_has_more (messages);
	 notmuch_messages_advance (messages))
    {
	int first = 1;
	message = notmuch_messages_get (messages);

	fprintf (output,
		 "%s (", notmuch_message_get_message_id (message));

	for (tags = notmuch_message_get_tags (message);
	     notmuch_tags_has_more (tags);
	     notmuch_tags_advance (tags))
	{
	    if (! first)
		fprintf (output, " ");

	    fprintf (output, "%s", notmuch_tags_get (tags));

	    first = 0;
	}

	fprintf (output, ")\n");

	notmuch_message_destroy (message);
    }

    notmuch_query_destroy (query);

  DONE:
    if (notmuch)
	notmuch_database_close (notmuch);
    if (output && output != stdout)
	fclose (output);

    return ret;
}

static int
restore_command (unused (void *ctx), int argc, char *argv[])
{
    FILE *input = NULL;
    notmuch_database_t *notmuch = NULL;
    char *line = NULL;
    size_t line_size;
    ssize_t line_len;
    regex_t regex;
    int rerr;
    int ret = 0;

    if (argc) {
	input = fopen (argv[0], "r");
	if (input == NULL) {
	    fprintf (stderr, "Error opening %s for reading: %s\n",
		     argv[0], strerror (errno));
	    ret = 1;
	    goto DONE;
	}
    } else {
	printf ("No filename given. Reading dump from stdin.\n");
	input = stdin;
    }

    notmuch = notmuch_database_open (NULL);
    if (notmuch == NULL) {
	ret = 1;
	goto DONE;
    }

    /* Dump output is one line per message. We match a sequence of
     * non-space characters for the message-id, then one or more
     * spaces, then a list of space-separated tags as a sequence of
     * characters within literal '(' and ')'. */
    xregcomp (&regex,
	      "^([^ ]+) \\(([^)]*)\\)$",
	      REG_EXTENDED);

    while ((line_len = getline (&line, &line_size, input)) != -1) {
	regmatch_t match[3];
	char *message_id, *tags, *tag, *next;
	notmuch_message_t *message;
	notmuch_status_t status;

	chomp_newline (line);

	rerr = xregexec (&regex, line, 3, match, 0);
	if (rerr == REG_NOMATCH)
	{
	    fprintf (stderr, "Warning: Ignoring invalid input line: %s\n",
		     line);
	    continue;
	}

	message_id = xstrndup (line + match[1].rm_so,
			       match[1].rm_eo - match[1].rm_so);
	tags = xstrndup (line + match[2].rm_so,
			 match[2].rm_eo - match[2].rm_so);

	if (strlen (tags)) {

	    message = notmuch_database_find_message (notmuch, message_id);
	    if (message == NULL) {
		fprintf (stderr, "Warning: Cannot apply tags to missing message: %s\n",
			 message_id);
		goto NEXT_LINE;
	    }

	    notmuch_message_freeze (message);

	    notmuch_message_remove_all_tags (message);

	    next = tags;
	    while (next) {
		tag = strsep (&next, " ");
		if (*tag == '\0')
		    continue;
		status = notmuch_message_add_tag (message, tag);
		if (status) {
		    fprintf (stderr,
			     "Error applying tag %s to message %s:\n",
			     tag, message_id);
		    fprintf (stderr, "%s\n",
			     notmuch_status_to_string (status));
		}
	    }

	    notmuch_message_thaw (message);
	    notmuch_message_destroy (message);
	}
      NEXT_LINE:
	free (message_id);
	free (tags);
    }

    regfree (&regex);

  DONE:
    if (line)
	free (line);
    if (notmuch)
	notmuch_database_close (notmuch);
    if (input && input != stdin)
	fclose (input);

    return ret;
}

static int
help_command (void *ctx, int argc, char *argv[]);

command_t commands[] = {
    { "setup", setup_command,
      "Interactively setup notmuch for first use.",
      "\t\tThe setup command is the first command you will run in order\n"
      "\t\tto start using notmuch. It will prompt you for the directory\n"
      "\t\tcontaining your email archives, and will then proceed to build\n"
      "\t\ta database to allow fast searching of that mail.\n\n"
      "\t\tInvoking notmuch with no command argument will run setup if\n"
      "\t\tthe setup command has not previously been completed." },
    { "new", new_command,
      "Find and import any new messages.",
      "\t\tScans all sub-directories of the database, adding new messages\n"
      "\t\tthat are found. Each new message will be tagged as both\n"
      "\t\t\"inbox\" and \"unread\".\n"
      "\n"
      "\t\tNote: \"notmuch new\" will skip any read-only directories,\n"
      "\t\tso you can use that to mark directories that will not\n"
      "\t\treceive any new mail (and make \"notmuch new\" faster)." },
    { "search", search_command,
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
    { "show", show_command,
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
    { "tag", tag_command,
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
    { "dump", dump_command,
      "[<filename>]\n\n"
      "\t\tCreate a plain-text dump of the tags for each message.",
      "\t\tOutput is to the given filename, if any, or to stdout.\n"
      "\t\tThese tags are the only data in the notmuch database\n"
      "\t\tthat can't be recreated from the messages themselves.\n"
      "\t\tThe output of notmuch dump is therefore the only\n"
      "\t\tcritical thing to backup (and much more friendly to\n"
      "\t\tincremental backup than the native database files.)" },
    { "restore", restore_command,
      "<filename>\n\n"
      "\t\tRestore the tags from the given dump file (see 'dump').",
      "\t\tNote: The dump file format is specifically chosen to be\n"
      "\t\tcompatible with the format of files produced by sup-dump.\n"
      "\t\tSo if you've previously been using sup for mail, then the\n"
      "\t\t\"notmuch restore\" command provides you a way to import\n"
      "\t\tall of your tags (or labels as sup calls them)." },
    { "help", help_command,
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
help_command (unused (void *ctx), int argc, char *argv[])
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
	return setup_command (local, 0, NULL);

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
