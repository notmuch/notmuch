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

typedef int (*command_function_t) (int argc, char *argv[]);

typedef struct command {
    const char *name;
    command_function_t function;
    const char *usage;
} command_t;

typedef struct {
    int ignore_read_only_directories;
    int saw_read_only_directory;

    int total_files;
    int processed_files;
    int added_messages;
    struct timeval tv_start;
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

		status = notmuch_database_add_message (notmuch, next);
		switch (status) {
		    /* success */
		    case NOTMUCH_STATUS_SUCCESS:
			state->added_messages++;
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
		    case NOTMUCH_STATUS_LAST_STATUS:
			INTERNAL_ERROR ("add_message returned unexpected value: %d",  status);
			goto DONE;
		}
		if (state->processed_files % 1000 == 0)
		    add_files_print_progress (state);
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
 * of error checks, and then calls into the recursive function,
 * (avoiding the repeating of these error checks at every
 * level---which would be useless becaues we already do a stat() at
 * the level above). */
static notmuch_status_t
add_files (notmuch_database_t *notmuch,
	   const char *path,
	   add_files_state_t *state)
{
    struct stat st;

    if (stat (path, &st)) {
	fprintf (stderr, "Error reading directory %s: %s\n",
		 path, strerror (errno));
	return NOTMUCH_STATUS_FILE_ERROR;
    }

    if (! S_ISDIR (st.st_mode)) {
	fprintf (stderr, "Error: %s is not a directory.\n", path);
	return NOTMUCH_STATUS_FILE_ERROR;
    }

    return add_files_recursive (notmuch, path, &st, state);
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
setup_command (unused (int argc), unused (char *argv[]))
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

static int
new_command (unused (int argc), unused (char *argv[]))
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

static int
search_command (int argc, char *argv[])
{
    void *local = talloc_new (NULL);
    notmuch_database_t *notmuch = NULL;
    notmuch_query_t *query;
    notmuch_thread_results_t *results;
    notmuch_thread_t *thread;
    notmuch_tags_t *tags;
    char *query_str;
    int i;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;

    notmuch = notmuch_database_open (NULL);
    if (notmuch == NULL) {
	ret = 1;
	goto DONE;
    }

    /* XXX: Should add xtalloc wrappers here and use them. */
    query_str = talloc_strdup (local, "");

    for (i = 0; i < argc; i++) {
	if (i != 0)
	    query_str = talloc_asprintf_append (query_str, " ");

	query_str = talloc_asprintf_append (query_str, "%s", argv[i]);
    }

    query = notmuch_query_create (notmuch, query_str);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	ret = 1;
	goto DONE;
    }

    for (results = notmuch_query_search_threads (query);
	 notmuch_thread_results_has_more (results);
	 notmuch_thread_results_advance (results))
    {
	int first = 1;

	thread = notmuch_thread_results_get (results);

	printf ("%s (", notmuch_thread_get_thread_id (thread));

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

static int
show_command (unused (int argc), unused (char *argv[]))
{
    fprintf (stderr, "Error: show is not implemented yet.\n");
    return 1;
}

static int
dump_command (int argc, char *argv[])
{
    FILE *output = NULL;
    notmuch_database_t *notmuch = NULL;
    notmuch_query_t *query;
    notmuch_message_results_t *results;
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

    for (results = notmuch_query_search_messages (query);
	 notmuch_message_results_has_more (results);
	 notmuch_message_results_advance (results))
    {
	int first = 1;
	message = notmuch_message_results_get (results);

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
restore_command (int argc, char *argv[])
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
		fprintf (stderr, "Warning: Cannot apply tags to missing message: %s (",
			 message_id);
	    }

	    next = tags;
	    while (next) {
		tag = strsep (&next, " ");
		if (*tag == '\0')
		    continue;
		if (message) {
		    status = notmuch_message_add_tag (message, tag);
		    if (status) {
			fprintf (stderr,
				 "Error applying tag %s to message %s:\n",
				 tag, message_id);
			fprintf (stderr, "%s\n",
				 notmuch_status_to_string (status));
		    }
		} else {
		    fprintf (stderr, "%s%s",
			     tag == tags ? "" : " ", tag);
		}
	    }

	    if (message)
		notmuch_message_destroy (message);
	    else
		fprintf (stderr, ")\n");
	}
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

command_t commands[] = {
    { "setup", setup_command,
      "Interactively setup notmuch for first use.\n\n"
      "\t\tInvoking notmuch with no command argument will run setup if\n"
      "\t\tthe setup command has not previously been completed." },
    { "new", new_command,
      "Find and import any new messages.\n\n"
      "\t\tScans all sub-directories of the database, adding new files\n"
      "\t\tthat are found. Note: \"notmuch new\" will skip any\n"
      "\t\tread-only directories, so you can use that to mark\n"
      "\t\tdirectories that will not receive any new mail."},
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

	fprintf (stderr, "\t%s\t%s\n\n", command->name, command->usage);
    }
}
    
int
main (int argc, char *argv[])
{
    command_t *command;
    unsigned int i;

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
	usage ();
	return 0;
    } else {
	fprintf (stderr, "Error: Unknown command '%s'\n\n", argv[1]);
	usage ();
	return 1;
    }
}
