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

typedef struct _filename_node {
    char *filename;
    struct _filename_node *next;
} _filename_node_t;

typedef struct _filename_list {
    unsigned count;
    _filename_node_t *head;
    _filename_node_t **tail;
} _filename_list_t;

typedef struct {
    int output_is_a_tty;
    int verbose;
    const char **new_tags;
    size_t new_tags_length;

    int total_files;
    int processed_files;
    int added_messages;
    struct timeval tv_start;

    _filename_list_t *removed_files;
    _filename_list_t *removed_directories;

    notmuch_bool_t synchronize_flags;
    _filename_list_t *message_ids_to_sync;
} add_files_state_t;

static volatile sig_atomic_t do_print_progress = 0;

static void
handle_sigalrm (unused (int signal))
{
    do_print_progress = 1;
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

static _filename_list_t *
_filename_list_create (const void *ctx)
{
    _filename_list_t *list;

    list = talloc (ctx, _filename_list_t);
    if (list == NULL)
	return NULL;

    list->head = NULL;
    list->tail = &list->head;
    list->count = 0;

    return list;
}

static void
_filename_list_add (_filename_list_t *list,
		    const char *filename)
{
    _filename_node_t *node = talloc (list, _filename_node_t);

    list->count++;

    node->filename = talloc_strdup (list, filename);
    node->next = NULL;

    *(list->tail) = node;
    list->tail = &node->next;
}

static void
generic_print_progress (const char *action, const char *object,
			struct timeval tv_start, unsigned processed, unsigned total)
{
    struct timeval tv_now;
    double elapsed_overall, rate_overall;

    gettimeofday (&tv_now, NULL);

    elapsed_overall = notmuch_time_elapsed (tv_start, tv_now);
    rate_overall = processed / elapsed_overall;

    printf ("%s %d ", action, processed);

    if (total) {
	printf ("of %d %s", total, object);
	if (processed > 0 && elapsed_overall > 0.5) {
	    double time_remaining = ((total - processed) / rate_overall);
	    printf (" (");
	    notmuch_time_print_formatted_seconds (time_remaining);
	    printf (" remaining)");
	}
    } else {
	printf ("%s", object);
	if (elapsed_overall > 0.5)
	    printf (" (%d %s/sec.)", (int) rate_overall, object);
    }
    printf (".\033[K\r");

    fflush (stdout);
}

static int
dirent_sort_inode (const struct dirent **a, const struct dirent **b)
{
    return ((*a)->d_ino < (*b)->d_ino) ? -1 : 1;
}

static int
dirent_sort_strcmp_name (const struct dirent **a, const struct dirent **b)
{
    return strcmp ((*a)->d_name, (*b)->d_name);
}

/* Test if the directory looks like a Maildir directory.
 *
 * Search through the array of directory entries to see if we can find all
 * three subdirectories typical for Maildir, that is "new", "cur", and "tmp".
 *
 * Return 1 if the directory looks like a Maildir and 0 otherwise.
 */
static int
_entries_resemble_maildir (struct dirent **entries, int count)
{
    int i, found = 0;

    for (i = 0; i < count; i++) {
	if (entries[i]->d_type != DT_DIR && entries[i]->d_type != DT_UNKNOWN)
	    continue;

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
 *   o Ask the filesystem for the mtime of 'path' (fs_mtime)
 *   o Ask the database for its timestamp of 'path' (db_mtime)
 *
 *   o Ask the filesystem for files and directories within 'path'
 *     (via scandir and stored in fs_entries)
 *   o Ask the database for files and directories within 'path'
 *     (db_files and db_subdirs)
 *
 *   o Pass 1: For each directory in fs_entries, recursively call into
 *     this same function.
 *
 *   o Pass 2: If 'fs_mtime' > 'db_mtime', then walk fs_entries
 *     simultaneously with db_files and db_subdirs. Look for one of
 *     three interesting cases:
 *
 *	   1. Regular file in fs_entries and not in db_files
 *            This is a new file to add_message into the database.
 *
 *         2. Filename in db_files not in fs_entries.
 *            This is a file that has been removed from the mail store.
 *
 *         3. Directory in db_subdirs not in fs_entries
 *            This is a directory that has been removed from the mail store.
 *
 *     Note that the addition of a directory is not interesting here,
 *     since that will have been taken care of in pass 1. Also, we
 *     don't immediately act on file/directory removal since we must
 *     ensure that in the case of a rename that the new filename is
 *     added before the old filename is removed, (so that no
 *     information is lost from the database).
 *
 *   o Tell the database to update its time of 'path' to 'fs_mtime'
 */
static notmuch_status_t
add_files_recursive (notmuch_database_t *notmuch,
		     const char *path,
		     add_files_state_t *state)
{
    DIR *dir = NULL;
    struct dirent *entry = NULL;
    char *next = NULL;
    time_t fs_mtime, db_mtime;
    notmuch_status_t status, ret = NOTMUCH_STATUS_SUCCESS;
    notmuch_message_t *message = NULL;
    struct dirent **fs_entries = NULL;
    int i, num_fs_entries;
    notmuch_directory_t *directory;
    notmuch_filenames_t *db_files = NULL;
    notmuch_filenames_t *db_subdirs = NULL;
    struct stat st;
    notmuch_bool_t is_maildir, new_directory;
    const char **tag;

    if (stat (path, &st)) {
	fprintf (stderr, "Error reading directory %s: %s\n",
		 path, strerror (errno));
	return NOTMUCH_STATUS_FILE_ERROR;
    }

    /* This is not an error since we may have recursed based on a
     * symlink to a regular file, not a directory, and we don't know
     * that until this stat. */
    if (! S_ISDIR (st.st_mode))
	return NOTMUCH_STATUS_SUCCESS;

    fs_mtime = st.st_mtime;

    directory = notmuch_database_get_directory (notmuch, path);
    db_mtime = notmuch_directory_get_mtime (directory);

    new_directory = db_mtime ? FALSE : TRUE;

    /* If the database knows about this directory, then we sort based
     * on strcmp to match the database sorting. Otherwise, we can do
     * inode-based sorting for faster filesystem operation. */
    num_fs_entries = scandir (path, &fs_entries, 0,
			      new_directory ?
			      dirent_sort_inode : dirent_sort_strcmp_name);

    if (num_fs_entries == -1) {
	fprintf (stderr, "Error opening directory %s: %s\n",
		 path, strerror (errno));
	ret = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    /* Pass 1: Recurse into all sub-directories. */
    is_maildir = _entries_resemble_maildir (fs_entries, num_fs_entries);

    for (i = 0; i < num_fs_entries; i++) {
	if (interrupted)
	    break;

	entry = fs_entries[i];

	/* We only want to descend into directories.
	 * But symlinks can be to directories too, of course.
	 *
	 * And if the filesystem doesn't tell us the file type in the
	 * scandir results, then it might be a directory (and if not,
	 * then we'll stat and return immediately in the next level of
	 * recursion). */
	if (entry->d_type != DT_DIR &&
	    entry->d_type != DT_LNK &&
	    entry->d_type != DT_UNKNOWN)
	{
	    continue;
	}

	/* Ignore special directories to avoid infinite recursion.
	 * Also ignore the .notmuch directory and any "tmp" directory
	 * that appears within a maildir.
	 */
	/* XXX: Eventually we'll want more sophistication to let the
	 * user specify files to be ignored. */
	if (strcmp (entry->d_name, ".") == 0 ||
	    strcmp (entry->d_name, "..") == 0 ||
	    (is_maildir && strcmp (entry->d_name, "tmp") == 0) ||
	    strcmp (entry->d_name, ".notmuch") ==0)
	{
	    continue;
	}

	next = talloc_asprintf (notmuch, "%s/%s", path, entry->d_name);
	status = add_files_recursive (notmuch, next, state);
	if (status && ret == NOTMUCH_STATUS_SUCCESS)
	    ret = status;
	talloc_free (next);
	next = NULL;
    }

    /* If the directory's modification time in the filesystem is the
     * same as what we recorded in the database the last time we
     * scanned it, then we can skip the second pass entirely.
     *
     * We test for strict equality here to avoid a bug that can happen
     * if the system clock jumps backward, (preventing new mail from
     * being discovered until the clock catches up and the directory
     * is modified again).
     */
    if (fs_mtime == db_mtime)
	goto DONE;

    if (!new_directory) {
	db_files = notmuch_directory_get_child_files (directory);
	db_subdirs = notmuch_directory_get_child_directories (directory);
    }

    /* Pass 2: Scan for new files, removed files, and removed directories. */
    for (i = 0; i < num_fs_entries; i++)
    {
	if (interrupted)
	    break;

        entry = fs_entries[i];

	/* Check if we've walked past any names in db_files or
	 * db_subdirs. If so, these have been deleted. */
	while (notmuch_filenames_valid (db_files) &&
	       strcmp (notmuch_filenames_get (db_files), entry->d_name) < 0)
	{
	    char *absolute = talloc_asprintf (state->removed_files,
					      "%s/%s", path,
					      notmuch_filenames_get (db_files));

	    _filename_list_add (state->removed_files, absolute);

	    notmuch_filenames_move_to_next (db_files);
	}

	while (notmuch_filenames_valid (db_subdirs) &&
	       strcmp (notmuch_filenames_get (db_subdirs), entry->d_name) <= 0)
	{
	    const char *filename = notmuch_filenames_get (db_subdirs);

	    if (strcmp (filename, entry->d_name) < 0)
	    {
		char *absolute = talloc_asprintf (state->removed_directories,
						  "%s/%s", path, filename);

		_filename_list_add (state->removed_directories, absolute);
	    }

	    notmuch_filenames_move_to_next (db_subdirs);
	}

	/* If we're looking at a symlink, we only want to add it if it
	 * links to a regular file, (and not to a directory, say).
	 *
	 * Similarly, if the file is of unknown type (due to filesytem
	 * limitations), then we also need to look closer.
	 *
	 * In either case, a stat does the trick.
	 */
	if (entry->d_type == DT_LNK || entry->d_type == DT_UNKNOWN) {
	    int err;

	    next = talloc_asprintf (notmuch, "%s/%s", path, entry->d_name);
	    err = stat (next, &st);
	    talloc_free (next);
	    next = NULL;

	    /* Don't emit an error for a link pointing nowhere, since
	     * the directory-traversal pass will have already done
	     * that. */
	    if (err)
		continue;

	    if (! S_ISREG (st.st_mode))
		continue;
	} else if (entry->d_type != DT_REG) {
	    continue;
	}

	/* Don't add a file that we've added before. */
	if (notmuch_filenames_valid (db_files) &&
	    strcmp (notmuch_filenames_get (db_files), entry->d_name) == 0)
	{
	    notmuch_filenames_move_to_next (db_files);
	    continue;
	}

	/* We're now looking at a regular file that doesn't yet exist
	 * in the database, so add it. */
	next = talloc_asprintf (notmuch, "%s/%s", path, entry->d_name);

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
	    notmuch_message_freeze (message);
	    for (tag=state->new_tags; *tag != NULL; tag++)
	        notmuch_message_add_tag (message, *tag);
	    if (state->synchronize_flags == TRUE)
		notmuch_message_maildir_flags_to_tags (message);
	    notmuch_message_thaw (message);
	    break;
	/* Non-fatal issues (go on to next file) */
	case NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID:
	    /* Defer sync of maildir flags until after old filenames
	     * are removed in the case of a rename. */
	    if (state->synchronize_flags == TRUE)
		_filename_list_add (state->message_ids_to_sync,
				    notmuch_message_get_message_id (message));
	    break;
	case NOTMUCH_STATUS_FILE_NOT_EMAIL:
	    fprintf (stderr, "Note: Ignoring non-mail file: %s\n",
		     next);
	    break;
	/* Fatal issues. Don't process anymore. */
	case NOTMUCH_STATUS_READ_ONLY_DATABASE:
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

	if (do_print_progress) {
	    do_print_progress = 0;
	    generic_print_progress ("Processed", "files", state->tv_start,
				    state->processed_files, state->total_files);
	}

	talloc_free (next);
	next = NULL;
    }

    if (interrupted)
	goto DONE;

    /* Now that we've walked the whole filesystem list, anything left
     * over in the database lists has been deleted. */
    while (notmuch_filenames_valid (db_files))
    {
	char *absolute = talloc_asprintf (state->removed_files,
					  "%s/%s", path,
					  notmuch_filenames_get (db_files));

	_filename_list_add (state->removed_files, absolute);

	notmuch_filenames_move_to_next (db_files);
    }

    while (notmuch_filenames_valid (db_subdirs))
    {
	char *absolute = talloc_asprintf (state->removed_directories,
					  "%s/%s", path,
					  notmuch_filenames_get (db_subdirs));

	_filename_list_add (state->removed_directories, absolute);

	notmuch_filenames_move_to_next (db_subdirs);
    }

    if (! interrupted) {
	status = notmuch_directory_set_mtime (directory, fs_mtime);
	if (status && ret == NOTMUCH_STATUS_SUCCESS)
	    ret = status;
    }

  DONE:
    if (next)
	talloc_free (next);
    if (entry)
	free (entry);
    if (dir)
	closedir (dir);
    if (fs_entries)
	free (fs_entries);
    if (db_subdirs)
	notmuch_filenames_destroy (db_subdirs);
    if (db_files)
	notmuch_filenames_destroy (db_files);
    if (directory)
	notmuch_directory_destroy (directory);

    return ret;
}

static void
setup_progress_printing_timer (void)
{
    struct sigaction action;
    struct itimerval timerval;

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
}

static void
stop_progress_printing_timer (void)
{
    struct sigaction action;
    struct itimerval timerval;

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


/* This is the top-level entry point for add_files. It does a couple
 * of error checks and then calls into the recursive function. */
static notmuch_status_t
add_files (notmuch_database_t *notmuch,
	   const char *path,
	   add_files_state_t *state)
{
    notmuch_status_t status;
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

    status = add_files_recursive (notmuch, path, state);

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
    struct dirent **fs_entries = NULL;
    int num_fs_entries = scandir (path, &fs_entries, 0, dirent_sort_inode);
    int i = 0;

    if (num_fs_entries == -1) {
	fprintf (stderr, "Warning: failed to open directory %s: %s\n",
		 path, strerror (errno));
	goto DONE;
    }

    while (!interrupted) {
        if (i == num_fs_entries)
	    break;

        entry = fs_entries[i++];

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
    if (fs_entries)
        free (fs_entries);
}

static void
upgrade_print_progress (void *closure,
			double progress)
{
    add_files_state_t *state = closure;

    printf ("Upgrading database: %.2f%% complete", progress * 100.0);

    if (progress > 0) {
	struct timeval tv_now;
	double elapsed, time_remaining;

	gettimeofday (&tv_now, NULL);

	elapsed = notmuch_time_elapsed (state->tv_start, tv_now);
	time_remaining = (elapsed / progress) * (1.0 - progress);
	printf (" (");
	notmuch_time_print_formatted_seconds (time_remaining);
	printf (" remaining)");
    }

    printf (".      \r");

    fflush (stdout);
}

/* Recursively remove all filenames from the database referring to
 * 'path' (or to any of its children). */
static void
_remove_directory (void *ctx,
		   notmuch_database_t *notmuch,
		   const char *path,
		   int *renamed_files,
		   int *removed_files)
{
    notmuch_directory_t *directory;
    notmuch_filenames_t *files, *subdirs;
    notmuch_status_t status;
    char *absolute;

    directory = notmuch_database_get_directory (notmuch, path);

    for (files = notmuch_directory_get_child_files (directory);
	 notmuch_filenames_valid (files);
	 notmuch_filenames_move_to_next (files))
    {
	absolute = talloc_asprintf (ctx, "%s/%s", path,
				    notmuch_filenames_get (files));
	status = notmuch_database_remove_message (notmuch, absolute);
	if (status == NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID)
	    *renamed_files = *renamed_files + 1;
	else
	    *removed_files = *removed_files + 1;
	talloc_free (absolute);
    }

    for (subdirs = notmuch_directory_get_child_directories (directory);
	 notmuch_filenames_valid (subdirs);
	 notmuch_filenames_move_to_next (subdirs))
    {
	absolute = talloc_asprintf (ctx, "%s/%s", path,
				    notmuch_filenames_get (subdirs));
	_remove_directory (ctx, notmuch, absolute, renamed_files, removed_files);
	talloc_free (absolute);
    }

    notmuch_directory_destroy (directory);
}

int
notmuch_new_command (void *ctx, int argc, char *argv[])
{
    notmuch_config_t *config;
    notmuch_database_t *notmuch;
    add_files_state_t add_files_state;
    double elapsed;
    struct timeval tv_now, tv_start;
    int ret = 0;
    struct stat st;
    const char *db_path;
    char *dot_notmuch_path;
    struct sigaction action;
    _filename_node_t *f;
    int renamed_files, removed_files;
    notmuch_status_t status;
    int i;
    notmuch_bool_t timer_is_active = FALSE;

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
    config = notmuch_config_open (ctx, NULL, NULL);
    if (config == NULL)
	return 1;

    add_files_state.new_tags = notmuch_config_get_new_tags (config, &add_files_state.new_tags_length);
    add_files_state.synchronize_flags = notmuch_config_get_maildir_synchronize_flags (config);
    add_files_state.message_ids_to_sync = _filename_list_create (ctx);
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
	if (notmuch == NULL)
	    return 1;

	if (notmuch_database_needs_upgrade (notmuch)) {
	    printf ("Welcome to a new version of notmuch! Your database will now be upgraded.\n");
	    gettimeofday (&add_files_state.tv_start, NULL);
	    notmuch_database_upgrade (notmuch, upgrade_print_progress,
				      &add_files_state);
	    printf ("Your notmuch database has now been upgraded to database format version %u.\n",
		    notmuch_database_get_version (notmuch));
	}

	add_files_state.total_files = 0;
    }

    if (notmuch == NULL)
	return 1;

    /* Setup our handler for SIGINT. We do this after having
     * potentially done a database upgrade we this interrupt handler
     * won't support. */
    memset (&action, 0, sizeof (struct sigaction));
    action.sa_handler = handle_sigint;
    sigemptyset (&action.sa_mask);
    action.sa_flags = SA_RESTART;
    sigaction (SIGINT, &action, NULL);

    talloc_free (dot_notmuch_path);
    dot_notmuch_path = NULL;

    add_files_state.processed_files = 0;
    add_files_state.added_messages = 0;
    gettimeofday (&add_files_state.tv_start, NULL);

    add_files_state.removed_files = _filename_list_create (ctx);
    add_files_state.removed_directories = _filename_list_create (ctx);

    if (! debugger_is_active () && add_files_state.output_is_a_tty
	&& ! add_files_state.verbose) {
	setup_progress_printing_timer ();
	timer_is_active = TRUE;
    }

    ret = add_files (notmuch, db_path, &add_files_state);

    removed_files = 0;
    renamed_files = 0;
    gettimeofday (&tv_start, NULL);
    for (f = add_files_state.removed_files->head; f; f = f->next) {
	status = notmuch_database_remove_message (notmuch, f->filename);
	if (status == NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID)
	    renamed_files++;
	else
	    removed_files++;
	if (do_print_progress) {
	    do_print_progress = 0;
	    generic_print_progress ("Cleaned up", "messages",
		tv_start, removed_files + renamed_files,
		add_files_state.removed_files->count);
	}
    }

    gettimeofday (&tv_start, NULL);
    for (f = add_files_state.removed_directories->head, i = 0; f; f = f->next, i++) {
	_remove_directory (ctx, notmuch, f->filename,
			   &renamed_files, &removed_files);
	if (do_print_progress) {
	    do_print_progress = 0;
	    generic_print_progress ("Cleaned up", "directories",
		tv_start, i,
		add_files_state.removed_directories->count);
	}
    }

    talloc_free (add_files_state.removed_files);
    talloc_free (add_files_state.removed_directories);

    /* Now that removals are done (hence the database is aware of all
     * renames), we can synchronize maildir_flags to tags for all
     * messages that had new filenames appear on this run. */
    gettimeofday (&tv_start, NULL);
    if (add_files_state.synchronize_flags) {
	_filename_node_t *node;
	notmuch_message_t *message;
	for (node = add_files_state.message_ids_to_sync->head, i = 0;
	     node;
	     node = node->next, i++)
	{
	    message = notmuch_database_find_message (notmuch, node->filename);
	    notmuch_message_maildir_flags_to_tags (message);
	    notmuch_message_destroy (message);
	    if (do_print_progress) {
		do_print_progress = 0;
		generic_print_progress (
		    "Synchronized tags for", "messages",
		    tv_start, i, add_files_state.message_ids_to_sync->count);
	    }
	}
    }

    talloc_free (add_files_state.message_ids_to_sync);
    add_files_state.message_ids_to_sync = NULL;

    if (timer_is_active)
	stop_progress_printing_timer ();

    gettimeofday (&tv_now, NULL);
    elapsed = notmuch_time_elapsed (add_files_state.tv_start,
				    tv_now);

    if (add_files_state.processed_files) {
	printf ("Processed %d %s in ", add_files_state.processed_files,
		add_files_state.processed_files == 1 ?
		"file" : "total files");
	notmuch_time_print_formatted_seconds (elapsed);
	if (elapsed > 1) {
	    printf (" (%d files/sec.).\033[K\n",
		    (int) (add_files_state.processed_files / elapsed));
	} else {
	    printf (".\033[K\n");
	}
    }

    if (add_files_state.added_messages) {
	printf ("Added %d new %s to the database.",
		add_files_state.added_messages,
		add_files_state.added_messages == 1 ?
		"message" : "messages");
    } else {
	printf ("No new mail.");
    }

    if (removed_files) {
	printf (" Removed %d %s.",
		removed_files,
		removed_files == 1 ? "message" : "messages");
    }

    if (renamed_files) {
	printf (" Detected %d file %s.",
		renamed_files,
		renamed_files == 1 ? "rename" : "renames");
    }

    printf ("\n");

    if (ret) {
	printf ("\nNote: At least one error was encountered: %s\n",
		notmuch_status_to_string (ret));
    }

    notmuch_database_close (notmuch);

    return ret || interrupted;
}
