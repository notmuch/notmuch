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
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-client.h"
#include "tag-util.h"

#include <unistd.h>

typedef struct _filename_node {
    char *filename;
    time_t mtime;
    struct _filename_node *next;
} _filename_node_t;

typedef struct _filename_list {
    unsigned count;
    _filename_node_t *head;
    _filename_node_t **tail;
} _filename_list_t;

enum verbosity {
    VERBOSITY_QUIET,
    VERBOSITY_NORMAL,
    VERBOSITY_VERBOSE,
};

typedef struct {
    const char *db_path;

    int output_is_a_tty;
    enum verbosity verbosity;
    bool debug;
    bool full_scan;
    const char **new_tags;
    size_t new_tags_length;
    const char **ignore_verbatim;
    size_t ignore_verbatim_length;
    regex_t *ignore_regex;
    size_t ignore_regex_length;

    int total_files;
    int processed_files;
    int added_messages, removed_messages, renamed_messages;
    int vanished_files;
    struct timeval tv_start;

    _filename_list_t *removed_files;
    _filename_list_t *removed_directories;
    _filename_list_t *directory_mtimes;

    bool synchronize_flags;
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
    static char msg[] = "Stopping...         \n";

    /* This write is "opportunistic", so it's okay to ignore the
     * result.  It is not required for correctness, and if it does
     * fail or produce a short write, we want to get out of the signal
     * handler as quickly as possible, not retry it. */
    IGNORE_RESULT (write (2, msg, sizeof (msg) - 1));
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

static _filename_node_t *
_filename_list_add (_filename_list_t *list,
		    const char *filename)
{
    _filename_node_t *node = talloc (list, _filename_node_t);

    list->count++;

    node->filename = talloc_strdup (list, filename);
    node->next = NULL;

    *(list->tail) = node;
    list->tail = &node->next;

    return node;
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

    printf ("%s %u ", action, processed);

    if (total) {
	printf ("of %u %s", total, object);
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

/* Return the type of a directory entry relative to path as a stat(2)
 * mode.  Like stat, this follows symlinks.  Returns -1 and sets errno
 * if the file's type cannot be determined (which includes dangling
 * symlinks).
 */
static int
dirent_type (const char *path, const struct dirent *entry)
{
    struct stat statbuf;
    char *abspath;
    int err, saved_errno;

#if HAVE_D_TYPE
    /* Mapping from d_type to stat mode_t.  We omit DT_LNK so that
     * we'll fall through to stat and get the real file type. */
    static const mode_t modes[] = {
	[DT_BLK] = S_IFBLK,
	[DT_CHR] = S_IFCHR,
	[DT_DIR] = S_IFDIR,
	[DT_FIFO] = S_IFIFO,
	[DT_REG] = S_IFREG,
	[DT_SOCK] = S_IFSOCK
    };
    if (entry->d_type < ARRAY_SIZE (modes) && modes[entry->d_type])
	return modes[entry->d_type];
#endif

    abspath = talloc_asprintf (NULL, "%s/%s", path, entry->d_name);
    if (! abspath) {
	errno = ENOMEM;
	return -1;
    }
    err = stat (abspath, &statbuf);
    saved_errno = errno;
    talloc_free (abspath);
    if (err < 0) {
	errno = saved_errno;
	return -1;
    }
    return statbuf.st_mode & S_IFMT;
}

/* Test if the directory looks like a Maildir directory.
 *
 * Search through the array of directory entries to see if we can find all
 * three subdirectories typical for Maildir, that is "new", "cur", and "tmp".
 *
 * Return 1 if the directory looks like a Maildir and 0 otherwise.
 */
static int
_entries_resemble_maildir (const char *path, struct dirent **entries, int count)
{
    int i, found = 0;

    for (i = 0; i < count; i++) {
	if (dirent_type (path, entries[i]) != S_IFDIR)
	    continue;

	if (strcmp (entries[i]->d_name, "new") == 0 ||
	    strcmp (entries[i]->d_name, "cur") == 0 ||
	    strcmp (entries[i]->d_name, "tmp") == 0) {
	    found++;
	    if (found == 3)
		return 1;
	}
    }

    return 0;
}

static bool
_special_directory (const char *entry)
{
    return strcmp (entry, ".") == 0 || strcmp (entry, "..") == 0;
}

static bool
_setup_ignore (notmuch_config_t *config, add_files_state_t *state)
{
    const char **ignore_list, **ignore;
    int nregex = 0, nverbatim = 0;
    const char **verbatim = NULL;
    regex_t *regex = NULL;

    ignore_list = notmuch_config_get_new_ignore (config, NULL);
    if (! ignore_list)
	return true;

    for (ignore = ignore_list; *ignore; ignore++) {
	const char *s = *ignore;
	size_t len = strlen (s);

	if (len == 0) {
	    fprintf (stderr, "Error: Empty string in new.ignore list\n");
	    return false;
	}

	if (s[0] == '/') {
	    regex_t *preg;
	    char *r;
	    int rerr;

	    if (len < 3 || s[len - 1] != '/') {
		fprintf (stderr, "Error: Malformed pattern '%s' in new.ignore\n",
			 s);
		return false;
	    }

	    r = talloc_strndup (config, s + 1, len - 2);
	    regex = talloc_realloc (config, regex, regex_t, nregex + 1);
	    preg = &regex[nregex];

	    rerr = regcomp (preg, r, REG_EXTENDED | REG_NOSUB);
	    if (rerr) {
		size_t error_size = regerror (rerr, preg, NULL, 0);
		char *error = talloc_size (r, error_size);

		regerror (rerr, preg, error, error_size);

		fprintf (stderr, "Error: Invalid regex '%s' in new.ignore: %s\n",
			 r, error);
		return false;
	    }
	    nregex++;

	    talloc_free (r);
	} else {
	    verbatim = talloc_realloc (config, verbatim, const char *,
				       nverbatim + 1);
	    verbatim[nverbatim++] = s;
	}
    }

    state->ignore_regex = regex;
    state->ignore_regex_length = nregex;
    state->ignore_verbatim = verbatim;
    state->ignore_verbatim_length = nverbatim;

    return true;
}

static char *
_get_relative_path (const char *db_path, const char *dirpath, const char *entry)
{
    size_t db_path_len = strlen (db_path);

    /* paranoia? */
    if (strncmp (dirpath, db_path, db_path_len) != 0) {
	fprintf (stderr, "Warning: '%s' is not a subdirectory of '%s'\n",
		 dirpath, db_path);
	return NULL;
    }

    dirpath += db_path_len;
    while (*dirpath == '/')
	dirpath++;

    if (*dirpath)
	return talloc_asprintf (NULL, "%s/%s", dirpath, entry);
    else
	return talloc_strdup (NULL, entry);
}

/* Test if the file/directory is to be ignored.
 */
static bool
_entry_in_ignore_list (add_files_state_t *state, const char *dirpath,
		       const char *entry)
{
    bool ret = false;
    size_t i;
    char *path;

    for (i = 0; i < state->ignore_verbatim_length; i++) {
	if (strcmp (entry, state->ignore_verbatim[i]) == 0)
	    return true;
    }

    if (state->ignore_regex_length == 0)
	return false;

    path = _get_relative_path (state->db_path, dirpath, entry);
    if (! path)
	return false;

    for (i = 0; i < state->ignore_regex_length; i++) {
	if (regexec (&state->ignore_regex[i], path, 0, NULL, 0) == 0) {
	    ret = true;
	    break;
	}
    }

    talloc_free (path);

    return ret;
}

/* Add a single file to the database. */
static notmuch_status_t
add_file (notmuch_database_t *notmuch, const char *filename,
	  add_files_state_t *state)
{
    notmuch_message_t *message = NULL;
    const char **tag;
    notmuch_status_t status;

    status = notmuch_database_begin_atomic (notmuch);
    if (status)
	goto DONE;

    status = notmuch_database_index_file (notmuch, filename, indexing_cli_choices.opts, &message);
    switch (status) {
    /* Success. */
    case NOTMUCH_STATUS_SUCCESS:
	state->added_messages++;
	notmuch_message_freeze (message);
	if (state->synchronize_flags)
	    notmuch_message_maildir_flags_to_tags (message);

	for (tag = state->new_tags; *tag != NULL; tag++) {
	    notmuch_bool_t is_set;
	    /* Currently all errors from has_maildir_flag are fatal */
	    if ((status = notmuch_message_has_maildir_flag_st (message, 'S', &is_set)))
		goto DONE;
	    if (strcmp ("unread", *tag) != 0 || ! is_set) {
		notmuch_message_add_tag (message, *tag);
	    }
	}

	notmuch_message_thaw (message);
	break;
    /* Non-fatal issues (go on to next file). */
    case NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID:
	if (state->synchronize_flags)
	    notmuch_message_maildir_flags_to_tags (message);
	break;
    case NOTMUCH_STATUS_FILE_NOT_EMAIL:
	fprintf (stderr, "Note: Ignoring non-mail file: %s\n", filename);
	break;
    case NOTMUCH_STATUS_FILE_ERROR:
	/* Someone renamed/removed the file between scandir and now. */
	state->vanished_files++;
	fprintf (stderr, "Unexpected error with file %s\n", filename);
	(void) print_status_database ("add_file", notmuch, status);
	break;
    /* Fatal issues. Don't process anymore. */
    case NOTMUCH_STATUS_READ_ONLY_DATABASE:
    case NOTMUCH_STATUS_XAPIAN_EXCEPTION:
    case NOTMUCH_STATUS_OUT_OF_MEMORY:
	(void) print_status_database ("add_file", notmuch, status);
	goto DONE;
    default:
	INTERNAL_ERROR ("add_message returned unexpected value: %d", status);
	goto DONE;
    }

    status = notmuch_database_end_atomic (notmuch);

  DONE:
    if (message)
	notmuch_message_destroy (message);

    return status;
}

/* Examine 'path' recursively as follows:
 *
 *   o Ask the filesystem for the mtime of 'path' (fs_mtime)
 *   o Ask the database for its timestamp of 'path' (db_mtime)
 *
 *   o Ask the filesystem for files and directories within 'path'
 *     (via scandir and stored in fs_entries)
 *
 *   o Pass 1: For each directory in fs_entries, recursively call into
 *     this same function.
 *
 *   o Compare fs_mtime to db_mtime. If they are equivalent, terminate
 *     the algorithm at this point, (this directory has not been
 *     updated in the filesystem since the last database scan of PASS
 *     2).
 *
 *   o Ask the database for files and directories within 'path'
 *     (db_files and db_subdirs)
 *
 *   o Pass 2: Walk fs_entries simultaneously with db_files and
 *     db_subdirs. Look for one of three interesting cases:
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
 *     if fs_mtime isn't the current wall-clock time.
 */
static notmuch_status_t
add_files (notmuch_database_t *notmuch,
	   const char *path,
	   add_files_state_t *state)
{
    struct dirent *entry = NULL;
    char *next = NULL;
    time_t fs_mtime, db_mtime;
    notmuch_status_t status, ret = NOTMUCH_STATUS_SUCCESS;
    struct dirent **fs_entries = NULL;
    int i, num_fs_entries = 0, entry_type;
    notmuch_directory_t *directory;
    notmuch_filenames_t *db_files = NULL;
    notmuch_filenames_t *db_subdirs = NULL;
    time_t stat_time;
    struct stat st;
    bool is_maildir;

    if (stat (path, &st)) {
	fprintf (stderr, "Error reading directory %s: %s\n",
		 path, strerror (errno));
	return NOTMUCH_STATUS_FILE_ERROR;
    }
    stat_time = time (NULL);

    if (! S_ISDIR (st.st_mode)) {
	fprintf (stderr, "Error: %s is not a directory.\n", path);
	return NOTMUCH_STATUS_FILE_ERROR;
    }

    fs_mtime = st.st_mtime;

    status = notmuch_database_get_directory (notmuch, path, &directory);
    if (status) {
	ret = status;
	goto DONE;
    }
    db_mtime = directory ? notmuch_directory_get_mtime (directory) : 0;

    /* If the directory is unchanged from our last scan and has no
     * sub-directories, then return without scanning it at all.  In
     * some situations, skipping the scan can substantially reduce the
     * cost of notmuch new, especially since the huge numbers of files
     * in Maildirs make scans expensive, but all files live in leaf
     * directories.
     *
     * To check for sub-directories, we borrow a trick from find,
     * kpathsea, and many other UNIX tools: since a directory's link
     * count is the number of sub-directories (specifically, their
     * '..' entries) plus 2 (the link from the parent and the link for
     * '.').  This check is safe even on weird file systems, since
     * file systems that can't compute this will return 0 or 1.  This
     * is safe even on *really* weird file systems like HFS+ that
     * mistakenly return the total number of directory entries, since
     * that only inflates the count beyond 2.
     */
    if (directory && (! state->full_scan) && fs_mtime == db_mtime && st.st_nlink == 2) {
	/* There's one catch: pass 1 below considers symlinks to
	 * directories to be directories, but these don't increase the
	 * file system link count.  So, only bail early if the
	 * database agrees that there are no sub-directories. */
	db_subdirs = notmuch_directory_get_child_directories (directory);
	if (! notmuch_filenames_valid (db_subdirs))
	    goto DONE;
	notmuch_filenames_destroy (db_subdirs);
	db_subdirs = NULL;
    }

    /* If the database knows about this directory, then we sort based
     * on strcmp to match the database sorting. Otherwise, we can do
     * inode-based sorting for faster filesystem operation. */
    num_fs_entries = scandir (path, &fs_entries, 0,
			      directory ?
			      dirent_sort_strcmp_name : dirent_sort_inode);

    if (num_fs_entries == -1) {
	fprintf (stderr, "Error opening directory %s: %s\n",
		 path, strerror (errno));
	/* We consider this a fatal error because, if a user moved a
	 * message from another directory that we were able to scan
	 * into this directory, skipping this directory will cause
	 * that message to be lost. */
	ret = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    /* Pass 1: Recurse into all sub-directories. */
    is_maildir = _entries_resemble_maildir (path, fs_entries, num_fs_entries);

    for (i = 0; i < num_fs_entries && ! interrupted; i++) {
	entry = fs_entries[i];

	/* Ignore special directories to avoid infinite recursion. */
	if (_special_directory (entry->d_name))
	    continue;

	/* Ignore any files/directories the user has configured to
	 * ignore.  We do this before dirent_type both for performance
	 * and because we don't care if dirent_type fails on entries
	 * that are explicitly ignored.
	 */
	if (_entry_in_ignore_list (state, path, entry->d_name)) {
	    if (state->debug)
		printf ("(D) add_files, pass 1: explicitly ignoring %s/%s\n",
			path, entry->d_name);
	    continue;
	}

	/* We only want to descend into directories (and symlinks to
	 * directories). */
	entry_type = dirent_type (path, entry);
	if (entry_type == -1) {
	    /* Be pessimistic, e.g. so we don't lose lots of mail just
	     * because a user broke a symlink. */
	    fprintf (stderr, "Error reading file %s/%s: %s\n",
		     path, entry->d_name, strerror (errno));
	    return NOTMUCH_STATUS_FILE_ERROR;
	} else if (entry_type != S_IFDIR) {
	    continue;
	}

	/* Ignore the .notmuch directory and any "tmp" directory
	 * that appears within a maildir.
	 */
	if ((is_maildir && strcmp (entry->d_name, "tmp") == 0) ||
	    strcmp (entry->d_name, ".notmuch") == 0)
	    continue;

	next = talloc_asprintf (notmuch, "%s/%s", path, entry->d_name);
	status = add_files (notmuch, next, state);
	if (status) {
	    ret = status;
	    goto DONE;
	}
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
    if (directory && (! state->full_scan) && fs_mtime == db_mtime)
	goto DONE;

    /* If the database has never seen this directory before, we can
     * simply leave db_files and db_subdirs NULL. */
    if (directory) {
	db_files = notmuch_directory_get_child_files (directory);
	db_subdirs = notmuch_directory_get_child_directories (directory);
    }

    /* Pass 2: Scan for new files, removed files, and removed directories. */
    for (i = 0; i < num_fs_entries && ! interrupted; i++) {
	entry = fs_entries[i];

	/* Ignore special directories early. */
	if (_special_directory (entry->d_name))
	    continue;

	/* Ignore files & directories user has configured to be ignored */
	if (_entry_in_ignore_list (state, path, entry->d_name)) {
	    if (state->debug)
		printf ("(D) add_files, pass 2: explicitly ignoring %s/%s\n",
			path, entry->d_name);
	    continue;
	}

	/* Check if we've walked past any names in db_files or
	 * db_subdirs. If so, these have been deleted. */
	while (notmuch_filenames_valid (db_files) &&
	       strcmp (notmuch_filenames_get (db_files), entry->d_name) < 0) {
	    char *absolute = talloc_asprintf (state->removed_files,
					      "%s/%s", path,
					      notmuch_filenames_get (db_files));

	    if (state->debug)
		printf ("(D) add_files, pass 2: queuing passed file %s for deletion from database\n",
			absolute);

	    _filename_list_add (state->removed_files, absolute);

	    notmuch_filenames_move_to_next (db_files);
	}

	while (notmuch_filenames_valid (db_subdirs) &&
	       strcmp (notmuch_filenames_get (db_subdirs), entry->d_name) <= 0) {
	    const char *filename = notmuch_filenames_get (db_subdirs);

	    if (strcmp (filename, entry->d_name) < 0) {
		char *absolute = talloc_asprintf (state->removed_directories,
						  "%s/%s", path, filename);
		if (state->debug)
		    printf ("(D) add_files, pass 2: queuing passed directory %s for deletion from database\n",
			    absolute);

		_filename_list_add (state->removed_directories, absolute);
	    }

	    notmuch_filenames_move_to_next (db_subdirs);
	}

	/* Only add regular files (and symlinks to regular files). */
	entry_type = dirent_type (path, entry);
	if (entry_type == -1) {
	    fprintf (stderr, "Error reading file %s/%s: %s\n",
		     path, entry->d_name, strerror (errno));
	    return NOTMUCH_STATUS_FILE_ERROR;
	} else if (entry_type != S_IFREG) {
	    continue;
	}

	/* Don't add a file that we've added before. */
	if (notmuch_filenames_valid (db_files) &&
	    strcmp (notmuch_filenames_get (db_files), entry->d_name) == 0) {
	    notmuch_filenames_move_to_next (db_files);
	    continue;
	}

	/* We're now looking at a regular file that doesn't yet exist
	 * in the database, so add it. */
	next = talloc_asprintf (notmuch, "%s/%s", path, entry->d_name);

	state->processed_files++;

	if (state->verbosity >= VERBOSITY_VERBOSE) {
	    if (state->output_is_a_tty)
		printf ("\r\033[K");

	    printf ("%i/%i: %s", state->processed_files, state->total_files,
		    next);

	    putchar ((state->output_is_a_tty) ? '\r' : '\n');
	    fflush (stdout);
	}

	status = add_file (notmuch, next, state);
	if (status) {
	    ret = status;
	    goto DONE;
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
    while (notmuch_filenames_valid (db_files)) {
	char *absolute = talloc_asprintf (state->removed_files,
					  "%s/%s", path,
					  notmuch_filenames_get (db_files));
	if (state->debug)
	    printf ("(D) add_files, pass 3: queuing leftover file %s for deletion from database\n",
		    absolute);

	_filename_list_add (state->removed_files, absolute);

	notmuch_filenames_move_to_next (db_files);
    }

    while (notmuch_filenames_valid (db_subdirs)) {
	char *absolute = talloc_asprintf (state->removed_directories,
					  "%s/%s", path,
					  notmuch_filenames_get (db_subdirs));

	if (state->debug)
	    printf ("(D) add_files, pass 3: queuing leftover directory %s for deletion from database\n",
		    absolute);

	_filename_list_add (state->removed_directories, absolute);

	notmuch_filenames_move_to_next (db_subdirs);
    }

    /* If the directory's mtime is the same as the wall-clock time
     * when we stat'ed the directory, we skip updating the mtime in
     * the database because a message could be delivered later in this
     * same second.  This may lead to unnecessary re-scans, but it
     * avoids overlooking messages. */
    if (fs_mtime != stat_time)
	_filename_list_add (state->directory_mtimes, path)->mtime = fs_mtime;

  DONE:
    if (next)
	talloc_free (next);
    if (fs_entries) {
	for (i = 0; i < num_fs_entries; i++)
	    free (fs_entries[i]);

	free (fs_entries);
    }
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

    /* Set up our handler for SIGALRM */
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


/* XXX: This should be merged with the add_files function since it
 * shares a lot of logic with it. */
/* Recursively count all regular files in path and all sub-directories
 * of path.  The result is added to *count (which should be
 * initialized to zero by the top-level caller before calling
 * count_files). */
static void
count_files (const char *path, int *count, add_files_state_t *state)
{
    struct dirent *entry = NULL;
    char *next;
    struct dirent **fs_entries = NULL;
    int num_fs_entries = scandir (path, &fs_entries, 0, dirent_sort_inode);
    int entry_type, i;

    if (num_fs_entries == -1) {
	fprintf (stderr, "Warning: failed to open directory %s: %s\n",
		 path, strerror (errno));
	goto DONE;
    }

    for (i = 0; i < num_fs_entries && ! interrupted; i++) {
	entry = fs_entries[i];

	/* Ignore special directories to avoid infinite recursion.
	 * Also ignore the .notmuch directory.
	 */
	if (_special_directory (entry->d_name) ||
	    strcmp (entry->d_name, ".notmuch") == 0)
	    continue;

	/* Ignore any files/directories the user has configured to be
	 * ignored
	 */
	if (_entry_in_ignore_list (state, path, entry->d_name)) {
	    if (state->debug)
		printf ("(D) count_files: explicitly ignoring %s/%s\n",
			path, entry->d_name);
	    continue;
	}

	if (asprintf (&next, "%s/%s", path, entry->d_name) == -1) {
	    next = NULL;
	    fprintf (stderr, "Error descending from %s to %s: Out of memory\n",
		     path, entry->d_name);
	    continue;
	}

	entry_type = dirent_type (path, entry);
	if (entry_type == S_IFREG) {
	    *count = *count + 1;
	    if (*count % 1000 == 0 && state->verbosity >= VERBOSITY_NORMAL) {
		printf ("Found %d files so far.\r", *count);
		fflush (stdout);
	    }
	} else if (entry_type == S_IFDIR) {
	    count_files (next, count, state);
	}

	free (next);
    }

  DONE:
    if (fs_entries) {
	for (i = 0; i < num_fs_entries; i++)
	    free (fs_entries[i]);

	free (fs_entries);
    }
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

/* Remove one message filename from the database. */
static notmuch_status_t
remove_filename (notmuch_database_t *notmuch,
		 const char *path,
		 add_files_state_t *add_files_state)
{
    notmuch_status_t status;
    notmuch_message_t *message;

    status = notmuch_database_begin_atomic (notmuch);
    if (status)
	return status;
    status = notmuch_database_find_message_by_filename (notmuch, path, &message);
    if (status || message == NULL)
	goto DONE;

    status = notmuch_database_remove_message (notmuch, path);
    if (status == NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID) {
	add_files_state->renamed_messages++;
	if (add_files_state->synchronize_flags == true)
	    notmuch_message_maildir_flags_to_tags (message);
	status = NOTMUCH_STATUS_SUCCESS;
    } else if (status == NOTMUCH_STATUS_SUCCESS) {
	add_files_state->removed_messages++;
    }
    notmuch_message_destroy (message);

  DONE:
    notmuch_database_end_atomic (notmuch);
    return status;
}

/* Recursively remove all filenames from the database referring to
 * 'path' (or to any of its children). */
static notmuch_status_t
_remove_directory (void *ctx,
		   notmuch_database_t *notmuch,
		   const char *path,
		   add_files_state_t *add_files_state)
{
    notmuch_status_t status;
    notmuch_directory_t *directory;
    notmuch_filenames_t *files, *subdirs;
    char *absolute;

    status = notmuch_database_get_directory (notmuch, path, &directory);
    if (status || ! directory)
	return status;

    for (files = notmuch_directory_get_child_files (directory);
	 notmuch_filenames_valid (files);
	 notmuch_filenames_move_to_next (files)) {
	absolute = talloc_asprintf (ctx, "%s/%s", path,
				    notmuch_filenames_get (files));
	status = remove_filename (notmuch, absolute, add_files_state);
	talloc_free (absolute);
	if (status)
	    goto DONE;
    }

    for (subdirs = notmuch_directory_get_child_directories (directory);
	 notmuch_filenames_valid (subdirs);
	 notmuch_filenames_move_to_next (subdirs)) {
	absolute = talloc_asprintf (ctx, "%s/%s", path,
				    notmuch_filenames_get (subdirs));
	status = _remove_directory (ctx, notmuch, absolute, add_files_state);
	talloc_free (absolute);
	if (status)
	    goto DONE;
    }

    status = notmuch_directory_delete (directory);

  DONE:
    if (status)
	notmuch_directory_destroy (directory);
    return status;
}

static void
print_results (const add_files_state_t *state)
{
    double elapsed;
    struct timeval tv_now;

    gettimeofday (&tv_now, NULL);
    elapsed = notmuch_time_elapsed (state->tv_start, tv_now);

    if (state->processed_files) {
	printf ("Processed %d %s in ", state->processed_files,
		state->processed_files == 1 ? "file" : "total files");
	notmuch_time_print_formatted_seconds (elapsed);
	if (elapsed > 1)
	    printf (" (%d files/sec.)",
		    (int) (state->processed_files / elapsed));
	printf (".%s\n", (state->output_is_a_tty) ? "\033[K" : "");
    }

    if (state->added_messages)
	printf ("Added %d new %s to the database.", state->added_messages,
		state->added_messages == 1 ? "message" : "messages");
    else
	printf ("No new mail.");

    if (state->removed_messages)
	printf (" Removed %d %s.", state->removed_messages,
		state->removed_messages == 1 ? "message" : "messages");

    if (state->renamed_messages)
	printf (" Detected %d file %s.", state->renamed_messages,
		state->renamed_messages == 1 ? "rename" : "renames");

    printf ("\n");
}

int
notmuch_new_command (notmuch_config_t *config, unused(notmuch_database_t *notmuch), int argc, char *argv[])
{
    notmuch_database_t *notmuch;
    add_files_state_t add_files_state = {
	.verbosity = VERBOSITY_NORMAL,
	.debug = false,
	.full_scan = false,
	.output_is_a_tty = isatty (fileno (stdout)),
    };
    struct timeval tv_start;
    int ret = 0;
    struct stat st;
    const char *db_path;
    char *dot_notmuch_path;
    struct sigaction action;
    _filename_node_t *f;
    int opt_index;
    unsigned int i;
    bool timer_is_active = false;
    bool hooks = true;
    bool quiet = false, verbose = false;
    notmuch_status_t status;

    notmuch_opt_desc_t options[] = {
	{ .opt_bool = &quiet, .name = "quiet" },
	{ .opt_bool = &verbose, .name = "verbose" },
	{ .opt_bool = &add_files_state.debug, .name = "debug" },
	{ .opt_bool = &add_files_state.full_scan, .name = "full-scan" },
	{ .opt_bool = &hooks, .name = "hooks" },
	{ .opt_inherit = notmuch_shared_indexing_options },
	{ .opt_inherit = notmuch_shared_options },
	{ }
    };

    opt_index = parse_arguments (argc, argv, options, 1);
    if (opt_index < 0)
	return EXIT_FAILURE;

    notmuch_process_shared_options (argv[0]);

    /* quiet trumps verbose */
    if (quiet)
	add_files_state.verbosity = VERBOSITY_QUIET;
    else if (verbose)
	add_files_state.verbosity = VERBOSITY_VERBOSE;

    add_files_state.new_tags = notmuch_config_get_new_tags (config, &add_files_state.new_tags_length);
    add_files_state.synchronize_flags = notmuch_config_get_maildir_synchronize_flags (config);
    db_path = notmuch_config_get_database_path (config);
    add_files_state.db_path = db_path;

    if (! _setup_ignore (config, &add_files_state))
	return EXIT_FAILURE;

    for (i = 0; i < add_files_state.new_tags_length; i++) {
	const char *error_msg;

	error_msg = illegal_tag (add_files_state.new_tags[i], false);
	if (error_msg) {
	    fprintf (stderr, "Error: tag '%s' in new.tags: %s\n",
		     add_files_state.new_tags[i], error_msg);
	    return EXIT_FAILURE;
	}
    }

    if (hooks) {
	ret = notmuch_run_hook (db_path, "pre-new");
	if (ret)
	    return EXIT_FAILURE;
    }

    dot_notmuch_path = talloc_asprintf (config, "%s/%s", db_path, ".notmuch");

    if (stat (dot_notmuch_path, &st)) {
	int count;

	count = 0;
	count_files (db_path, &count, &add_files_state);
	if (interrupted)
	    return EXIT_FAILURE;

	if (add_files_state.verbosity >= VERBOSITY_NORMAL)
	    printf ("Found %d total files (that's not much mail).\n", count);
	if (notmuch_database_create (db_path, &notmuch))
	    return EXIT_FAILURE;
	add_files_state.total_files = count;
    } else {
	char *status_string = NULL;
	if (notmuch_database_open_verbose (db_path, NOTMUCH_DATABASE_MODE_READ_WRITE,
					   &notmuch, &status_string)) {
	    if (status_string) {
		fputs (status_string, stderr);
		free (status_string);
	    }
	    return EXIT_FAILURE;
	}

	notmuch_exit_if_unmatched_db_uuid (notmuch);

	if (notmuch_database_needs_upgrade (notmuch)) {
	    time_t now = time (NULL);
	    struct tm *gm_time = gmtime (&now);

	    /* since dump files are written atomically, the amount of
	     * harm from overwriting one within a second seems
	     * relatively small. */

	    const char *backup_name =
		talloc_asprintf (notmuch, "%s/dump-%04d%02d%02dT%02d%02d%02d.gz",
				 dot_notmuch_path,
				 gm_time->tm_year + 1900,
				 gm_time->tm_mon + 1,
				 gm_time->tm_mday,
				 gm_time->tm_hour,
				 gm_time->tm_min,
				 gm_time->tm_sec);

	    if (add_files_state.verbosity >= VERBOSITY_NORMAL) {
		printf ("Welcome to a new version of notmuch! Your database will now be upgraded.\n");
		printf ("This process is safe to interrupt.\n");
		printf ("Backing up tags to %s...\n", backup_name);
	    }

	    if (notmuch_database_dump (notmuch, backup_name, "",
				       DUMP_FORMAT_BATCH_TAG, DUMP_INCLUDE_DEFAULT, true)) {
		fprintf (stderr, "Backup failed. Aborting upgrade.");
		return EXIT_FAILURE;
	    }

	    gettimeofday (&add_files_state.tv_start, NULL);
	    status = notmuch_database_upgrade (
		notmuch,
		add_files_state.verbosity >= VERBOSITY_NORMAL ? upgrade_print_progress : NULL,
		&add_files_state);
	    if (status) {
		printf ("Upgrade failed: %s\n",
			notmuch_status_to_string (status));
		notmuch_database_destroy (notmuch);
		return EXIT_FAILURE;
	    }
	    if (add_files_state.verbosity >= VERBOSITY_NORMAL)
		printf ("Your notmuch database has now been upgraded.\n");
	}

	add_files_state.total_files = 0;
    }

    if (notmuch == NULL)
	return EXIT_FAILURE;

    status = notmuch_process_shared_indexing_options (notmuch);
    if (status != NOTMUCH_STATUS_SUCCESS) {
	fprintf (stderr, "Error: Failed to process index options. (%s)\n",
		 notmuch_status_to_string (status));
	return EXIT_FAILURE;
    }

    /* Set up our handler for SIGINT. We do this after having
     * potentially done a database upgrade we this interrupt handler
     * won't support. */
    memset (&action, 0, sizeof (struct sigaction));
    action.sa_handler = handle_sigint;
    sigemptyset (&action.sa_mask);
    action.sa_flags = SA_RESTART;
    sigaction (SIGINT, &action, NULL);

    talloc_free (dot_notmuch_path);
    dot_notmuch_path = NULL;

    gettimeofday (&add_files_state.tv_start, NULL);

    add_files_state.removed_files = _filename_list_create (config);
    add_files_state.removed_directories = _filename_list_create (config);
    add_files_state.directory_mtimes = _filename_list_create (config);

    if (add_files_state.verbosity == VERBOSITY_NORMAL &&
	add_files_state.output_is_a_tty && ! debugger_is_active ()) {
	setup_progress_printing_timer ();
	timer_is_active = true;
    }

    ret = add_files (notmuch, db_path, &add_files_state);
    if (ret)
	goto DONE;

    gettimeofday (&tv_start, NULL);
    for (f = add_files_state.removed_files->head; f && ! interrupted; f = f->next) {
	ret = remove_filename (notmuch, f->filename, &add_files_state);
	if (ret)
	    goto DONE;
	if (do_print_progress) {
	    do_print_progress = 0;
	    generic_print_progress ("Cleaned up", "messages",
				    tv_start, add_files_state.removed_messages + add_files_state.renamed_messages,
				    add_files_state.removed_files->count);
	}
    }

    gettimeofday (&tv_start, NULL);
    for (f = add_files_state.removed_directories->head, i = 0; f && ! interrupted; f = f->next, i++) {
	ret = _remove_directory (config, notmuch, f->filename, &add_files_state);
	if (ret)
	    goto DONE;
	if (do_print_progress) {
	    do_print_progress = 0;
	    generic_print_progress ("Cleaned up", "directories",
				    tv_start, i,
				    add_files_state.removed_directories->count);
	}
    }

    for (f = add_files_state.directory_mtimes->head; f && ! interrupted; f = f->next) {
	notmuch_directory_t *directory;
	status = notmuch_database_get_directory (notmuch, f->filename, &directory);
	if (status == NOTMUCH_STATUS_SUCCESS && directory) {
	    notmuch_directory_set_mtime (directory, f->mtime);
	    notmuch_directory_destroy (directory);
	}
    }

  DONE:
    talloc_free (add_files_state.removed_files);
    talloc_free (add_files_state.removed_directories);
    talloc_free (add_files_state.directory_mtimes);

    if (timer_is_active)
	stop_progress_printing_timer ();

    if (add_files_state.verbosity >= VERBOSITY_NORMAL)
	print_results (&add_files_state);

    if (ret)
	fprintf (stderr, "Note: A fatal error was encountered: %s\n",
		 notmuch_status_to_string (ret));

    notmuch_database_destroy (notmuch);

    if (hooks && ! ret && ! interrupted)
	ret = notmuch_run_hook (db_path, "post-new");

    if (ret || interrupted)
	return EXIT_FAILURE;

    if (add_files_state.vanished_files)
	return NOTMUCH_EXIT_TEMPFAIL;

    return EXIT_SUCCESS;
}
