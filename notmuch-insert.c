/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2013 Peter Wang
 *
 * Based in part on notmuch-deliver
 * Copyright © 2010 Ali Polatel
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
 * Author: Peter Wang <novalazy@gmail.com>
 */

#include "notmuch-client.h"
#include "tag-util.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "string-util.h"

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

/* Like gethostname but guarantees that a null-terminated hostname is
 * returned, even if it has to make one up. Invalid characters are
 * substituted such that the hostname can be used within a filename.
 */
static void
safe_gethostname (char *hostname, size_t len)
{
    char *p;

    if (gethostname (hostname, len) == -1) {
	strncpy (hostname, "unknown", len);
    }
    hostname[len - 1] = '\0';

    for (p = hostname; *p != '\0'; p++) {
	if (*p == '/' || *p == ':')
	    *p = '_';
    }
}

/* Call fsync() on a directory path. */
static bool
sync_dir (const char *dir)
{
    int fd, r;

    fd = open (dir, O_RDONLY);
    if (fd == -1) {
	fprintf (stderr, "Error: open %s: %s\n", dir, strerror (errno));
	return false;
    }

    r = fsync (fd);
    if (r)
	fprintf (stderr, "Error: fsync %s: %s\n", dir, strerror (errno));

    close (fd);

    return r == 0;
}

/*
 * Check the specified folder name does not contain a directory
 * component ".." to prevent writes outside of the Maildir
 * hierarchy. Return true on valid folder name, false otherwise.
 */
static bool
is_valid_folder_name (const char *folder)
{
    const char *p = folder;

    for (;;) {
	if ((p[0] == '.') && (p[1] == '.') && (p[2] == '\0' || p[2] == '/'))
	    return false;
	p = strchr (p, '/');
	if (! p)
	    return true;
	p++;
    }
}

/*
 * Make the given directory and its parents as necessary, using the
 * given mode. Return true on success, false otherwise. Partial
 * results are not cleaned up on errors.
 */
static bool
mkdir_recursive (const void *ctx, const char *path, int mode)
{
    struct stat st;
    int r;
    char *parent = NULL, *slash;

    /* First check the common case: directory already exists. */
    r = stat (path, &st);
    if (r == 0) {
	if (! S_ISDIR (st.st_mode)) {
	    fprintf (stderr, "Error: '%s' is not a directory: %s\n",
		     path, strerror (EEXIST));
	    return false;
	}

	return true;
    } else if (errno != ENOENT) {
	fprintf (stderr, "Error: stat '%s': %s\n", path, strerror (errno));
	return false;
    }

    /* mkdir parents, if any */
    slash = strrchr (path, '/');
    if (slash && slash != path) {
	parent = talloc_strndup (ctx, path, slash - path);
	if (! parent) {
	    fprintf (stderr, "Error: %s\n", strerror (ENOMEM));
	    return false;
	}

	if (! mkdir_recursive (ctx, parent, mode))
	    return false;
    }

    if (mkdir (path, mode)) {
	fprintf (stderr, "Error: mkdir '%s': %s\n", path, strerror (errno));
	return false;
    }

    return parent ? sync_dir (parent) : true;
}

/*
 * Create the given maildir folder, i.e. maildir and its
 * subdirectories cur/new/tmp. Return true on success, false
 * otherwise. Partial results are not cleaned up on errors.
 */
static bool
maildir_create_folder (const void *ctx, const char *maildir, bool world_readable)
{
    const char *subdirs[] = { "cur", "new", "tmp" };
    const int mode = (world_readable ? 0755 : 0700);
    char *subdir;
    unsigned int i;

    for (i = 0; i < ARRAY_SIZE (subdirs); i++) {
	subdir = talloc_asprintf (ctx, "%s/%s", maildir, subdirs[i]);
	if (! subdir) {
	    fprintf (stderr, "Error: %s\n", strerror (ENOMEM));
	    return false;
	}

	if (! mkdir_recursive (ctx, subdir, mode))
	    return false;
    }

    return true;
}

/*
 * Generate a temporary file basename, no path, do not create an
 * actual file. Return the basename, or NULL on errors.
 */
static char *
tempfilename (const void *ctx)
{
    char *filename;
    char hostname[256];
    struct timeval tv;
    pid_t pid;

    /* We follow the Dovecot file name generation algorithm. */
    pid = getpid ();
    safe_gethostname (hostname, sizeof (hostname));
    gettimeofday (&tv, NULL);

    filename = talloc_asprintf (ctx, "%ld.M%ldP%d.%s",
				(long) tv.tv_sec, (long) tv.tv_usec, pid, hostname);
    if (! filename)
	fprintf (stderr, "Error: %s\n", strerror (ENOMEM));

    return filename;
}

/*
 * Create a unique temporary file in maildir/tmp, return fd and full
 * path to file in *path_out, or -1 on errors (in which case *path_out
 * is not touched).
 */
static int
maildir_mktemp (const void *ctx, const char *maildir, bool world_readable, char **path_out)
{
    char *filename, *path;
    int fd;
    const int mode = (world_readable ? 0644 : 0600);

    do {
	filename = tempfilename (ctx);
	if (! filename)
	    return -1;

	path = talloc_asprintf (ctx, "%s/tmp/%s", maildir, filename);
	if (! path) {
	    fprintf (stderr, "Error: %s\n", strerror (ENOMEM));
	    return -1;
	}

	fd = open (path, O_WRONLY | O_CREAT | O_TRUNC | O_EXCL, mode);
    } while (fd == -1 && errno == EEXIST);

    if (fd == -1) {
	fprintf (stderr, "Error: open '%s': %s\n", path, strerror (errno));
	return -1;
    }

    *path_out = path;

    return fd;
}

/*
 * Copy fdin to fdout, return true on success, and false on errors and
 * empty input.
 */
static bool
copy_fd (int fdout, int fdin)
{
    bool empty = true;

    while (! interrupted) {
	ssize_t remain;
	char buf[4096];
	char *p;

	remain = read (fdin, buf, sizeof (buf));
	if (remain == 0)
	    break;
	if (remain < 0) {
	    if (errno == EINTR)
		continue;
	    fprintf (stderr, "Error: reading from standard input: %s\n",
		     strerror (errno));
	    return false;
	}

	p = buf;
	do {
	    ssize_t written = write (fdout, p, remain);
	    if (written < 0 && errno == EINTR)
		continue;
	    if (written <= 0) {
		fprintf (stderr, "Error: writing to temporary file: %s",
			 strerror (errno));
		return false;
	    }
	    p += written;
	    remain -= written;
	    empty = false;
	} while (remain > 0);
    }

    return (! interrupted && ! empty);
}

/*
 * Write fdin to a new temp file in maildir/tmp, return full path to
 * the file, or NULL on errors.
 */
static char *
maildir_write_tmp (const void *ctx, int fdin, const char *maildir, bool world_readable)
{
    char *path;
    int fdout;

    fdout = maildir_mktemp (ctx, maildir, world_readable, &path);
    if (fdout < 0)
	return NULL;

    if (! copy_fd (fdout, fdin))
	goto FAIL;

    if (fsync (fdout)) {
	fprintf (stderr, "Error: fsync '%s': %s\n", path, strerror (errno));
	goto FAIL;
    }

    close (fdout);

    return path;

  FAIL:
    close (fdout);
    unlink (path);

    return NULL;
}

/*
 * Write fdin to a new file in maildir/new, using an intermediate temp
 * file in maildir/tmp, return full path to the new file, or NULL on
 * errors.
 */
static char *
maildir_write_new (const void *ctx, int fdin, const char *maildir, bool world_readable)
{
    char *cleanpath, *tmppath, *newpath, *newdir;

    tmppath = maildir_write_tmp (ctx, fdin, maildir, world_readable);
    if (! tmppath)
	return NULL;
    cleanpath = tmppath;

    newpath = talloc_strdup (ctx, tmppath);
    if (! newpath) {
	fprintf (stderr, "Error: %s\n", strerror (ENOMEM));
	goto FAIL;
    }

    /* sanity checks needed? */
    memcpy (newpath + strlen (maildir) + 1, "new", 3);

    if (rename (tmppath, newpath)) {
	fprintf (stderr, "Error: rename '%s' '%s': %s\n",
		 tmppath, newpath, strerror (errno));
	goto FAIL;
    }
    cleanpath = newpath;

    newdir = talloc_asprintf (ctx, "%s/%s", maildir, "new");
    if (! newdir) {
	fprintf (stderr, "Error: %s\n", strerror (ENOMEM));
	goto FAIL;
    }

    if (! sync_dir (newdir))
	goto FAIL;

    return newpath;

  FAIL:
    unlink (cleanpath);

    return NULL;
}

/*
 * Add the specified message file to the notmuch database, applying
 * tags in tag_ops. If synchronize_flags is true, the tags are
 * synchronized to maildir flags (which may result in message file
 * rename).
 *
 * Return NOTMUCH_STATUS_SUCCESS on success, errors otherwise. If keep
 * is true, errors in tag changes and flag syncing are ignored and
 * success status is returned; otherwise such errors cause the message
 * to be removed from the database. Failure to add the message to the
 * database results in error status regardless of keep.
 */
static notmuch_status_t
add_file (notmuch_database_t *notmuch, const char *path, tag_op_list_t *tag_ops,
	  bool synchronize_flags, bool keep,
	  notmuch_indexopts_t *indexopts)
{
    notmuch_message_t *message;
    notmuch_status_t status;

    status = notmuch_database_index_file (notmuch, path, indexopts, &message);
    if (status == NOTMUCH_STATUS_SUCCESS) {
	status = tag_op_list_apply (message, tag_ops, 0);
	if (status) {
	    fprintf (stderr, "%s: failed to apply tags to file '%s': %s\n",
		     keep ? "Warning" : "Error",
		     path, notmuch_status_to_string (status));
	    goto DONE;
	}
    } else if (status == NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID) {
	status = NOTMUCH_STATUS_SUCCESS;
    } else if (status == NOTMUCH_STATUS_FILE_NOT_EMAIL) {
	fprintf (stderr, "Error: delivery of non-mail file: '%s'\n", path);
	goto FAIL;
    } else {
	fprintf (stderr, "Error: failed to add '%s' to notmuch database: %s\n",
		 path, notmuch_status_to_string (status));
	goto FAIL;
    }

    if (synchronize_flags) {
	status = notmuch_message_tags_to_maildir_flags (message);
	if (status != NOTMUCH_STATUS_SUCCESS)
	    fprintf (stderr, "%s: failed to sync tags to maildir flags for '%s': %s\n",
		     keep ? "Warning" : "Error",
		     path, notmuch_status_to_string (status));

	/*
	 * Note: Unfortunately a failed maildir flag sync might
	 * already have renamed the file, in which case the cleanup
	 * path may fail.
	 */
    }

  DONE:
    notmuch_message_destroy (message);

    if (status) {
	if (keep) {
	    status = NOTMUCH_STATUS_SUCCESS;
	} else {
	    notmuch_status_t cleanup_status;

	    cleanup_status = notmuch_database_remove_message (notmuch, path);
	    if (cleanup_status != NOTMUCH_STATUS_SUCCESS &&
		cleanup_status != NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID) {
		fprintf (stderr, "Warning: failed to remove '%s' from database "
			 "after errors: %s. Please run 'notmuch new' to fix.\n",
			 path, notmuch_status_to_string (cleanup_status));
	    }
	}
    }

  FAIL:
    return status;
}

int
notmuch_insert_command (unused(notmuch_config_t *config),notmuch_database_t *notmuch, int argc, char *argv[])
{
    notmuch_status_t status, close_status;
    struct sigaction action;
    const char *db_path;
    notmuch_config_values_t *new_tags = NULL;
    tag_op_list_t *tag_ops;
    char *query_string = NULL;
    const char *folder = "";
    bool create_folder = false;
    bool keep = false;
    bool hooks = true;
    bool world_readable = false;
    notmuch_bool_t synchronize_flags;
    char *maildir;
    char *newpath;
    int opt_index;
    void *local = talloc_new (NULL);

    notmuch_opt_desc_t options[] = {
	{ .opt_string = &folder, .name = "folder", .allow_empty = true },
	{ .opt_bool = &create_folder, .name = "create-folder" },
	{ .opt_bool = &keep, .name = "keep" },
	{ .opt_bool = &hooks, .name = "hooks" },
	{ .opt_bool = &world_readable, .name = "world-readable" },
	{ .opt_inherit = notmuch_shared_indexing_options },
	{ .opt_inherit = notmuch_shared_options },
	{ }
    };

    opt_index = parse_arguments (argc, argv, options, 1);
    if (opt_index < 0)
	return EXIT_FAILURE;

    notmuch_process_shared_options (argv[0]);


    /* XXX TODO replace this use of DATABASE_PATH with something specific to hooks */
    db_path = notmuch_config_get (notmuch, NOTMUCH_CONFIG_DATABASE_PATH);

    if (! db_path)
	INTERNAL_ERROR ("Unable to retrieve database path");
    else
	db_path = talloc_strdup (local, db_path);

    new_tags = notmuch_config_get_values (notmuch, NOTMUCH_CONFIG_NEW_TAGS);

    if (print_status_database (
	    "notmuch insert",
	    notmuch,
	    notmuch_config_get_bool (notmuch, NOTMUCH_CONFIG_SYNC_MAILDIR_FLAGS,
				     &synchronize_flags)))
	return EXIT_FAILURE;

    tag_ops = tag_op_list_create (local);
    if (tag_ops == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return EXIT_FAILURE;
    }
    for (;
	 notmuch_config_values_valid (new_tags);
	 notmuch_config_values_move_to_next (new_tags)) {
	const char *error_msg;
	const char *tag = notmuch_config_values_get (new_tags);
	error_msg = illegal_tag (tag, false);
	if (error_msg) {
	    fprintf (stderr, "Error: tag '%s' in new.tags: %s\n",
		     tag,  error_msg);
	    return EXIT_FAILURE;
	}

	if (tag_op_list_append (tag_ops, tag, false))
	    return EXIT_FAILURE;
    }

    if (parse_tag_command_line (local, argc - opt_index, argv + opt_index,
				&query_string, tag_ops))
	return EXIT_FAILURE;

    if (*query_string != '\0') {
	fprintf (stderr, "Error: unexpected query string: %s\n", query_string);
	return EXIT_FAILURE;
    }

    if (! is_valid_folder_name (folder)) {
	fprintf (stderr, "Error: invalid folder name: '%s'\n", folder);
	return EXIT_FAILURE;
    }

    maildir = talloc_asprintf (local, "%s/%s", db_path, folder);
    if (! maildir) {
	fprintf (stderr, "Out of memory\n");
	return EXIT_FAILURE;
    }

    strip_trailing (maildir, '/');
    if (create_folder && ! maildir_create_folder (local, maildir, world_readable))
	return EXIT_FAILURE;

    /* Set up our handler for SIGINT. We do not set SA_RESTART so that copying
     * from standard input may be interrupted. */
    memset (&action, 0, sizeof (struct sigaction));
    action.sa_handler = handle_sigint;
    sigemptyset (&action.sa_mask);
    action.sa_flags = 0;
    sigaction (SIGINT, &action, NULL);

    /* Write the message to the Maildir new directory. */
    newpath = maildir_write_new (local, STDIN_FILENO, maildir, world_readable);
    if (! newpath) {
	return EXIT_FAILURE;
    }

    notmuch_exit_if_unmatched_db_uuid (notmuch);

    status = notmuch_process_shared_indexing_options (notmuch);
    if (status != NOTMUCH_STATUS_SUCCESS) {
	fprintf (stderr, "Error: Failed to process index options. (%s)\n",
		 notmuch_status_to_string (status));
	return EXIT_FAILURE;
    }

    /* Index the message. */
    status = add_file (notmuch, newpath, tag_ops, synchronize_flags, keep, indexing_cli_choices.opts);

    /* Commit changes. */
    close_status = notmuch_database_destroy (notmuch);
    if (close_status) {
	/* Hold on to the first error, if any. */
	if (! status)
	    status = close_status;
	fprintf (stderr, "%s: failed to commit database changes: %s\n",
		 keep ? "Warning" : "Error",
		 notmuch_status_to_string (close_status));
    }

    if (status) {
	if (keep) {
	    status = NOTMUCH_STATUS_SUCCESS;
	} else {
	    /* If maildir flag sync failed, this might fail. */
	    if (unlink (newpath)) {
		fprintf (stderr, "Warning: failed to remove '%s' from maildir "
			 "after errors: %s. Please run 'notmuch new' to fix.\n",
			 newpath, strerror (errno));
	    }
	}
    }

    if (hooks && status == NOTMUCH_STATUS_SUCCESS) {
	/* Ignore hook failures. */
	notmuch_run_hook (db_path, "post-insert");
    }

    talloc_free (local);

    return status_to_exit (status);
}
