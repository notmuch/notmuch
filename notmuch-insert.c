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
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: Peter Wang <novalazy@gmail.com>
 */

#include "notmuch-client.h"
#include "tag-util.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

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
static notmuch_bool_t
sync_dir (const char *dir)
{
    notmuch_bool_t ret;
    int fd;

    fd = open (dir, O_RDONLY);
    if (fd == -1) {
	fprintf (stderr, "Error: open() dir failed: %s\n", strerror (errno));
	return FALSE;
    }
    ret = (fsync (fd) == 0);
    if (! ret) {
	fprintf (stderr, "Error: fsync() dir failed: %s\n", strerror (errno));
    }
    close (fd);
    return ret;
}

/* Check the specified folder name does not contain a directory
 * component ".." to prevent writes outside of the Maildir hierarchy. */
static notmuch_bool_t
check_folder_name (const char *folder)
{
    const char *p = folder;

    for (;;) {
	if ((p[0] == '.') && (p[1] == '.') && (p[2] == '\0' || p[2] == '/'))
	    return FALSE;
	p = strchr (p, '/');
	if (!p)
	    return TRUE;
	p++;
    }
}

/* Open a unique file in the 'tmp' sub-directory of dir.
 * Returns the file descriptor on success, or -1 on failure.
 * On success, file paths for the message in the 'tmp' and 'new'
 * directories are returned via tmppath and newpath,
 * and the path of the 'new' directory itself in newdir. */
static int
maildir_open_tmp_file (void *ctx, const char *dir,
		       char **tmppath, char **newpath, char **newdir)
{
    pid_t pid;
    char hostname[256];
    struct timeval tv;
    char *filename;
    int fd = -1;

    /* We follow the Dovecot file name generation algorithm. */
    pid = getpid ();
    safe_gethostname (hostname, sizeof (hostname));
    do {
	gettimeofday (&tv, NULL);
	filename = talloc_asprintf (ctx, "%ld.M%ldP%d.%s",
				    tv.tv_sec, tv.tv_usec, pid, hostname);
	if (! filename) {
	    fprintf (stderr, "Out of memory\n");
	    return -1;
	}

	*tmppath = talloc_asprintf (ctx, "%s/tmp/%s", dir, filename);
	if (! *tmppath) {
	    fprintf (stderr, "Out of memory\n");
	    return -1;
	}

	fd = open (*tmppath, O_WRONLY | O_CREAT | O_TRUNC | O_EXCL, 0600);
    } while (fd == -1 && errno == EEXIST);

    if (fd == -1) {
	fprintf (stderr, "Error: opening %s: %s\n", *tmppath, strerror (errno));
	return -1;
    }

    *newdir = talloc_asprintf (ctx, "%s/new", dir);
    *newpath = talloc_asprintf (ctx, "%s/new/%s", dir, filename);
    if (! *newdir || ! *newpath) {
	fprintf (stderr, "Out of memory\n");
	close (fd);
	unlink (*tmppath);
	return -1;
    }

    talloc_free (filename);

    return fd;
}

/* Copy the contents of standard input (fdin) into fdout.
 * Returns TRUE if a non-empty file was written successfully.
 * Otherwise, return FALSE. */
static notmuch_bool_t
copy_stdin (int fdin, int fdout)
{
    notmuch_bool_t empty = TRUE;

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
	    return FALSE;
	}

	p = buf;
	do {
	    ssize_t written = write (fdout, p, remain);
	    if (written < 0 && errno == EINTR)
		continue;
	    if (written <= 0) {
		fprintf (stderr, "Error: writing to temporary file: %s",
			 strerror (errno));
		return FALSE;
	    }
	    p += written;
	    remain -= written;
	    empty = FALSE;
	} while (remain > 0);
    }

    return (!interrupted && !empty);
}

/* Add the specified message file to the notmuch database, applying tags.
 * The file is renamed to encode notmuch tags as maildir flags. */
static void
add_file_to_database (notmuch_database_t *notmuch, const char *path,
		      tag_op_list_t *tag_ops)
{
    notmuch_message_t *message;
    notmuch_status_t status;

    status = notmuch_database_add_message (notmuch, path, &message);
    switch (status) {
    case NOTMUCH_STATUS_SUCCESS:
    case NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID:
	break;
    default:
    case NOTMUCH_STATUS_FILE_NOT_EMAIL:
    case NOTMUCH_STATUS_READ_ONLY_DATABASE:
    case NOTMUCH_STATUS_XAPIAN_EXCEPTION:
    case NOTMUCH_STATUS_OUT_OF_MEMORY:
    case NOTMUCH_STATUS_FILE_ERROR:
    case NOTMUCH_STATUS_NULL_POINTER:
    case NOTMUCH_STATUS_TAG_TOO_LONG:
    case NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW:
    case NOTMUCH_STATUS_UNBALANCED_ATOMIC:
    case NOTMUCH_STATUS_LAST_STATUS:
	fprintf (stderr, "Error: failed to add `%s' to notmuch database: %s\n",
		 path, notmuch_status_to_string (status));
	return;
    }

    if (status == NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID) {
	/* Don't change tags of an existing message. */
	status = notmuch_message_tags_to_maildir_flags (message);
	if (status != NOTMUCH_STATUS_SUCCESS)
	    fprintf (stderr, "Error: failed to sync tags to maildir flags\n");
    } else {
	tag_op_list_apply (message, tag_ops, TAG_FLAG_MAILDIR_SYNC);
    }

    notmuch_message_destroy (message);
}

static notmuch_bool_t
insert_message (void *ctx, notmuch_database_t *notmuch, int fdin,
		const char *dir, tag_op_list_t *tag_ops)
{
    char *tmppath;
    char *newpath;
    char *newdir;
    int fdout;
    char *cleanup_path;

    fdout = maildir_open_tmp_file (ctx, dir, &tmppath, &newpath, &newdir);
    if (fdout < 0)
	return FALSE;

    cleanup_path = tmppath;

    if (! copy_stdin (fdin, fdout))
	goto FAIL;

    if (fsync (fdout) != 0) {
	fprintf (stderr, "Error: fsync failed: %s\n", strerror (errno));
	goto FAIL;
    }

    close (fdout);
    fdout = -1;

    /* Atomically move the new message file from the Maildir 'tmp' directory
     * to the 'new' directory.  We follow the Dovecot recommendation to
     * simply use rename() instead of link() and unlink().
     * See also: http://wiki.dovecot.org/MailboxFormat/Maildir#Mail_delivery
     */
    if (rename (tmppath, newpath) != 0) {
	fprintf (stderr, "Error: rename() failed: %s\n", strerror (errno));
	goto FAIL;
    }

    cleanup_path = newpath;

    if (! sync_dir (newdir))
	goto FAIL;

    /* Even if adding the message to the notmuch database fails,
     * the message is on disk and we consider the delivery completed. */
    add_file_to_database (notmuch, newpath, tag_ops);

    return TRUE;

  FAIL:
    if (fdout >= 0)
	close (fdout);
    unlink (cleanup_path);
    return FALSE;
}

int
notmuch_insert_command (notmuch_config_t *config, int argc, char *argv[])
{
    notmuch_database_t *notmuch;
    struct sigaction action;
    const char *db_path;
    const char **new_tags;
    size_t new_tags_length;
    tag_op_list_t *tag_ops;
    char *query_string = NULL;
    const char *folder = NULL;
    const char *maildir;
    int opt_index;
    unsigned int i;
    notmuch_bool_t ret;

    notmuch_opt_desc_t options[] = {
	{ NOTMUCH_OPT_STRING, &folder, "folder", 0, 0 },
	{ NOTMUCH_OPT_END, 0, 0, 0, 0 }
    };

    opt_index = parse_arguments (argc, argv, options, 1);

    if (opt_index < 0) {
	/* diagnostics already printed */
	return 1;
    }

    db_path = notmuch_config_get_database_path (config);
    new_tags = notmuch_config_get_new_tags (config, &new_tags_length);

    tag_ops = tag_op_list_create (config);
    if (tag_ops == NULL) {
	fprintf (stderr, "Out of memory.\n");
	return 1;
    }
    for (i = 0; i < new_tags_length; i++) {
	if (tag_op_list_append (tag_ops, new_tags[i], FALSE))
	    return 1;
    }

    if (parse_tag_command_line (config, argc - opt_index, argv + opt_index,
				&query_string, tag_ops))
	return 1;

    if (*query_string != '\0') {
	fprintf (stderr, "Error: unexpected query string: %s\n", query_string);
	return 1;
    }

    if (folder == NULL) {
	maildir = db_path;
    } else {
	if (! check_folder_name (folder)) {
	    fprintf (stderr, "Error: bad folder name: %s\n", folder);
	    return 1;
	}
	maildir = talloc_asprintf (config, "%s/%s", db_path, folder);
	if (! maildir) {
	    fprintf (stderr, "Out of memory\n");
	    return 1;
	}
    }

    /* Setup our handler for SIGINT. We do not set SA_RESTART so that copying
     * from standard input may be interrupted. */
    memset (&action, 0, sizeof (struct sigaction));
    action.sa_handler = handle_sigint;
    sigemptyset (&action.sa_mask);
    action.sa_flags = 0;
    sigaction (SIGINT, &action, NULL);

    if (notmuch_database_open (notmuch_config_get_database_path (config),
			       NOTMUCH_DATABASE_MODE_READ_WRITE, &notmuch))
	return 1;

    ret = insert_message (config, notmuch, STDIN_FILENO, maildir, tag_ops);

    notmuch_database_destroy (notmuch);

    return (ret) ? 0 : 1;
}
