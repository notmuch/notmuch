/*
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#define _GNU_SOURCE

#include "path-util.h"
#include "compat.h"
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <limits.h>
#include <stdlib.h>

#include <talloc.h>

char *
notmuch_canonicalize_file_name (const char *path)
{
#if HAVE_CANONICALIZE_FILE_NAME
    return canonicalize_file_name (path);
#elif defined(PATH_MAX)
    char *resolved_path =  malloc (PATH_MAX + 1);
    if (resolved_path == NULL)
	return NULL;

    return realpath (path, resolved_path);
#else
#error undefined PATH_MAX _and_ missing canonicalize_file_name not supported
#endif
}

/* Call fsync() on a directory path. */
notmuch_status_t
sync_dir (const char *dir, char **status_string)
{
    int fd, r;

    fd = open (dir, O_RDONLY);
    if (fd == -1) {
	if (status_string)
	    IGNORE_RESULT (asprintf (status_string,
				     "Error: open %s: %s\n", dir, strerror (errno)));
	return NOTMUCH_STATUS_FILE_ERROR;
    }

    r = fsync (fd);
    if (r && status_string)
	IGNORE_RESULT (asprintf (status_string,
				 "Error: fsync %s: %s\n", dir, strerror (errno)));

    close (fd);

    return r == 0 ? NOTMUCH_STATUS_SUCCESS : NOTMUCH_STATUS_FILE_ERROR;
}

/*
 * Make the given directory and its parents as necessary, using the
 * given mode. Partial results are not cleaned up on errors.
 */
notmuch_status_t
mkdir_recursive (const void *ctx, const char *path, int mode,
		 char **status_string)
{
    notmuch_status_t status;
    struct stat st;
    int r;
    char *parent = NULL, *slash;

    /* First check the common case: directory already exists. */
    r = stat (path, &st);
    if (r == 0) {
	if (! S_ISDIR (st.st_mode)) {
	    if (status_string)
		IGNORE_RESULT (asprintf (status_string, "Error: '%s' is not a directory: %s\n",
					 path, strerror (EEXIST)));
	    return NOTMUCH_STATUS_FILE_ERROR;
	}

	return NOTMUCH_STATUS_SUCCESS;
    } else if (errno != ENOENT) {
	if (status_string)
	    IGNORE_RESULT (asprintf (status_string,
				     "Error: stat '%s': %s\n", path, strerror (errno)));
	return NOTMUCH_STATUS_FILE_ERROR;
    }

    /* mkdir parents, if any */
    slash = strrchr (path, '/');
    if (slash && slash != path) {
	parent = talloc_strndup (ctx, path, slash - path);
	if (! parent) {
	    if (status_string)
		IGNORE_RESULT (asprintf (status_string,
					 "Error: %s\n",
					 strerror (ENOMEM)));
	    return NOTMUCH_STATUS_FILE_ERROR;
	}

	status = mkdir_recursive (ctx, parent, mode, status_string);
	if (status)
	    return status;
    }

    if (mkdir (path, mode)) {
	if (status_string)
	    IGNORE_RESULT (asprintf (status_string,
				     "Error: mkdir '%s': %s\n",
				     path, strerror (errno)));
	return NOTMUCH_STATUS_FILE_ERROR;
    }

    if (parent) {
	status = sync_dir (parent, status_string);
	if (status)
	    return status;
    }
    return NOTMUCH_STATUS_SUCCESS;
}
