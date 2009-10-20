/* notmuch - Not much of an email library, (just index and search)
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

#ifndef NOTMUCH_H
#define NOTMUCH_H

#ifdef  __cplusplus
# define NOTMUCH_BEGIN_DECLS  extern "C" {
# define NOTMUCH_END_DECLS    }
#else
# define NOTMUCH_BEGIN_DECLS
# define NOTMUCH_END_DECLS
#endif

NOTMUCH_BEGIN_DECLS

/* Status codes used for the return values of most functions.
 *
 * A zero value (NOTMUCH_STATUS_SUCCESS) indicates that the function
 * completed without error. Any other value indicates an error as
 * follows:
 *
 * NOTMUCH_STATUS_SUCCESS: No error occurred.
 *
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION: A Xapian exception occurred
 *
 * NOTMUCH_STATUS_FILE_NOT_EMAIL: A file was presented that doesn't
 * 	appear to be an email message.
 */
typedef enum _notmuch_status {
    NOTMUCH_STATUS_SUCCESS = 0,
    NOTMUCH_STATUS_XAPIAN_EXCEPTION,
    NOTMUCH_STATUS_FILE_NOT_EMAIL
} notmuch_status_t;

/* An opaque data structure representing a notmuch database. See
 * notmuch_database_open and other notmuch_database functions
 * below. */
typedef struct _notmuch_database notmuch_database_t;

/* Create a new, empty notmuch database located at 'path'.
 *
 * The path should be a top-level directory to a collection of
 * plain-text email messages (one message per file). This call will
 * create a new ".notmuch" directory within 'path' where notmuch will
 * store its data.
 *
 * Passing a value of NULL for 'path' will cause notmuch to open the
 * default database. The default database path can be specified by the
 * NOTMUCH_BASE environment variable, and is equivalent to
 * ${HOME}/mail if NOTMUCH_BASE is not set.
 *
 * After a successful call to notmuch_database_create, the returned
 * database will be open so the caller should call
 * notmuch_database_close when finished with it.
 *
 * The database will not yet have any data in it
 * (notmuch_database_create itself is a very cheap function). Messages
 * contained within 'path' can be added to the database by calling
 * notmuch_database_add_message.
 *
 * In case of any failure, this function returns NULL, (after printing
 * an error message on stderr).
 */
notmuch_database_t *
notmuch_database_create (const char *path);

/* Open a an existing notmuch database located at 'path'.
 *
 * The database should have been created at some time in the past,
 * (not necessarily by this process), by calling
 * notmuch_database_create with 'path'.
 *
 * An existing notmuch database can be identified by the presence of a
 * directory named ".notmuch" below 'path'.
 *
 * Passing a value of NULL for 'path' will cause notmuch to open the
 * default database. The default database path can be specified by the
 * NOTMUCH_BASE environment variable, and is equivalent to
 * ${HOME}/mail if NOTMUCH_BASE is not set.
 *
 * The caller should call notmuch_database_close when finished with
 * this database.
 *
 * In case of any failure, this function returns NULL, (after printing
 * an error message on stderr).
 */
notmuch_database_t *
notmuch_database_open (const char *path);

/* Close the given notmuch database, freeing all associated
 * resources. See notmuch_database_open. */
void
notmuch_database_close (notmuch_database_t *database);

/* Lookup the default database path.
 *
 * This is the path that will be used by notmuch_database_create and
 * notmuch_database_open if given a NULL path. Specifically it will be
 * the value of the NOTMUCH_BASE environment variable if set,
 * otherwise ${HOME}/mail
 *
 * Returns a newly allocated string which the caller should free()
 * when finished with it.
 */
char *
notmuch_database_default_path (void);

/* Return the database path of the given database.
 *
 * The return value is a string owned by notmuch so should not be
 * modified nor freed by the caller. */
const char *
notmuch_database_get_path (notmuch_database_t *database);

/* Add a new message to the given notmuch database.
 *
 * Here,'filename' should be a path relative to the the path of
 * 'database' (see notmuch_database_get_path). The file should be a
 * single mail message (not a multi-message mbox) that is expected to
 * remain at its current location, (since the notmuch database will
 * reference the filename, and will not copy the entire contents of
 * the file.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Message successfully added to database.
 *
 * NOTMUCH_STATUS_FILE_NOT_EMAIL: the contents of filename don't look
 * 	like an email message. Nothing added to the database.
 */
notmuch_status_t
notmuch_database_add_message (notmuch_database_t *database,
			      const char *filename);

NOTMUCH_END_DECLS

#endif
