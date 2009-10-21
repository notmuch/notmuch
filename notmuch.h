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

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

typedef int notmuch_bool_t;

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

/* Various opaque data types. For each notmuch_<foo>_t see the various
 * notmuch_<foo> functions below. */
typedef struct _notmuch_database notmuch_database_t;
typedef struct _notmuch_query notmuch_query_t;
typedef struct _notmuch_results notmuch_results_t;
typedef struct _notmuch_message notmuch_message_t;
typedef struct _notmuch_tags notmuch_tags_t;

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

/* Create a new query for 'database'.
 *
 * Here, 'database' should be an open database, (see
 * notmuch_database_open and notmuch_database_create).
 *
 * For the query string, we'll document the syntax here more
 * completely in the future, but it's likely to be a specialized
 * version of the general Xapian query syntax:
 *
 * http://xapian.org/docs/queryparser.html
 *
 * As a special case, passing a value of NOTMUCH_QUERY_ALL for the
 * query string will result in a query that returns all messages in
 * the database.
 *
 * See notmuch_query_set_sort for controlling the order of results and
 * notmuch_query_search to actually execute the query.
 *
 * User should call notmuch_query_destroy when finished with this
 * query.
 *
 * Will return NULL if insufficient memory is available.
 */
notmuch_query_t *
notmuch_query_create (notmuch_database_t *database,
		      const char *query_string);

/* Special value to cause notmuch_query_create to return all
 * messages. */
extern const char *NOTMUCH_QUERY_ALL;

/* Sort values for notmuch_query_set_sort */
typedef enum {
    NOTMUCH_SORT_DATE_OLDEST_FIRST,
    NOTMUCH_SORT_DATE_NEWEST_FIRST,
    NOTMUCH_SORT_MESSAGE_ID
} notmuch_sort_t;

/* Specify the sorting desired for this query. */
void
notmuch_query_set_sort (notmuch_query_t *query, notmuch_sort_t sort);

/* Execute a query, returning a notmuch_results_t object which can be
 * used to iterate over the results. The results object is owned by
 * the query and as such, will only be valid until notmuch_query_destroy.
 *
 * Typical usage might be:
 *
 *     notmuch_query_t *query;
 *     notmuch_results_t *results;
 *
 *     query = notmuch_query_create (database, query_string);
 *
 *     for (results = notmuch_query_search (query);
 *          notmuch_results_has_more (results);
 *          notmuch_result_advance (results))
 *     {
 *         message = notmuch_results_get (results);
 *         ....
 *     }
 *
 *     notmuch_query_destroy (query);
 *
 * Note that there's no explicit destructor needed for the
 * notmuch_results_t object.
 *
 * (For consistency, we do provide a notmuch_results_destroy function,
 * but there's no point in calling it if you're about to destroy the
 * query object as well too---either call will free all the memory of
 * the results).
 */
notmuch_results_t *
notmuch_query_search (notmuch_query_t *query);

/* Destroy a notmuch_query_t along with any associated resources.
 *
 * This will in turn destroy any notmuch_results_t objects generated
 * by this query, (and in turn any notmuch_message_t objects generated
 * from those results, etc.).
 */
void
notmuch_query_destroy (notmuch_query_t *query);

/* Does the given notmuch_results_t object contain any more results.
 *
 * When this function returns TRUE, notmuch_results_get will return a
 * valid object. Whereas when this function returns FALSE,
 * notmuch_results_get will return NULL.
 *
 * See the documentation of notmuch_query_search for example code
 * showing how to iterate over a notmuch_results_t object.
 */
notmuch_bool_t
notmuch_results_has_more (notmuch_results_t *results);

/* Get the current result from 'results' as a notmuch_message_t.
 *
 * Note: The returned message belongs to 'results' and has a lifetime
 * identical to it (and the query to which it belongs).
 *
 * See the documentation of notmuch_query_search for example code
 * showing how to iterate over a notmuch_results_t object.
 */
notmuch_message_t *
notmuch_results_get (notmuch_results_t *results);

/* Advance the 'results' iterator to the next result.
 *
 * See the documentation of notmuch_query_search for example code
 * showing how to iterate over a notmuch_results_t object.
 */
void
notmuch_results_advance (notmuch_results_t *results);

/* Destroy a notmuch_results_t object.
 *
 * It's not strictly necessary to call this function. All memory from
 * the notmuch_results_t object will be reclaimed when the containg
 * query object is destroyed.
 */
void
notmuch_results_destroy (notmuch_results_t *results);

/* Get the message ID of 'message'.
 *
 * The returned string belongs to 'message' and as such, should not be
 * modified by the caller and will only be valid for as long as the
 * message is valid, (which is until the query from which it derived
 * is destroyed).
 */
const char *
notmuch_message_get_message_id (notmuch_message_t *message);

/* Get the tags for 'message', returning a notmuch_tags_t object which
 * can be used to iterate over all tags.
 *
 * The tags object is owned by the message and as such, will only be
 * valid for as long as the message is valid, (which is until the
 * query from which it derived is destroyed).
 *
 * Typical usage might be:
 *
 *     notmuch_message_t *message;
 *     notmuch_tags_t *tags;
 *     const char *tag;
 *
 *     message = notmuch_results_get (results);
 *
 *     for (tags = notmuch_message_get_tags (message);
 *          notmuch_tags_has_more (tags);
 *          notmuch_result_advance (tags))
 *     {
 *         tag = notmuch_tags_get_string (tags);
 *         ....
 *     }
 *
 * Note: If you are finished with a message before its containing
 * query, you can call notmuch_message_destroy to clean up some memory
 * sooner. If you don't call it, all the memory will still be
 * reclaimed when the query is destroyed.
 */
notmuch_tags_t *
notmuch_message_get_tags (notmuch_message_t *message);

/* Destroy a notmuch_message_t object.
 *
 * It can be useful to call this function in the case of a single
 * query object with many messages in the result, (such as iterating
 * over the entire database). Otherwise, it's fine to never call this
 * function and there will still be no memory leaks. (The memory from
 * the messages get reclaimed when the containing query is destroyed.)
 */
void
notmuch_message_destroy (notmuch_message_t *message);

/* Does the given notmuch_tags_t object contain any more results.
 *
 * When this function returns TRUE, notmuch_tags_get will return a
 * valid string. Whereas when this function returns FALSE,
 * notmuch_tags_get will return NULL.
 *
 * See the documentation of notmuch_message_get_tags for example code
 * showing how to iterate over a notmuch_tags_t object.
 */
notmuch_bool_t
notmuch_tags_has_more (notmuch_tags_t *tags);

/* Get the current result from 'tags' as a string.
 *
 * Note: The returned string belongs to 'tags' and has a lifetime
 * identical to it (and the query to which it utlimately belongs).
 *
 * See the documentation of notmuch_message_get_tags for example code
 * showing how to iterate over a notmuch_tags_t object.
 */
const char *
notmuch_tags_get (notmuch_tags_t *tags);

/* Advance the 'tags' iterator to the next tag.
 *
 * See the documentation of notmuch_message_get_tags for example code
 * showing how to iterate over a notmuch_tags_t object.
 */
void
notmuch_tags_advance (notmuch_tags_t *results);

/* Destroy a notmuch_tags_t object.
 *
 * It's not strictly necessary to call this function. All memory from
 * the notmuch_tags_t object will be reclaimed when the containg
 * message or query objects are destroyed.
 */
void
notmuch_tags_destroy (notmuch_tags_t *tags);

NOTMUCH_END_DECLS

#endif
