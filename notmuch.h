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

#include <time.h>

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
 * NOTMUCH_STATUS_OUT_OF_MEMORY: Out of memory
 *
 * XXX: We don't really want to expose this lame XAPIAN_EXCEPTION
 * value. Instead we should map to things like DATABASE_LOCKED or
 * whatever.
 *
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION: A Xapian exception occurred
 *
 * NOTMUCH_STATUS_FILE_ERROR: An error occurred trying to read or
 *	write to a file (this could be file not found, permission
 *	denied, etc.)
 *
 * NOTMUCH_STATUS_FILE_NOT_EMAIL: A file was presented that doesn't
 *	appear to be an email message.
 *
 * NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID: A file contains a message ID
 *	that is identical to a message already in the database.
 *
 * NOTMUCH_STATUS_NULL_POINTER: The user erroneously passed a NULL
 *	pointer to a notmuch function.
 *
 * NOTMUCH_STATUS_TAG_TOO_LONG: A tag value is too long (exceeds
 *	NOTMUCH_TAG_MAX)
 *
 * NOTMUCH_STATUS_LAST_STATUS: Not an actual status value. Just a way
 *	to find out how many valid status values there are.
 */
typedef enum _notmuch_status {
    NOTMUCH_STATUS_SUCCESS = 0,
    NOTMUCH_STATUS_OUT_OF_MEMORY,
    NOTMUCH_STATUS_XAPIAN_EXCEPTION,
    NOTMUCH_STATUS_FILE_ERROR,
    NOTMUCH_STATUS_FILE_NOT_EMAIL,
    NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID,
    NOTMUCH_STATUS_NULL_POINTER,
    NOTMUCH_STATUS_TAG_TOO_LONG,

    NOTMUCH_STATUS_LAST_STATUS
} notmuch_status_t;

/* Get a string representation of a notmuch_status_t value.
 *
 * The result is readonly.
 */
const char *
notmuch_status_to_string (notmuch_status_t status);

/* Various opaque data types. For each notmuch_<foo>_t see the various
 * notmuch_<foo> functions below. */
typedef struct _notmuch_database notmuch_database_t;
typedef struct _notmuch_query notmuch_query_t;
typedef struct _notmuch_thread_results notmuch_thread_results_t;
typedef struct _notmuch_thread notmuch_thread_t;
typedef struct _notmuch_message_results notmuch_message_results_t;
typedef struct _notmuch_message notmuch_message_t;
typedef struct _notmuch_tags notmuch_tags_t;

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

/* XXX: I think I'd like this to take an extra argument of
 * notmuch_status_t* for returning a status value on failure. */

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

/* Return the database path of the given database.
 *
 * The return value is a string owned by notmuch so should not be
 * modified nor freed by the caller. */
const char *
notmuch_database_get_path (notmuch_database_t *database);

/* Store a timestamp within the database.
 *
 * The Notmuch database will not interpret this key nor the timestamp
 * values at all. It will merely store them together and return the
 * timestamp when notmuch_database_get_timestamp is called with the
 * same value for 'key'.
 *
 * The intention is for the caller to use the timestamp to allow
 * efficient identification of new messages to be added to the
 * database. The recommended usage is as follows:
 *
 *   o Read the mtime of a directory from the filesystem
 *
 *   o Call add_message for all mail files in the directory
 *
 *   o Call notmuch_database_set_timestamp with the path of the
 *     directory as 'key' and the originally read mtime as 'value'.
 *
 * Then, when wanting to check for updates to the directory in the
 * future, the client can call notmuch_database_get_timestamp and know
 * that it only needs to add files if the mtime of the directory and
 * files are newer than the stored timestamp.
 *
 * Note: The notmuch_database_get_timestamp function does not allow
 * the caller to distinguish a timestamp of 0 from a non-existent
 * timestamp. So don't store a timestamp of 0 unless you are
 * comfortable with that.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Timestamp successfully stored in database.
 *
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION: A Xapian exception
 *	occurred. Timestamp not stored.
 */
notmuch_status_t
notmuch_database_set_timestamp (notmuch_database_t *database,
				const char *key, time_t timestamp);

/* Retrieve a timestamp from the database.
 *
 * Returns the timestamp value previously stored by calling
 * notmuch_database_set_timestamp with the same value for 'key'.
 *
 * Returns 0 if no timestamp is stored for 'key' or if any error
 * occurred querying the database.
 */
time_t
notmuch_database_get_timestamp (notmuch_database_t *database,
				const char *key);

/* Add a new message to the given notmuch database.
 *
 * Here,'filename' should be a path relative to the the path of
 * 'database' (see notmuch_database_get_path). The file should be a
 * single mail message (not a multi-message mbox) that is expected to
 * remain at its current location, (since the notmuch database will
 * reference the filename, and will not copy the entire contents of
 * the file.
 *
 * If 'message' is not NULL, then '*message' will be initialized to a
 * message object that can be used for things such as adding tags to
 * the just-added message. The user should call
 * notmuch_message_destroy when done with the message.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Message successfully added to database.
 *
 * NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID: Message has the same message
 *	ID as another message already in the database. Nothing added
 *	to the database.
 *
 * NOTMUCH_STATUS_FILE_ERROR: an error occurred trying to open the
 *	file, (such as permission denied, or file not found,
 *	etc.). Nothing added to the database.
 *
 * NOTMUCH_STATUS_FILE_NOT_EMAIL: the contents of filename don't look
 *	like an email message. Nothing added to the database.
 */
notmuch_status_t
notmuch_database_add_message (notmuch_database_t *database,
			      const char *filename,
			      notmuch_message_t **message);

/* Find a message with the given messsage_id.
 *
 * If the database contains a message with the given message_id, then
 * a new notmuch_message_t object is returned. The caller should call
 * notmuch_message_destroy when done with the message.
 *
 * If no message is found with the given message_id or if an
 * out-of-memory situation occurs, this function returns NULL.
 */
notmuch_message_t *
notmuch_database_find_message (notmuch_database_t *database,
			       const char *message_id);

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
 * As a special case, passing a length-zero string, (that is ""), will
 * result in a query that returns all messages in the database.
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

/* Sort values for notmuch_query_set_sort */
typedef enum {
    NOTMUCH_SORT_DATE_OLDEST_FIRST,
    NOTMUCH_SORT_DATE_NEWEST_FIRST,
    NOTMUCH_SORT_MESSAGE_ID
} notmuch_sort_t;

/* Specify the sorting desired for this query. */
void
notmuch_query_set_sort (notmuch_query_t *query, notmuch_sort_t sort);

/* Execute a query for threads, returning a notmuch_thread_results_t
 * object which can be used to iterate over the results. The results
 * object is owned by the query and as such, will only be valid until
 * notmuch_query_destroy.
 *
 * Typical usage might be:
 *
 *     notmuch_query_t *query;
 *     notmuch_thread_results_t *results;
 *     notmuch_thread_t *thread;
 *
 *     query = notmuch_query_create (database, query_string);
 *
 *     for (results = notmuch_query_search_threads (query);
 *          notmuch_thread_results_has_more (results);
 *          notmuch_thread_results_advance (results))
 *     {
 *         thread = notmuch_thread_results_get (results);
 *         ....
 *         notmuch_thread_destroy (thread);
 *     }
 *
 *     notmuch_query_destroy (query);
 *
 * Note: If you are finished with a thread before its containing
 * query, you can call notmuch_thread_destroy to clean up some memory
 * sooner (as in the above example). Otherwise, if your thread objects
 * are long-lived, then you don't need to call notmuch_thread_destroy
 * and all the memory will still be reclaimed when the query is
 * destroyed.
 *
 * Note that there's no explicit destructor needed for the
 * notmuch_thread_results_t object. (For consistency, we do provide a
 * notmuch_thread_results_destroy function, but there's no good reason
 * to call it if the query is about to be destroyed).
 */
notmuch_thread_results_t *
notmuch_query_search_threads (notmuch_query_t *query);

/* Execute a query for messages, returning a notmuch_message_results_t
 * object which can be used to iterate over the results. The results
 * object is owned by the query and as such, will only be valid until
 * notmuch_query_destroy.
 *
 * Typical usage might be:
 *
 *     notmuch_query_t *query;
 *     notmuch_message_results_t *results;
 *     notmuch_message_t *message;
 *
 *     query = notmuch_query_create (database, query_string);
 *
 *     for (results = notmuch_query_search_messages (query);
 *          notmuch_message_results_has_more (results);
 *          notmuch_message_results_advance (results))
 *     {
 *         message = notmuch_message_results_get (results);
 *         ....
 *         notmuch_message_destroy (message);
 *     }
 *
 *     notmuch_query_destroy (query);
 *
 * Note: If you are finished with a message before its containing
 * query, you can call notmuch_message_destroy to clean up some memory
 * sooner (as in the above example). Otherwise, if your message
 * objects are long-lived, then you don't need to call
 * notmuch_message_destroy and all the memory will still be reclaimed
 * when the query is destroyed.
 *
 * Note that there's no explicit destructor needed for the
 * notmuch_message_results_t object. (For consistency, we do provide a
 * notmuch_message_results_destroy function, but there's no good
 * reason to call it if the query is about to be destroyed).
 */
notmuch_message_results_t *
notmuch_query_search_messages (notmuch_query_t *query);

/* Destroy a notmuch_query_t along with any associated resources.
 *
 * This will in turn destroy any notmuch_thread_results_t and
 * notmuch_message_results_t objects generated by this query, (and in
 * turn any notmuch_thrad_t and notmuch_message_t objects generated
 * from those results, etc.), if such objects haven't already been
 * destroyed.
 */
void
notmuch_query_destroy (notmuch_query_t *query);

/* Does the given notmuch_thread_results_t object contain any more
 * results.
 *
 * When this function returns TRUE, notmuch_thread_results_get will
 * return a valid object. Whereas when this function returns FALSE,
 * notmuch_thread_results_get will return NULL.
 *
 * See the documentation of notmuch_query_search_threads for example
 * code showing how to iterate over a notmuch_thread_results_t object.
 */
notmuch_bool_t
notmuch_thread_results_has_more (notmuch_thread_results_t *results);

/* Get the current result from 'results' as a notmuch_thread_t.
 *
 * Note: The returned thread belongs to 'results' and has a lifetime
 * identical to it (and the query to which it belongs).
 *
 * See the documentation of notmuch_query_search_threads for example
 * code showing how to iterate over a notmuch_thread_results_t object.
 *
 * If an out-of-memory situation occurs, this function will return
 * NULL.
 */
notmuch_thread_t *
notmuch_thread_results_get (notmuch_thread_results_t *results);

/* Advance the 'results' iterator to the next result.
 *
 * See the documentation of notmuch_query_search_threads for example
 * code showing how to iterate over a notmuch_thread_results_t object.
 */
void
notmuch_thread_results_advance (notmuch_thread_results_t *results);

/* Destroy a notmuch_thread_results_t object.
 *
 * It's not strictly necessary to call this function. All memory from
 * the notmuch_thread_results_t object will be reclaimed when the
 * containg query object is destroyed.
 */
void
notmuch_thread_results_destroy (notmuch_thread_results_t *results);

/* Get the thread ID of 'thread'.
 *
 * The returned string belongs to 'thread' and as such, should not be
 * modified by the caller and will only be valid for as long as the
 * thread is valid, (which is until notmuch_thread_destroy or until
 * the query from which it derived is destroyed).
 */
const char *
notmuch_thread_get_thread_id (notmuch_thread_t *thread);

/* Get the subject of 'thread'
 *
 * The subject is taken from the first message (according to the query
 * order---see notmuch_query_set_sort) in the query results that
 * belongs to this thread.
 *
 * The returned string belongs to 'thread' and as such, should not be
 * modified by the caller and will only be valid for as long as the
 * thread is valid, (which is until notmuch_thread_destroy or until
 * the query from which it derived is destroyed).
 */
const char *
notmuch_thread_get_subject (notmuch_thread_t *thread);

/* Get the tags for 'thread', returning a notmuch_tags_t object which
 * can be used to iterate over all tags.
 *
 * Note: In the Notmuch database, tags are stored on individual
 * messages, not on threads. So the tags returned here will be all
 * tags of the messages which matched the search and which belong to
 * this thread.
 *
 * The tags object is owned by the thread and as such, will only be
 * valid for as long as the thread is valid, (for example, until
 * notmuch_thread_destroy or until the query from which it derived is
 * destroyed).
 *
 * Typical usage might be:
 *
 *     notmuch_thread_t *thread;
 *     notmuch_tags_t *tags;
 *     const char *tag;
 *
 *     thread = notmuch_thread_results_get (thread_results);
 *
 *     for (tags = notmuch_thread_get_tags (thread);
 *          notmuch_tags_has_more (tags);
 *          notmuch_result_advance (tags))
 *     {
 *         tag = notmuch_tags_get (tags);
 *         ....
 *     }
 *
 *     notmuch_thread_destroy (thread);
 *
 * Note that there's no explicit destructor needed for the
 * notmuch_tags_t object. (For consistency, we do provide a
 * notmuch_tags_destroy function, but there's no good reason to call
 * it if the message is about to be destroyed).
 */
notmuch_tags_t *
notmuch_thread_get_tags (notmuch_thread_t *thread);

/* Destroy a notmuch_thread_t object. */
void
notmuch_thread_destroy (notmuch_thread_t *thread);

/* Does the given notmuch_message_results_t object contain any more
 * results.
 *
 * When this function returns TRUE, notmuch_message_results_get will
 * return a valid object. Whereas when this function returns FALSE,
 * notmuch_message_results_get will return NULL.
 *
 * See the documentation of notmuch_query_search_messages for example
 * code showing how to iterate over a notmuch_message_results_t
 * object.
 */
notmuch_bool_t
notmuch_message_results_has_more (notmuch_message_results_t *results);

/* Get the current result from 'results' as a notmuch_message_t.
 *
 * Note: The returned message belongs to 'results' and has a lifetime
 * identical to it (and the query to which it belongs).
 *
 * See the documentation of notmuch_query_search_messages for example
 * code showing how to iterate over a notmuch_message_results_t
 * object.
 *
 * If an out-of-memory situation occurs, this function will return
 * NULL.
 */
notmuch_message_t *
notmuch_message_results_get (notmuch_message_results_t *results);

/* Advance the 'results' iterator to the next result.
 *
 * See the documentation of notmuch_query_search_messages for example
 * code showing how to iterate over a notmuch_message_results_t
 * object.
 */
void
notmuch_message_results_advance (notmuch_message_results_t *results);

/* Destroy a notmuch_message_results_t object.
 *
 * It's not strictly necessary to call this function. All memory from
 * the notmuch_message_results_t object will be reclaimed when the
 * containg query object is destroyed.
 */
void
notmuch_message_results_destroy (notmuch_message_results_t *results);

/* Get the message ID of 'message'.
 *
 * The returned string belongs to 'message' and as such, should not be
 * modified by the caller and will only be valid for as long as the
 * message is valid, (which is until the query from which it derived
 * is destroyed).
 *
 * This function will not return NULL since Notmuch ensures that every
 * message has a unique message ID, (Notmuch will generate an ID for a
 * message if the original file does not contain one).
 */
const char *
notmuch_message_get_message_id (notmuch_message_t *message);

/* Get the thread ID of 'message'.
 *
 * The returned string belongs to 'message' and as such, should not be
 * modified by the caller and will only be valid for as long as the
 * message is valid, (for example, until the user calls
 * notmuch_message_destroy on 'message' or until a query from which it
 * derived is destroyed).
 *
 * This function will not return NULL since Notmuch ensures that every
 * message belongs to a single thread.
 */
const char *
notmuch_message_get_thread_id (notmuch_message_t *message);

/* Get the filename for the email corresponding to 'message'.
 *
 * The returned filename is relative to the base of the database from
 * which 'message' was obtained. See notmuch_database_get_path() .
 * The returned string belongs to the message so should not be
 * modified or freed by the caller (nor should it be referenced after
 * the message is destroyed). */
const char *
notmuch_message_get_filename (notmuch_message_t *message);

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
 *     message = notmuch_database_find_message (database, message_id);
 *
 *     for (tags = notmuch_message_get_tags (message);
 *          notmuch_tags_has_more (tags);
 *          notmuch_result_advance (tags))
 *     {
 *         tag = notmuch_tags_get (tags);
 *         ....
 *     }
 *
 *     notmuch_message_destroy (message);
 *
 * Note that there's no explicit destructor needed for the
 * notmuch_tags_t object. (For consistency, we do provide a
 * notmuch_tags_destroy function, but there's no good reason to call
 * it if the message is about to be destroyed).
 */
notmuch_tags_t *
notmuch_message_get_tags (notmuch_message_t *message);

/* The longest possible tag value. */
#define NOTMUCH_TAG_MAX 200

/* Add a tag to the given message.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Tag successfully added to message
 *
 * NOTMUCH_STATUS_NULL_POINTER: The 'tag' argument is NULL
 *
 * NOTMUCH_STATUS_TAG_TOO_LONG: The length of 'tag' is longer than
 *	too long (exceeds NOTMUCH_TAG_MAX)
 */
notmuch_status_t
notmuch_message_add_tag (notmuch_message_t *message, const char *tag);

/* Remove a tag from the given message.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Tag successfully added to message
 *
 * NOTMUCH_STATUS_NULL_POINTER: The 'tag' argument is NULL
 *
 * NOTMUCH_STATUS_TAG_TOO_LONG: The length of 'tag' is longer than
 *	too long (exceeds NOTMUCH_TAG_MAX)
 */
notmuch_status_t
notmuch_message_remove_tag (notmuch_message_t *message, const char *tag);

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

/* Does the given notmuch_tags_t object contain any more tags.
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

/* Get the current tag from 'tags' as a string.
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
notmuch_tags_advance (notmuch_tags_t *tags);

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
