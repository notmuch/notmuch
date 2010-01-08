/* notmuch-private.h - Internal interfaces for notmuch.
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

#ifndef NOTMUCH_PRIVATE_H
#define NOTMUCH_PRIVATE_H

#ifndef _GNU_SOURCE
#define _GNU_SOURCE /* For getline and asprintf */
#endif
#include <stdio.h>

#include "compat.h"

#include "notmuch.h"

NOTMUCH_BEGIN_DECLS

#include <stdlib.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <ctype.h>
#include <assert.h>

#include <talloc.h>

#include "xutil.h"

#ifdef DEBUG
# define DEBUG_DATABASE_SANITY 1
# define DEBUG_QUERY 1
#endif

#define COMPILE_TIME_ASSERT(pred) ((void)sizeof(char[1 - 2*!(pred)]))

/* There's no point in continuing when we've detected that we've done
 * something wrong internally (as opposed to the user passing in a
 * bogus value).
 *
 * Note that PRINTF_ATTRIBUTE comes from talloc.h
 */
int
_internal_error (const char *format, ...) PRINTF_ATTRIBUTE (1, 2);

/* There's no point in continuing when we've detected that we've done
 * something wrong internally (as opposed to the user passing in a
 * bogus value).
 *
 * Note that __location__ comes from talloc.h.
 */
#define INTERNAL_ERROR(format, ...)			\
    _internal_error (format " (%s).\n",			\
		     ##__VA_ARGS__, __location__)

#define unused(x) x __attribute__ ((unused))

/* Thanks to Andrew Tridgell's (SAMBA's) talloc for this definition of
 * unlikely. The talloc source code comes to us via the GNU LGPL v. 3.
 */
/* these macros gain us a few percent of speed on gcc */
#if (__GNUC__ >= 3)
/* the strange !! is to ensure that __builtin_expect() takes either 0 or 1
   as its first argument */
#ifndef likely
#define likely(x)   __builtin_expect(!!(x), 1)
#endif
#ifndef unlikely
#define unlikely(x) __builtin_expect(!!(x), 0)
#endif
#else
#ifndef likely
#define likely(x) (x)
#endif
#ifndef unlikely
#define unlikely(x) (x)
#endif
#endif

typedef enum {
    NOTMUCH_VALUE_TIMESTAMP = 0,
    NOTMUCH_VALUE_MESSAGE_ID
} notmuch_value_t;

/* Xapian (with flint backend) complains if we provide a term longer
 * than this, but I haven't yet found a way to query the limit
 * programmatically. */
#define NOTMUCH_TERM_MAX 245

typedef enum _notmuch_private_status {
    /* First, copy all the public status values. */
    NOTMUCH_PRIVATE_STATUS_SUCCESS = NOTMUCH_STATUS_SUCCESS,
    NOTMUCH_PRIVATE_STATUS_OUT_OF_MEMORY = NOTMUCH_STATUS_OUT_OF_MEMORY,
    NOTMUCH_PRIVATE_STATUS_READ_ONLY_DATABASE = NOTMUCH_STATUS_READ_ONLY_DATABASE,
    NOTMUCH_PRIVATE_STATUS_XAPIAN_EXCEPTION = NOTMUCH_STATUS_XAPIAN_EXCEPTION,
    NOTMUCH_PRIVATE_STATUS_FILE_NOT_EMAIL = NOTMUCH_STATUS_FILE_NOT_EMAIL,
    NOTMUCH_PRIVATE_STATUS_NULL_POINTER = NOTMUCH_STATUS_NULL_POINTER,
    NOTMUCH_PRIVATE_STATUS_TAG_TOO_LONG = NOTMUCH_STATUS_TAG_TOO_LONG,
    NOTMUCH_PRIVATE_STATUS_UNBALANCED_FREEZE_THAW = NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW,

    /* Then add our own private values. */
    NOTMUCH_PRIVATE_STATUS_TERM_TOO_LONG = NOTMUCH_STATUS_LAST_STATUS,
    NOTMUCH_PRIVATE_STATUS_NO_DOCUMENT_FOUND,

    NOTMUCH_PRIVATE_STATUS_LAST_STATUS
} notmuch_private_status_t;

/* Coerce a notmuch_private_status_t value to a notmuch_status_t
 * value, generating an internal error if the private value is equal
 * to or greater than NOTMUCH_STATUS_LAST_STATUS. (The idea here is
 * that the caller has previously handled any expected
 * notmuch_private_status_t values.)
 */
#define COERCE_STATUS(private_status, format, ...)			\
    ((private_status >= (notmuch_private_status_t) NOTMUCH_STATUS_LAST_STATUS)\
     ?									\
     (notmuch_status_t) _internal_error (format " (%s).\n",		\
                                         ##__VA_ARGS__,			\
                                         __location__)			\
     :									\
     (notmuch_status_t) private_status)

/* database.cc */

/* Lookup a prefix value by name.
 *
 * XXX: This should really be static inside of message.cc, and we can
 * do that once we convert database.cc to use the
 * _notmuch_message_add/remove_term functions. */
const char *
_find_prefix (const char *name);

notmuch_status_t
_notmuch_database_ensure_writable (notmuch_database_t *notmuch);

const char *
_notmuch_database_relative_path (notmuch_database_t *notmuch,
				 const char *path);

notmuch_status_t
_notmuch_database_split_path (void *ctx,
			      const char *path,
			      const char **directory,
			      const char **basename);

const char *
_notmuch_database_get_directory_db_path (const char *path);

notmuch_private_status_t
_notmuch_database_find_unique_doc_id (notmuch_database_t *notmuch,
				      const char *prefix_name,
				      const char *value,
				      unsigned int *doc_id);

notmuch_status_t
_notmuch_database_find_directory_id (notmuch_database_t *database,
				     const char *path,
				     unsigned int *directory_id);

const char *
_notmuch_database_get_directory_path (void *ctx,
				      notmuch_database_t *notmuch,
				      unsigned int doc_id);

notmuch_status_t
_notmuch_database_filename_to_direntry (void *ctx,
					notmuch_database_t *notmuch,
					const char *filename,
					char **direntry);

/* directory.cc */

notmuch_directory_t *
_notmuch_directory_create (notmuch_database_t *notmuch,
			   const char *path,
			   notmuch_status_t *status_ret);

unsigned int
_notmuch_directory_get_document_id (notmuch_directory_t *directory);

/* thread.cc */

notmuch_thread_t *
_notmuch_thread_create (void *ctx,
			notmuch_database_t *notmuch,
			const char *thread_id,
			const char *query_string);

/* message.cc */

notmuch_message_t *
_notmuch_message_create (const void *talloc_owner,
			 notmuch_database_t *notmuch,
			 unsigned int doc_id,
			 notmuch_private_status_t *status);

notmuch_message_t *
_notmuch_message_create_for_message_id (notmuch_database_t *notmuch,
					const char *message_id,
					notmuch_private_status_t *status);

const char *
_notmuch_message_get_in_reply_to (notmuch_message_t *message);

notmuch_private_status_t
_notmuch_message_add_term (notmuch_message_t *message,
			   const char *prefix_name,
			   const char *value);

notmuch_private_status_t
_notmuch_message_remove_term (notmuch_message_t *message,
			      const char *prefix_name,
			      const char *value);

notmuch_private_status_t
_notmuch_message_gen_terms (notmuch_message_t *message,
			    const char *prefix_name,
			    const char *text);

void
_notmuch_message_upgrade_filename_storage (notmuch_message_t *message);

notmuch_status_t
_notmuch_message_add_filename (notmuch_message_t *message,
			       const char *filename);

void
_notmuch_message_ensure_thread_id (notmuch_message_t *message);

void
_notmuch_message_set_date (notmuch_message_t *message,
			   const char *date);

void
_notmuch_message_sync (notmuch_message_t *message);

void
_notmuch_message_close (notmuch_message_t *message);

/* index.cc */

notmuch_status_t
_notmuch_message_index_file (notmuch_message_t *message,
			     const char *filename);

/* message-file.c */

/* XXX: I haven't decided yet whether these will actually get exported
 * into the public interface in notmuch.h
 */

typedef struct _notmuch_message_file notmuch_message_file_t;

/* Open a file containing a single email message.
 *
 * The caller should call notmuch_message_close when done with this.
 *
 * Returns NULL if any error occurs.
 */
notmuch_message_file_t *
notmuch_message_file_open (const char *filename);

/* Like notmuch_message_file_open but with 'ctx' as the talloc owner. */
notmuch_message_file_t *
_notmuch_message_file_open_ctx (void *ctx, const char *filename);

/* Close a notmuch message previously opened with notmuch_message_open. */
void
notmuch_message_file_close (notmuch_message_file_t *message);

/* Restrict 'message' to only save the named headers.
 *
 * When the caller is only interested in a short list of headers,
 * known in advance, calling this function can avoid wasted time and
 * memory parsing/saving header values that will never be needed.
 *
 * The variable arguments should be a list of const char * with a
 * final '(const char *) NULL' to terminate the list.
 *
 * If this function is called, it must be called before any calls to
 * notmuch_message_get_header for this message.
 *
 * After calling this function, if notmuch_message_get_header is
 * called with a header name not in this list, then NULL will be
 * returned even if that header exists in the actual message.
 */
void
notmuch_message_file_restrict_headers (notmuch_message_file_t *message, ...);

/* Identical to notmuch_message_restrict_headers but accepting a va_list. */
void
notmuch_message_file_restrict_headersv (notmuch_message_file_t *message,
					va_list va_headers);

/* Get the value of the specified header from the message.
 *
 * The header name is case insensitive.
 *
 * The returned value is owned by the notmuch message and is valid
 * only until the message is closed. The caller should copy it if
 * needing to modify the value or to hold onto it for longer.
 *
 * Returns NULL if the message does not contain a header line matching
 * 'header'.
 */
const char *
notmuch_message_file_get_header (notmuch_message_file_t *message,
				 const char *header);

/* messages.c */

typedef struct _notmuch_message_node {
    notmuch_message_t *message;
    struct _notmuch_message_node *next;
} notmuch_message_node_t;

typedef struct _notmuch_message_list {
    notmuch_message_node_t *head;
    notmuch_message_node_t **tail;
} notmuch_message_list_t;

/* There's a rumor that there's an alternate struct _notmuch_messages
 * somewhere with some nasty C++ objects in it. We'll try to maintain
 * ignorance of that here. (See notmuch_mset_messages_t in query.cc)
 */
struct _notmuch_messages {
    notmuch_bool_t is_of_list_type;
    notmuch_message_node_t *iterator;
};

notmuch_message_list_t *
_notmuch_message_list_create (const void *ctx);

void
_notmuch_message_list_append (notmuch_message_list_t *list,
			      notmuch_message_node_t *node);

void
_notmuch_message_list_add_message (notmuch_message_list_t *list,
				   notmuch_message_t *message);

notmuch_messages_t *
_notmuch_messages_create (notmuch_message_list_t *list);

/* query.cc */

notmuch_bool_t
_notmuch_mset_messages_has_more (notmuch_messages_t *messages);

notmuch_message_t *
_notmuch_mset_messages_get (notmuch_messages_t *messages);

void
_notmuch_mset_messages_advance (notmuch_messages_t *messages);

/* message.cc */

void
_notmuch_message_add_reply (notmuch_message_t *message,
			    notmuch_message_node_t *reply);

/* sha1.c */

char *
notmuch_sha1_of_string (const char *str);

char *
notmuch_sha1_of_file (const char *filename);

/* tags.c */

notmuch_tags_t *
_notmuch_tags_create (void *ctx);

void
_notmuch_tags_add_tag (notmuch_tags_t *tags, const char *tag);

void
_notmuch_tags_prepare_iterator (notmuch_tags_t *tags);

NOTMUCH_END_DECLS

#endif
